{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}


module Unison.Codebase.CommandLine.Main where

import qualified Control.Concurrent.Async                 as Async
import           Control.Concurrent.STM                   (atomically)
import           Control.Exception                        (finally)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Control.Monad.Trans                      (lift)
import           Data.IORef
import           Data.Map                                 (Map)
import qualified Data.Map                                 as Map
import           Data.Maybe                               (fromMaybe,
                                                           listToMaybe)
import           Data.String                              (fromString)
import qualified Data.Text                                as Text
import           Prelude                                  hiding (readFile,
                                                           writeFile)
import qualified System.Console.Haskeline                 as Line
import           Unison.Codebase                          (Codebase)
import qualified Unison.Codebase                          as Codebase
import           Unison.Codebase.Branch                   (Branch)
import           Unison.Codebase.CommandLine
import           Unison.Codebase.CommandLine.InputPattern (ArgumentType (suggestions),
                                                           InputPattern (args, patternName, aliases))
import           Unison.Codebase.CommandLine.InputPatterns (validInputs)
import           Unison.Codebase.Editor                   (BranchName,
                                                           Input (..))
import qualified Unison.Codebase.Editor                   as E
import qualified Unison.Codebase.Editor.Actions           as Actions
import           Unison.Codebase.Runtime                  (Runtime)
import qualified Unison.Codebase.Runtime                  as Runtime
import           Unison.Parser                            (Ann)
import qualified Unison.Util.Pretty                       as P
import qualified Unison.Util.TQueue                       as Q
import           Unison.Var                               (Var)

getUserInput
  :: (MonadIO m, Line.MonadException m)
  => Map String InputPattern
  -> Codebase m v a
  -> Branch
  -> BranchName
  -> m Input
getUserInput patterns codebase branch branchName = Line.runInputT settings $ do
  line <- Line.getInputLine $
    P.toANSI 80 (P.green (P.text branchName <> fromString prompt))
  case line of
    Nothing -> pure QuitI
    Just l  -> case parseInput patterns $ words l of
      Left msg -> lift $ do
        liftIO $ putPrettyLn msg
        getUserInput patterns codebase branch branchName
      Right i -> pure i
 where
  settings    = Line.Settings tabComplete (Just ".unisonHistory") True
  tabComplete = Line.completeWordWithPrev Nothing " " $ \prev word ->
    -- User hasn't finished a command name, complete from command names
    if null prev
      then pure $ autoComplete word (Map.keys patterns)
    -- User has finished a command name; use completions for that command
      else case words $ reverse prev of
        h : t -> fromMaybe (pure []) $ do
          p            <- Map.lookup h patterns
          (_, argType) <- listToMaybe $ drop (length t) (args p)
          pure $ suggestions argType word codebase branch
        _ -> pure []

main
  :: forall v
   . Var v
  => FilePath
  -> BranchName
  -> Maybe FilePath
  -> IO (Runtime v)
  -> Codebase IO v Ann
  -> IO ()
main dir currentBranchName _initialFile startRuntime codebase = do
  currentBranch <- Codebase.getBranch codebase currentBranchName
  eventQueue    <- Q.newIO
  currentBranch <- case currentBranch of
    Nothing ->
      Codebase.mergeBranch codebase currentBranchName Codebase.builtinBranch
        <* (  putStrLn
           $  "☝️  I found no branch named '"
           <> Text.unpack currentBranchName
           <> "' so I've created it for you."
           )
    Just b -> pure b
  do
    runtime                  <- startRuntime
    branchRef                <- newIORef (currentBranch, currentBranchName)
    cancelFileSystemWatch    <- watchFileSystem eventQueue dir
    cancelWatchBranchUpdates <- watchBranchUpdates (readIORef branchRef) eventQueue codebase
    let patternMap =
          Map.fromList
            $   validInputs
            >>= (\p -> [(patternName p, p)] ++ ((, p) <$> aliases p))
        getInput = do
          (branch, branchName) <- readIORef branchRef
          getUserInput patternMap codebase branch branchName
    let awaitInput = do
          -- Race the user input and file watch.
          Async.race (atomically $ Q.peek eventQueue) getInput >>= \case
            Left _ -> Left <$> atomically (Q.dequeue eventQueue)
            x      -> pure x
        cleanup = do
          Runtime.terminate runtime
          cancelFileSystemWatch
          cancelWatchBranchUpdates
    (`finally` cleanup)
      $ E.commandLine awaitInput
                           runtime
                           (\b bn -> writeIORef branchRef (b, bn))
                           (notifyUser dir)
                           codebase
      $ Actions.startLoop currentBranch currentBranchName
