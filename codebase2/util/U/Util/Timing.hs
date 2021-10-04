{-# LANGUAGE BangPatterns #-}

module U.Util.Timing where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception (evaluate)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock (picosecondsToDiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (diffAbsoluteTime)
import Control.Monad (when)
import GHC.Clock (getMonotonicTime)
import System.CPUTime (getCPUTime)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC)
import UnliftIO (MonadIO, liftIO)

enabled :: Bool
enabled = True

time :: MonadIO m => String -> m a -> m a
time _ ma | not enabled = ma
time label ma = do
  systemStart <- liftIO getSystemTime
  cpuPicoStart <- liftIO getCPUTime
  liftIO $ putStrLn $ "Timing " ++ label ++ "..."
  a <- ma
  cpuPicoEnd <- liftIO getCPUTime
  systemEnd <- liftIO getSystemTime
  let systemDiff = diffAbsoluteTime (systemToTAITime systemEnd) (systemToTAITime systemStart)
  let cpuDiff = picosecondsToDiffTime (cpuPicoEnd - cpuPicoStart)
  liftIO $ putStrLn $ "Finished " ++ label ++ " in " ++ show cpuDiff ++ " (cpu), " ++ show systemDiff ++ " (system)"
  pure a

unsafeTime :: Monad m => String -> m a -> m a
unsafeTime _ ma | not enabled = ma
unsafeTime label ma = do
  let !systemStart = unsafePerformIO getSystemTime
      !cpuPicoStart = unsafePerformIO getCPUTime
      !_ = unsafePerformIO $ putStrLn $ "Timing " ++ label ++ "..."
  a <- ma
  let !cpuPicoEnd = unsafePerformIO getCPUTime
      !systemEnd = unsafePerformIO getSystemTime
  let systemDiff = diffAbsoluteTime (systemToTAITime systemEnd) (systemToTAITime systemStart)
  let cpuDiff = picosecondsToDiffTime (cpuPicoEnd - cpuPicoStart)
  let !_ = unsafePerformIO $ putStrLn $ "Finished " ++ label ++ " in " ++ show cpuDiff ++ " (cpu), " ++ show systemDiff ++ " (system)"
  pure a

contextVar :: MVar (Map ThreadId [String])
contextVar =
  unsafePerformIO (newMVar Map.empty)
{-# NOINLINE contextVar #-}

pushContext :: String -> IO ()
pushContext message = do
  threadId <- myThreadId
  modifyMVar_ contextVar (pure . Map.alter f threadId)
  where
    f :: Maybe [String] -> Maybe [String]
    f context =
      Just (message : fromMaybe [] context)

popContext :: IO [String]
popContext = do
  threadId <- myThreadId
  modifyMVar contextVar \context ->
    case Map.alterF f threadId context of
      (x, y) -> pure (y, x)
  where
    f :: Maybe [String] -> ([String], Maybe [String])
    f context =
      case fromMaybe [] context of
        x : y : ys -> (x : y : ys, Just (y : ys))
        xs -> (xs, Nothing)

getContext :: IO [String]
getContext = do
  threadId <- myThreadId
  context <- readMVar contextVar
  pure (Map.findWithDefault [] threadId context)

collectGarbage :: IO ()
collectGarbage =
  when False do
    g0 <- getMonotonicTime
    performGC -- to get a more accurate timing
    g1 <- getMonotonicTime
    putTime ["GC"] g0 g1

timeNf :: NFData a => String -> a -> a
timeNf message val =
  unsafePerformIO (timeNfIO message val)

timeNfIO :: NFData a => String -> a -> IO a
timeNfIO message val = do
  timeIO_ message (rnf val)
  pure val

timeM :: Monad m => String -> m a -> m a
timeM message action = do
  let !t0 =
        unsafePerformIO do
          pushContext message
          collectGarbage
          getMonotonicTime
  val <- action
  let !_ =
        unsafePerformIO do
          t1 <- getMonotonicTime
          context <- popContext
          putTime context t0 t1
  pure val

timeWhnf :: String -> a -> a
timeWhnf message val =
  unsafePerformIO (timeWhnfIO message val)

timeWhnfIO :: String -> a -> IO a
timeWhnfIO message val = do
  timeIO_ message val
  pure val

timeIO_ :: String -> a -> IO ()
timeIO_ message val = do
  pushContext message
  collectGarbage
  t0 <- getMonotonicTime
  _ <- evaluate val
  t1 <- getMonotonicTime
  context <- popContext
  putTime context t0 t1

putTime :: [String] -> Double -> Double -> IO ()
putTime context t0 t1 =
  putStrLn (concat (replicate (length context - 1) "  ") ++ s ++ "ms " ++ List.intercalate " ⫻ " (reverse context))
  where
    s = show (round ((t1 - t0) * 1000) :: Int)
