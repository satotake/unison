{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.SqliteCodebase.Branch.Dependencies where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid.Generic (GenericMonoid (..), GenericSemigroup (..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Unison.Codebase.Branch (Branch (Branch), BranchSnapshot, EditHash)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.Patch (Patch)
import Unison.NameSegment (NameSegment)
import Unison.Reference (Reference, pattern Derived)
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Star3 as Star3
import Unison.Hash (Hash)

type Branches m = [(Branch.Hash, m (Branch m))]

data Dependencies = Dependencies
  { patches :: Set EditHash
  , terms :: Set Hash
  , decls :: Set Hash
  }
  deriving Show
  deriving Generic
  deriving Semigroup via GenericSemigroup Dependencies
  deriving Monoid via GenericMonoid Dependencies

data Dependencies' = Dependencies'
  { patches' :: [EditHash]
  , terms' :: [Hash]
  , decls' :: [Hash]
  }
  deriving (Eq, Show)
  deriving Generic
  deriving Semigroup via GenericSemigroup Dependencies'
  deriving Monoid via GenericMonoid Dependencies'


to' :: Dependencies -> Dependencies'
to' Dependencies{..} = Dependencies' (toList patches) (toList terms) (toList decls)

fromBranch :: Applicative m => Branch m -> (Branches m, Dependencies)
fromBranch (Branch c) = case c of
  Causal.One _hh e         -> fromBranchSnapshot e
  Causal.Cons _hh e (h, m) -> fromBranchSnapshot e <> fromTails (Map.singleton h m)
  Causal.Merge _hh e tails -> fromBranchSnapshot e <> fromTails tails
  where
  fromTails m = ([(h, Branch <$> mc) | (h, mc) <- Map.toList m], mempty)

fromBranchSnapshot :: Applicative m => BranchSnapshot m -> (Branches m, Dependencies)
fromBranchSnapshot b =
  ( fromChildren (Branch.children b)
  , fromTermsStar (Branch.terms b)
    <> fromTypesStar (Branch.types b)
    <> fromEdits (Branch.edits b) )
  where
  fromChildren :: Applicative m => Map NameSegment (Branch m) -> Branches m
  fromChildren m = [ (Branch.headHash b, pure b) | b <- toList m ]
  references :: Branch.Star r NameSegment -> [r]
  references = toList . R.dom . Star3.d1
  mdValues :: Branch.Star r NameSegment -> [Reference]
  mdValues = fmap snd . toList . R.ran . Star3.d3
  fromTermsStar :: Branch.Star Referent NameSegment -> Dependencies
  fromTermsStar s = Dependencies mempty terms decls where
    terms = Set.fromList $
      [ h | Referent.Ref (Derived h _ _) <- references s] ++
      [ h | (Derived h _ _) <- mdValues s]
    decls = Set.fromList $
      [ h | Referent.Con (Derived h _i _n) _ _ <- references s ]
  fromTypesStar :: Branch.Star Reference NameSegment -> Dependencies
  fromTypesStar s = Dependencies mempty terms decls where
    terms = Set.fromList [ h | (Derived h _ _) <- mdValues s ]
    decls = Set.fromList [ h | (Derived h _ _) <- references s ]
  fromEdits :: Map NameSegment (EditHash, m Patch) -> Dependencies
  fromEdits m = Dependencies (Set.fromList . fmap fst $ toList m) mempty mempty
