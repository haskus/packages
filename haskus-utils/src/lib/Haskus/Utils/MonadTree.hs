{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Monadic tree with cache
module Haskus.Utils.MonadTree
   ( MonadTree (..)
   , showMonadTree
   , showMonadTrees
   , updateMonadTree
   )
where

import Haskus.Utils.Monad
import Haskus.Utils.MonadVar

-- | Monad dependent rose tree
--
-- Both the tree values and the tree nodes may be monadically dependent.
data MonadTree m a
   = StaticBranch a [MonadTree m a]                               -- ^ Static node
   | forall s. Eq s => MonadBranch (MonadVar m s [MonadTree m a]) -- ^ Monad dependent nodes

deriving instance Functor (MonadTree m)

-- | Pretty-show an MonadTree
showMonadTree :: Show a => MonadTree m a -> String
showMonadTree = go 0
   where
      indent n c      = replicate (2*n) ' ' <> c
      showNode n a ts = indent n "- " <> show a <> "\n" <> concatMap (go (n+1)) ts
      go n = \case
         StaticBranch a ts                     -> showNode n a ts
         MonadBranch (MonadVar {})             -> indent n "{...}\n"
         MonadBranch (CachedMonadVar ts _ _ _) -> indent n "{\n" <> concatMap (go (n+1)) ts <> indent n "}\n"

-- | Pretty-show some MonadTrees
showMonadTrees :: Show a => [MonadTree m a] -> String
showMonadTrees = concatMap showMonadTree

data Diff a
   = Same a    -- ^ Has not changed
   | Updated a -- ^ May have changed
   deriving (Show,Eq,Ord)

-- | Update a MonadTree recursively. Reuse cached values when possible
--
-- Left return: nothing has changed in the tree
-- Right return: the tree has changed
updateMonadTree :: Monad m => MonadTree m a -> m (MonadTree m a)
updateMonadTree = go
   where
      go node = case node of
            StaticBranch a ts -> StaticBranch a <$> forM ts go
            MonadBranch dv -> case dv of
               MonadVar io f -> do
                  ~(CachedMonadVar ts s _ _) <- updateMonadVarForce dv
                  ts' <- forM ts go
                  pure (MonadBranch (CachedMonadVar ts' s io f))
               CachedMonadVar ts s io f -> do
                  mcdv <- updateMonadVarMaybe dv
                  case mcdv of
                     Nothing -> do
                        ts' <- forM ts go
                        pure (MonadBranch (CachedMonadVar ts' s io f))
                     Just ~(CachedMonadVar ts' s' _ _) -> do
                        ts'' <- forM ts' go
                        pure (MonadBranch (CachedMonadVar ts'' s' io f))
