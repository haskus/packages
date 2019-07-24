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
   , updateMonadTreeMaybe
   , updateMonadTreesMaybe
   , updateMonadTrees
   )
where

import Haskus.Utils.Monad
import Haskus.Utils.MonadVar
import Haskus.Utils.Flow
import Haskus.Utils.Maybe

-- | Monad dependent rose tree
--
-- Both the tree values and the tree nodes may be monadically dependent.
data MonadTree m a
   = StaticBranch a [MonadTree m a]                                 -- ^ Static node
   | forall s. Eq s => MonadBranch (MonadVarNE m s [MonadTree m a]) -- ^ Monad dependent nodes

deriving instance Functor (MonadTree m)

-- | Pretty-show an MonadTree
showMonadTree :: Show a => MonadTree m a -> String
showMonadTree = go 0
   where
      indent n c      = replicate (2*n) ' ' <> c
      showNode n a ts = indent n "- " <> show a <> "\n" <> concatMap (go (n+1)) ts
      go n = \case
         StaticBranch a ts                 -> showNode n a ts
         MonadBranch (MonadVarNE ts _ _ _) -> indent n "{\n" <> concatMap (go (n+1)) ts <> indent n "}\n"

-- | Pretty-show some MonadTrees
showMonadTrees :: Show a => [MonadTree m a] -> String
showMonadTrees = concatMap showMonadTree

-- | Update a MonadTree recursively. Reuse cached values when possible
updateMonadTree :: Monad m => MonadTree m a -> m (MonadTree m a)
updateMonadTree t = updateMonadTreeMaybe t
   ||> fromMaybe t

-- | Update a MonadTree recursively. Reuse cached values when possible
updateMonadTreeMaybe :: Monad m => MonadTree m a -> m (Maybe (MonadTree m a))
updateMonadTreeMaybe = go False
   where
      go False (StaticBranch a ts) = StaticBranch a <||| updateMonadTreesMaybe ts
      go True  (StaticBranch a ts) = Just <|| StaticBranch a <|| updateMonadTrees ts
      go True (MonadBranch dv) = do
            (MonadVarNE ts' ms' io f) <- updateMonadVarNE dv
            ts'' <- updateMonadTrees ts'
            pure (Just (MonadBranch (MonadVarNE ts'' ms' io f)))
      go False (MonadBranch dv@(MonadVarNE ts ms io f)) = do
            mcdv <- updateMonadVarNEMaybe dv
            case mcdv of
               Nothing -> updateMonadTreesMaybe ts
                          |||> (\ts' -> MonadBranch (MonadVarNE ts' ms io f))
               Just (MonadVarNE ts' ms' _ _) -> do
                  ts'' <- updateMonadTrees ts'
                  pure (Just (MonadBranch (MonadVarNE ts'' ms' io f)))

-- | Update a MonadTree forest recursively. Reuse cached values when possible
updateMonadTreesMaybe :: Monad m => [MonadTree m a] -> m (Maybe [MonadTree m a])
updateMonadTreesMaybe ns = do
   ns' <- forM ns updateMonadTreeMaybe
   if all isNothing ns'
      then pure Nothing
      else pure (Just (zipWith fromMaybe ns ns'))

-- | Update a MonadTree forest recursively
updateMonadTrees :: Monad m => [MonadTree m a] -> m [MonadTree m a]
updateMonadTrees ns = updateMonadTreesMaybe ns
   ||> fromMaybe ns
