{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Monadic tree with cache
module Haskus.Utils.MonadTree
   ( MonadTree (..)
   , Node (..)
   , showMonadTree
   , showMonadTrees
   , updateMonadTree
   )
where

import Haskus.Utils.Monad
import Haskus.Utils.Either
import Haskus.Utils.MonadVar

-- | Monad dependent rose tree
--
-- Both the tree values and the tree nodes may be monadically dependent.
data MonadTree m a
   = StaticBranch (Node m a)                                 -- ^ Static node
   | forall s. Eq s => MonadBranch (MonadVar m s (Node m a)) -- ^ Monad dependent node

deriving instance Functor (MonadTree m)

data Node m a = Node (Maybe a) [MonadTree m a]
   deriving (Functor)

-- | Pretty-show an MonadTree
showMonadTree :: Show a => MonadTree m a -> String
showMonadTree = go 0
   where
      indent isIO b n = replicate (2*n) ' ' ++ if b then "+ " else (if isIO then "* " else "- ")
      showNode isIO b n = \case
         Node Nothing ts  -> indent isIO b n <> "()\n" <> concatMap (go (n+1)) ts
         Node (Just a) ts -> indent isIO b n <> show a <> "\n" <> concatMap (go (n+1)) ts
      go n = \case
         StaticBranch nd                 -> showNode False False n nd
         MonadBranch (MonadVar {})             -> indent True True n <> "...\n"
         MonadBranch (CachedMonadVar nd _ _ _) -> showNode True False n nd

-- | Pretty-show some MonadTrees
showMonadTrees :: Show a => [MonadTree m a] -> String
showMonadTrees = concatMap showMonadTree

-- | Update a MonadTree recursively. Reuse cached values when possible
--
-- Left return: nothing has changed in the tree
-- Right return: the tree has changed
updateMonadTree :: Monad m => MonadTree m a -> m (Either (MonadTree m a) (MonadTree m a))
updateMonadTree node = case node of
   StaticBranch nd -> updateNode nd >>= \case
                     Left _    -> pure (Left node)
                     Right nd' -> pure (Right (StaticBranch nd'))
   MonadBranch dv -> case dv of
      MonadVar io f -> do
         ~cdv@(CachedMonadVar a s _ _) <- updateMonadVarForce dv
         updateNode a >>= \case
            Left _   -> pure (Right (MonadBranch cdv))
            Right a' -> pure (Right (MonadBranch (CachedMonadVar a' s io f)))
      CachedMonadVar a s io f -> do
         mcdv <- updateMonadVarMaybe dv
         case mcdv of
            Nothing -> do
               updateNode a >>= \case
                  Left  _  -> pure (Left node)
                  Right a' -> pure (Right (MonadBranch (CachedMonadVar a' s io f)))
            Just ~cdv@(CachedMonadVar a' s' _ _) -> do
               updateNode a' >>= \case
                  Left _    -> pure (Right (MonadBranch cdv))
                  Right a'' -> pure (Right (MonadBranch (CachedMonadVar a'' s' io f)))
         
   where
      updateNode nd@(Node a ts) = do
         ts' <- forM ts updateMonadTree
         if null ts' || all isLeft ts'
            then pure (Left  nd)
            else pure (Right (Node a (fmap fromEither ts')))
