{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Data depending on IO
module Haskus.Utils.IOVar
   ( IOVar (..)
   , updateIOVarForce
   , updateIOVarMaybe
   , updateIOVar
   -- * IOTree
   , IOTree (..)
   , Node (..)
   , showIOTree
   , updateIOTree
   )
where

import Haskus.Utils.Monad
import Haskus.Utils.Maybe
import Haskus.Utils.Flow
import Haskus.Utils.Either

-- | A value that can be read with IO. The last read can be cached in it too.
--
-- We store both the read value (type s) and a pure modifier function (s -> a).
-- By doing this we can easily compare a read value to the cached one without
-- performing extra computations. The functor instance compose with the modifier
-- function.
--
-- The supposedly costly modifier function is applied lazily
data IOVar s a
   = IOVar !(IO s) (s -> a)            -- ^ IO accessor + modifier function
   | CachedIOVar a !s !(IO s) (s -> a) -- ^ Additional cached transformed and read values.
   deriving (Functor)

-- | Check if the IOVar cache needs to be updated.
--
-- Invariably produce an IOVar with cached values or Nothing if the old one
-- hasn't changed.
updateIOVarMaybe :: (MonadIO m, Eq s) => IOVar s a -> m (Maybe (IOVar s a))
updateIOVarMaybe dv@(IOVar {}) = Just <|| updateIOVarForce dv
updateIOVarMaybe (CachedIOVar _ s io f) = do
   s' <- liftIO io
   if s == s'
      then pure Nothing
      else pure <| Just <| CachedIOVar (f s') s' io f

-- | Check if the IOVar cache needs to be updated. Return the updated IOVar.
--
-- Invariably produce an IOVar with cached values.
updateIOVar :: (MonadIO m, Eq s) => IOVar s a -> m (IOVar s a)
updateIOVar dv = fromMaybe dv <|| updateIOVarMaybe dv

-- | Update an IOVar without comparing to the cache even if it is available.
--
-- Invariably produce an IOVar with cached values.
updateIOVarForce :: (MonadIO m, Eq s) => IOVar s a -> m (IOVar s a)
updateIOVarForce (CachedIOVar _ _ io f) = do
   s <- liftIO io
   pure (CachedIOVar (f s) s io f)
updateIOVarForce (IOVar io f) = do
   s <- liftIO io
   pure (CachedIOVar (f s) s io f)


-----------------------------------------
-- IO Tree
-----------------------------------------

-- | IO dependent rose tree
--
-- Both the tree values and the tree nodes may be IO dependent.
data IOTree a
   = StaticBranch (Node a)                         -- ^ Static node
   | forall s. Eq s => IOBranch (IOVar s (Node a)) -- ^ IO dependent node

deriving instance Functor IOTree

data Node a = Node (Maybe a) [IOTree a]
   deriving (Functor)

-- | Pretty-show an IO tree
showIOTree :: Show a => IOTree a -> String
showIOTree = go 0
   where
      indent b n = replicate (2*n) ' ' ++ if b then "+ " else "- "
      showNode b n = \case
         Node Nothing ts  -> indent b n <> "()\n" <> concatMap (go (n+1)) ts
         Node (Just a) ts -> indent b n <> show a <> "\n" <> concatMap (go (n+1)) ts
      go n = \case
         StaticBranch nd                 -> showNode False n nd
         IOBranch (IOVar {})             -> indent True n <> "...\n"
         IOBranch (CachedIOVar nd _ _ _) -> showNode False n nd

-- | Update a IOTree recursively. Reuse cached values when possible
--
-- Left return: nothing has changed in the tree
-- Right return: the tree has changed
updateIOTree :: MonadIO m => IOTree a -> m (Either (IOTree a) (IOTree a))
updateIOTree node = case node of
   StaticBranch nd -> updateNode nd >>= \case
                     Left _    -> pure (Left node)
                     Right nd' -> pure (Right (StaticBranch nd'))
   IOBranch dv -> case dv of
      IOVar io f -> do
         ~cdv@(CachedIOVar a s _ _) <- updateIOVarForce dv
         updateNode a >>= \case
            Left _   -> pure (Right (IOBranch cdv))
            Right a' -> pure (Right (IOBranch (CachedIOVar a' s io f)))
      CachedIOVar a s io f -> do
         mcdv <- updateIOVarMaybe dv
         case mcdv of
            Nothing -> do
               updateNode a >>= \case
                  Left  _  -> pure (Left node)
                  Right a' -> pure (Right (IOBranch (CachedIOVar a' s io f)))
            Just ~cdv@(CachedIOVar a' s' _ _) -> do
               updateNode a' >>= \case
                  Left _    -> pure (Right (IOBranch cdv))
                  Right a'' -> pure (Right (IOBranch (CachedIOVar a'' s' io f)))
         
   where
      updateNode nd@(Node a ts) = do
         ts' <- forM ts updateIOTree
         if null ts' || all isLeft ts'
            then pure (Left  nd)
            else pure (Right (Node a (fmap fromEither ts')))
