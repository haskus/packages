{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BlockArguments #-}

-- | Monadic tree with cache
module Haskus.Utils.MonadStream
   ( MonadStream (..)
   , MonadTree
   , MonadList
   , showMonadStream
   , showMonadStreams
   , updateMonadStream
   , updateMonadStreamMaybe
   , updateMonadStreamsMaybe
   , updateMonadStreams
   )
where

import Haskus.Utils.Monad
import Haskus.Utils.MonadVar
import Haskus.Utils.Flow
import Haskus.Utils.Maybe

-- | Monadic stream with cache
--
-- Both the structure and the values may be monadically dependent. The last
-- monadic values read can be stored in a cache.
data MonadStream m n a
   = PureStream a (n (MonadStream m n a))                                   -- ^ Pure stream
   | forall s. Eq s => MonadStream (MonadVarNE m s (n (MonadStream m n a))) -- ^ Monadic stream

deriving instance Functor n => Functor (MonadStream m n)

-- | Monadic rose tree
type MonadTree m a = MonadStream m [] a

-- | Monadic list
type MonadList m a = MonadStream m Maybe a

-- | Pretty-show an MonadStream
showMonadStream ::
   ( Foldable n
   , Show a
   , Eq (n (MonadStream m n a))
   , Monoid (n (MonadStream m n a))
   ) => MonadStream m n a -> String
showMonadStream = go 0
   where
      indent n c      = replicate (2*n) ' ' <> c
      showNode n a ts = indent n "- " <> show a <> "\n" <> concatMap (go (n+1)) ts
      go n = \case
         PureStream a ts                 -> showNode n a ts
         MonadStream (MonadVarNE ts _ _ _)
            | ts == mempty -> indent n "{}\n"
            | otherwise    -> indent n "{\n" <> concatMap (go (n+1)) ts <> indent n "}\n"

-- | Pretty-show some MonadStreams
showMonadStreams ::
   ( Foldable n
   , Show a
   , Eq (n (MonadStream m n a))
   , Monoid (n (MonadStream m n a))
   ) => n (MonadStream m n a) -> String
showMonadStreams = concatMap showMonadStream

-- | Update a MonadStream recursively. Reuse cached values when possible
updateMonadStream ::
   ( Monad m
   , Traversable n
   ) => MonadStream m n a -> m (MonadStream m n a)
updateMonadStream t = updateMonadStreamMaybe t
   ||> fromMaybe t

-- | Update a MonadStream recursively. Reuse cached values when possible
updateMonadStreamMaybe ::
   ( Monad m
   , Traversable n
   ) => MonadStream m n a -> m (Maybe (MonadStream m n a))
updateMonadStreamMaybe = go False
   where
      go False (PureStream a ts) = PureStream a <||| updateMonadStreamsMaybe ts
      go True  (PureStream a ts) = Just <|| PureStream a <|| updateMonadStreams ts
      go True (MonadStream dv) = do
            (MonadVarNE ts' ms' io f) <- updateMonadVarNE dv
            ts'' <- updateMonadStreams ts'
            pure (Just (MonadStream (MonadVarNE ts'' ms' io f)))
      go False (MonadStream dv@(MonadVarNE ts ms io f)) = do
            mcdv <- updateMonadVarNEMaybe dv
            case mcdv of
               Nothing -> updateMonadStreamsMaybe ts
                          |||> (\ts' -> MonadStream (MonadVarNE ts' ms io f))
               Just (MonadVarNE ts' ms' _ _) -> do
                  ts'' <- updateMonadStreams ts'
                  pure (Just (MonadStream (MonadVarNE ts'' ms' io f)))

-- | Update a MonadStream forest recursively. Reuse cached values when possible
updateMonadStreamsMaybe ::
   ( Monad m
   , Traversable n
   ) => n (MonadStream m n a) -> m (Maybe (n (MonadStream m n a)))
updateMonadStreamsMaybe ns = do
   ns' <- forM ns \n -> do
      mu <- updateMonadStreamMaybe n
      pure (n,mu)
   if all (isNothing . snd) ns'
      then pure Nothing
      else pure (Just (fmap fst ns'))

-- | Update a MonadStream forest recursively
updateMonadStreams ::
   ( Monad m
   , Traversable n
   ) => n (MonadStream m n a) -> m (n (MonadStream m n a))
updateMonadStreams ns = updateMonadStreamsMaybe ns
   ||> fromMaybe ns
