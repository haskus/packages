{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Monadic variable with cache
module Haskus.Utils.MonadVar
   ( MonadVar (..)
   , updateMonadVarForce
   , updateMonadVarMaybe
   , updateMonadVar
   -- * Non-empty
   , MonadVarNE (..)
   , updateMonadVarNEMaybe
   , updateMonadVarNE
   )
where

import Haskus.Utils.Flow
import Haskus.Utils.Maybe

-- | A value that can be read with IO. The last read can be cached in it too.
--
-- We store both the read value (type s) and a pure modifier function (s -> a).
-- By doing this we can easily compare a read value to the cached one without
-- performing extra computations. The functor instance compose with the modifier
-- function.
--
-- The supposedly costly modifier function is applied lazily
data MonadVar m s a
   = MonadVar !(m s) (s -> a)            -- ^ IO accessor + modifier function
   | CachedMonadVar a !s !(m s) (s -> a) -- ^ Additional cached transformed and read values.
   deriving (Functor)

-- | Check if the MonadVar cache needs to be updated.
--
-- Invariably produce an MonadVar with cached values or Nothing if the old one
-- hasn't changed.
updateMonadVarMaybe :: (Monad m, Eq s) => MonadVar m s a -> m (Maybe (MonadVar m s a))
updateMonadVarMaybe dv@(MonadVar {}) = Just <|| updateMonadVarForce dv
updateMonadVarMaybe (CachedMonadVar _ s io f) = do
   s' <- io
   if s == s'
      then pure Nothing
      else pure <| Just <| CachedMonadVar (f s') s' io f

-- | Check if the MonadVar cache needs to be updated. Return the updated MonadVar.
--
-- Invariably produce an MonadVar with cached values.
updateMonadVar :: (Monad m, Eq s) => MonadVar m s a -> m (MonadVar m s a)
updateMonadVar dv = fromMaybe dv <|| updateMonadVarMaybe dv

-- | Update an MonadVar without comparing to the cache even if it is available.
--
-- Invariably produce an MonadVar with cached values.
updateMonadVarForce :: (Monad m, Eq s) => MonadVar m s a -> m (MonadVar m s a)
updateMonadVarForce (CachedMonadVar _ _ io f) = do
   s <- io
   pure (CachedMonadVar (f s) s io f)
updateMonadVarForce (MonadVar io f) = do
   s <- io
   pure (CachedMonadVar (f s) s io f)



----------------------------------
-- Non-empty MonadVar
----------------------------------


-- Non-empty MonadVar
data MonadVarNE m s a
   = MonadVarNE a !s !(m s) (s -> a) -- ^ Additional cached transformed and read values.
   deriving (Functor)

-- | Check if the MonadVarNE cache needs to be updated.
updateMonadVarNEMaybe :: (Monad m, Eq s) => MonadVarNE m s a -> m (Maybe (MonadVarNE m s a))
updateMonadVarNEMaybe (MonadVarNE _ s io f) = do
   s' <- io
   if s == s'
      then pure Nothing
      else pure <| Just <| MonadVarNE (f s') s' io f

-- | Check if the MonadVarNE cache needs to be updated. Return the updated
-- MonadVarNE
updateMonadVarNE :: (Monad m, Eq s) => MonadVarNE m s a -> m (MonadVarNE m s a)
updateMonadVarNE dv = fromMaybe dv <|| updateMonadVarNEMaybe dv
