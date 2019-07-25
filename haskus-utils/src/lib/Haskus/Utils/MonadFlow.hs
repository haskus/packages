{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

-- | IO control-flow with cache
module Haskus.Utils.MonadFlow
   ( MonadFlowF (..)
   , MonadFlow
   , runMonadFlow
   , runM
   , withM
   , emitM
   -- * Cached control flow
   , CachedMonadFlow (..)
   , cacheMonadFlow
   , cacheMonadFlowPure
   , updateCachedMonadFlow
   , updateCachedMonadFlowMaybe
   , monadFlowToMonadTree
   )
where

import Haskus.Utils.Flow
import Haskus.Utils.MonadVar
import Haskus.Utils.MonadStream
import Control.Monad.Free

-- | MonadFlow Functor
data MonadFlowF m a e
   = MonadEmit a e                                               -- emit a pure value
   | forall v. Eq v => MonadRead (m v) (v -> e)                  -- read a monadic value and put it in the current scope
   | forall v. Eq v => MonadWith (m v) (v -> MonadFlow m a ()) e -- open a new scope and read a monadic value in it

type MonadFlow m a r = Free (MonadFlowF m a) r

instance Functor (MonadFlowF m a) where
   fmap f = \case
      MonadEmit a e   -> MonadEmit a (f e)
      MonadRead v g   -> MonadRead v (f . g)
      MonadWith v k e -> MonadWith v k (f e)

-- | Run an MonadFlow
runMonadFlow :: Monad m => MonadFlow m a r -> m (r,[a])
runMonadFlow = \case
   Free (MonadWith io f t) -> do
      val <- io
      (_,r1)  <- runMonadFlow (f val)
      (k2,r2) <- runMonadFlow t
      pure (k2, r1 <> r2)
   Free (MonadRead io f)  -> do
      val <- io
      runMonadFlow (f val)
   Free (MonadEmit a t)   -> do
      (k,as) <- runMonadFlow t
      pure (k,a:as)
   Pure k              ->
      pure (k,[])


-- | Emit a pure value
emitM :: a -> MonadFlow m a ()
emitM a = liftF (MonadEmit a ())

-- | Get a variable in IO
--
-- Use `withM` to clearly limit the variable scope
runM :: forall m v a. (Eq v) => m v -> MonadFlow m a v
runM f = liftF (MonadRead f id)

-- | Read and use an IO variable in a delimited scope
withM :: Eq v => m v -> (v -> MonadFlow m a ()) -> MonadFlow m a ()
withM f g = liftF (MonadWith f g ())

------------------------------------------------
-- Cached control-flow
------------------------------------------------

-- | Cached control-flow
data CachedMonadFlow m a = CachedMonadFlow
   { cachedTree    :: [MonadTree m a]      -- ^ Cached control-flow as an MonadTree
   , cachedContext :: forall b. m b -> m b -- ^ Monadic context when performing an update (e.g. withSnapshot ctx)
   }
   deriving (Functor)

-- | Create a cache from an MonadFlow.
--
-- Execute the MonadFlow once to get cached values
cacheMonadFlow :: Monad m => (forall b. m b -> m b) -> MonadFlow m a r -> m (CachedMonadFlow m a)
cacheMonadFlow ctx cflow = updateCachedMonadFlow (cacheMonadFlowPure ctx cflow)

-- | Create a cache from an MonadFlow.
--
-- This is the pure version: IO dependent nodes may not have any cached value
cacheMonadFlowPure :: (forall b. m b -> m b) -> MonadFlow m a r -> CachedMonadFlow m a
cacheMonadFlowPure ctx f = (CachedMonadFlow (monadFlowToMonadTree f) ctx)

-- | Update a cached MonadFlow
updateCachedMonadFlow :: Monad m => CachedMonadFlow m a -> m (CachedMonadFlow m a)
updateCachedMonadFlow (CachedMonadFlow trees withCtx) = do
   trees' <- withCtx (forM trees updateMonadStream)
   pure (CachedMonadFlow trees' withCtx)

-- | Update a cached MonadFlow
updateCachedMonadFlowMaybe :: Monad m => CachedMonadFlow m a -> m (Maybe (CachedMonadFlow m a))
updateCachedMonadFlowMaybe (CachedMonadFlow trees withCtx) =
   withCtx (updateMonadStreamsMaybe trees)
   |||> (\ts -> CachedMonadFlow ts withCtx)

monadFlowToMonadTree :: MonadFlow m a r -> [MonadTree m a]
monadFlowToMonadTree = \case
   Free (MonadRead io f)   -> [ MonadStream (MonadVarNE [] Nothing io (monadFlowToMonadTree . f)) ]
   Free (MonadWith io f c) -> MonadStream (MonadVarNE [] Nothing io (monadFlowToMonadTree . f)):monadFlowToMonadTree c
   Free (MonadEmit a t)    -> PureStream a []:monadFlowToMonadTree t
   Pure _                  -> []

