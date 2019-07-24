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
   , scopeM
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

import Haskus.Utils.Either
import Haskus.Utils.Maybe
import Haskus.Utils.Flow
import Haskus.Utils.MonadVar
import Control.Monad.Free

-- | MonadFlow Functor
data MonadFlowF m a e
   = MonadEdmit a e                                               -- emit a pure value
   | forall v. Eq v => MonadRead (m v) (v -> e)                  -- read a monadic value and put it in the current scope
   | forall v. Eq v => MonadWith (m v) (v -> MonadFlow m a ()) e -- open a new scope and read a monadic value in it
   | MonadScope (MonadFlow m a ())  e                            -- open a new scope

type MonadFlow m a r = Free (MonadFlowF m a) r

instance Functor (MonadFlowF m a) where
   fmap f = \case
      MonadEdmit a e   -> MonadEdmit a (f e)
      MonadRead v g   -> MonadRead v (f . g)
      MonadScope k e  -> MonadScope k (f e)
      MonadWith v k e -> MonadWith v k (f e)

-- | Run an MonadFlow
runMonadFlow :: Monad m => MonadFlow m a r -> m (r,[a])
runMonadFlow = \case
   Free (MonadScope t1 t2) -> do
      (_,r1)  <- runMonadFlow t1
      (k2,r2) <- runMonadFlow t2
      pure (k2, r1 <> r2)
   Free (MonadWith io f t) -> do
      val <- io
      (_,r1)  <- runMonadFlow (f val)
      (k2,r2) <- runMonadFlow t
      pure (k2, r1 <> r2)
   Free (MonadRead io f)  -> do
      val <- io
      runMonadFlow (f val)
   Free (MonadEdmit a t)   -> do
      (k,as) <- runMonadFlow t
      pure (k,a:as)
   Pure k              ->
      pure (k,[])


-- | Emit a pure value
emitM :: a -> MonadFlow m a ()
emitM a = liftF (MonadEdmit a ())

-- | Create a new scope
scopeM :: MonadFlow m a () -> MonadFlow m a ()
scopeM a = liftF (MonadScope a ())

-- | Get a variable in IO
--
-- Use `withM` to clearly limit the variable scope
runM :: forall m v a. (Eq v) => m v -> MonadFlow m a v
runM f = liftF (MonadRead f id)

-- | Read and use a UI variable in a delimited scope
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
updateCachedMonadFlow cflow = do
   cflow' <- updateCachedMonadFlowMaybe cflow
   pure (fromMaybe cflow cflow')

-- | Update a cached MonadFlow
--
-- Return Nothing if nothing changed
updateCachedMonadFlowMaybe :: Monad m => CachedMonadFlow m a -> m (Maybe (CachedMonadFlow m a))
updateCachedMonadFlowMaybe (CachedMonadFlow trees withCtx) = do
   trees' <- withCtx (forM trees updateMonadTree)
   if all isLeft trees'
      then pure Nothing
      else do
         let rOrL (Right x) = x
             rOrL (Left x)  = x
         pure (Just (CachedMonadFlow (rOrL <|| trees') withCtx))

monadFlowToMonadTree :: MonadFlow m a r -> [MonadTree m a]
monadFlowToMonadTree = \case
   Free (MonadScope t1 t2) -> StaticBranch (Node Nothing (monadFlowToMonadTree t1)): monadFlowToMonadTree t2
   Free (MonadRead io f)   -> [ MonadBranch (MonadVar io (Node Nothing . monadFlowToMonadTree . f)) ]
   Free (MonadWith io f c) -> MonadBranch (MonadVar io (Node Nothing . monadFlowToMonadTree . f)):monadFlowToMonadTree c
   Free (MonadEdmit a t)   -> StaticBranch (Node (Just a) []):monadFlowToMonadTree t
   Pure _                  -> []

