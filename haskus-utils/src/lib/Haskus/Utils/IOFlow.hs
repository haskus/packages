{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

-- | IO control-flow with cache
module Haskus.Utils.IOFlow
   ( IOFlowF (..)
   , IOFlow
   , runIOFlow
   , runIO
   , withIO
   , scopeIO
   , emitIO
   -- * Cached control flow
   , CachedIOFlow (..)
   , cacheIOFlow
   , cacheIOFlowPure
   , updateCachedIOFlow
   , updateCachedIOFlowMaybe
   , ioFlowToIOTree
   )
where

import Haskus.Utils.Either
import Haskus.Utils.Maybe
import Haskus.Utils.Flow
import Haskus.Utils.IOVar
import Control.Monad.Free

-- | IOFlow Functor
data IOFlowF a e
   = IOPure a e                                           -- emit a pure value
   | forall v. Eq v => IORead (IO v) (v -> e)             -- add an IO variable in current scope
   | forall v. Eq v => IOWith (IO v) (v -> IOFlow a ()) e -- create a new scope using an IO variable
   | IOScope (IOFlow a ())  e                             -- create a new scope

type IOFlow a r = Free (IOFlowF a) r

instance Functor (IOFlowF a) where
   fmap f = \case
      IOPure a e   -> IOPure a (f e)
      IORead v g   -> IORead v (f . g)
      IOScope k e  -> IOScope k (f e)
      IOWith v k e -> IOWith v k (f e)

-- | Run an IOFlow
runIOFlow :: MonadIO m => IOFlow a r -> m (r,[a])
runIOFlow = \case
   Free (IOScope t1 t2) -> do
      (_,r1)  <- runIOFlow t1
      (k2,r2) <- runIOFlow t2
      pure (k2, r1 <> r2)
   Free (IOWith io f t) -> do
      val <- liftIO io
      (_,r1)  <- runIOFlow (f val)
      (k2,r2) <- runIOFlow t
      pure (k2, r1 <> r2)
   Free (IORead io f)  -> do
      val <- liftIO io
      runIOFlow (f val)
   Free (IOPure a t)   -> do
      (k,as) <- runIOFlow t
      pure (k,a:as)
   Pure k              ->
      pure (k,[])


-- | Emit a pure value
emitIO :: a -> IOFlow a ()
emitIO a = liftF (IOPure a ())

-- | Create a new scope
scopeIO :: IOFlow a () -> IOFlow a ()
scopeIO a = liftF (IOScope a ())

-- | Get a variable in IO
--
-- Use `withIO` to clearly limit the variable scope
runIO :: forall v a. (Eq v) => IO v -> IOFlow a v
runIO f = liftF (IORead f id)

-- | Read and use a UI variable in a delimited scope
withIO :: Eq v => IO v -> (v -> IOFlow a ()) -> IOFlow a ()
withIO f g = liftF (IOWith f g ())

------------------------------------------------
-- Cached control-flow
------------------------------------------------

-- | Cached control-flow
newtype CachedIOFlow a = CachedIOFlow [IOTree a]

-- | Create a cache from an IOFlow.
--
-- Execute the IOFlow once to get cached values
cacheIOFlow :: MonadIO m => IOFlow a r -> m (CachedIOFlow a)
cacheIOFlow cflow = updateCachedIOFlow (cacheIOFlowPure cflow)

-- | Create a cache from an IOFlow.
--
-- This is the pure version: IO dependent nodes may not have any cached value
cacheIOFlowPure :: IOFlow a r -> CachedIOFlow a
cacheIOFlowPure f = (CachedIOFlow (ioFlowToIOTree f))

-- | Update a cached IOFlow
updateCachedIOFlow :: MonadIO m => CachedIOFlow a -> m (CachedIOFlow a)
updateCachedIOFlow cflow = do
   cflow' <- updateCachedIOFlowMaybe cflow
   pure (fromMaybe cflow cflow')

-- | Update a cached IOFlow
--
-- Return Nothing if nothing changed
updateCachedIOFlowMaybe :: MonadIO m => CachedIOFlow a -> m (Maybe (CachedIOFlow a))
updateCachedIOFlowMaybe (CachedIOFlow trees) = do
   trees' <- forM trees updateIOTree
   if all isLeft trees'
      then pure Nothing
      else do
         let rOrL (Right x) = x
             rOrL (Left x)  = x
         pure (Just (CachedIOFlow (rOrL <|| trees')))

ioFlowToIOTree :: IOFlow a r -> [IOTree a]
ioFlowToIOTree = \case
   Free (IOScope t1 t2) -> StaticBranch (Node Nothing (ioFlowToIOTree t1)): ioFlowToIOTree t2
   Free (IORead io f)   -> [ IOBranch (IOVar io (Node Nothing . ioFlowToIOTree . f)) ]
   Free (IOWith io f c) -> IOBranch (IOVar io (Node Nothing . ioFlowToIOTree . f)):ioFlowToIOTree c
   Free (IOPure a t)    -> StaticBranch (Node (Just a) []):ioFlowToIOTree t
   Pure _               -> []

