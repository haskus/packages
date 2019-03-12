{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Haskus.Utils.Variant.Flow
   ( Flow
   , runFlow
   , runFlow_
   , evalFlow
   , evalCatchFlow
   , injectFlow
   , mapFlow
   , liftFlow
   , variantToFlow
   , success
   , failure
   , throwE
   , catchE
   , catchLiftBoth
   , catchLiftLeft
   , catchLiftRight
   , catchAllE
   , catchDie
   , catchDieAll
   , catchRemove
   , onFlowError_
   , onFlowError
   , finallyFlow
   -- * Reexport
   , module Haskus.Utils.Variant.VEither
   )
where

import Haskus.Utils.Monad
import Haskus.Utils.Variant.VEither

import Control.Monad.Catch

------------------------------------------------------------------------------
-- Flow
------------------------------------------------------------------------------
newtype Flow es m a = Flow (m (VEither es a))

deriving instance Show (m (VEither es a)) => Show (Flow es m a)

runFlow :: forall es a m.
   Flow es m a -> m (VEither es a)
{-# INLINABLE runFlow #-}
runFlow (Flow m) = m

runFlow_ :: forall es a m.
   Functor m => Flow es m a -> m ()
{-# INLINABLE runFlow_ #-}
runFlow_ m = void (runFlow m)

injectFlow :: forall es a m.
   Monad m => Flow es m a -> Flow es m (VEither es a)
{-# INLINABLE injectFlow #-}
injectFlow (Flow m) = return =<< lift m

-- | Convert a flow without error into a value
evalFlow :: Monad m => Flow '[] m a -> m a
{-# INLINABLE evalFlow #-}
evalFlow v = veitherToValue <$> runFlow v

mapFlow :: (m (VEither es a) -> n (VEither es' b)) -> Flow es m a -> Flow es' n b
{-# INLINABLE mapFlow #-}
mapFlow f = Flow . f . runFlow

-- | Lift a Flow into another
liftFlow :: forall es' es a m.
   ( Monad m
   , VEitherLift es es'
   ) => Flow es m a -> Flow es' m a
{-# INLINABLE liftFlow #-}
liftFlow = mapFlow (liftM veitherLift)

instance Functor m => Functor (Flow es m) where
   {-# INLINABLE fmap #-}
   fmap f = mapFlow (fmap (fmap f))

instance Foldable m => Foldable (Flow es m) where
   {-# INLINABLE foldMap #-}
   foldMap f (Flow m) = foldMap (veitherCont (const mempty) f) m

instance Traversable m => Traversable (Flow es m) where
   {-# INLINABLE traverse #-}
   traverse f (Flow m) =
      Flow <$> traverse (veitherCont (pure . VLeft) (fmap VRight . f)) m

instance (Functor m, Monad m) => Applicative (Flow es m) where
    {-# INLINABLE pure #-}
    pure a = Flow $ return (VRight a)

    {-# INLINABLE (<*>) #-}
    Flow mf <*> Flow ma = Flow $ do
      f <- mf
      a <- ma
      pure (f <*> a)

    {-# INLINABLE (*>) #-}
    m *> k = m >>= \_ -> k

instance (Monad m) => Monad (Flow es m) where
    {-# INLINABLE (>>=) #-}
    m >>= k = Flow $ do
        a <- runFlow m
        case a of
            VLeft es -> return (VLeft es)
            VRight x -> runFlow (k x)

    {-# INLINABLE fail #-}
    fail = Flow . fail

instance MonadTrans (Flow e) where
    {-# INLINABLE lift #-}
    lift = Flow . liftM VRight

instance (MonadIO m) => MonadIO (Flow es m) where
    {-# INLINABLE liftIO #-}
    liftIO = lift . liftIO


-- | Throws exceptions into the base monad.
instance MonadThrow m => MonadThrow (Flow e m) where
   {-# INLINABLE throwM #-}
   throwM = lift . throwM

-- | Catches exceptions from the base monad.
instance MonadCatch m => MonadCatch (Flow e m) where
   catch (Flow m) f = Flow $ catch m (runFlow . f)

instance MonadMask m => MonadMask (Flow e m) where
   mask f = Flow $ mask $ \u -> runFlow $ f (q u)
      where
         q :: (m (VEither e a) -> m (VEither e a)) -> Flow e m a -> Flow e m a
         q u (Flow b) = Flow (u b)

   uninterruptibleMask f = Flow $ uninterruptibleMask $ \u -> runFlow $ f (q u)
      where
         q :: (m (VEither e a) -> m (VEither e a)) -> Flow e m a -> Flow e m a
         q u (Flow b) = Flow (u b)

   generalBracket acquire release use = Flow $ do
      (eb, ec) <- generalBracket
         (runFlow acquire)
         (\eresource exitCase -> case eresource of
            VLeft e -> return (VLeft e) -- nothing to release, acquire didn't succeed
            VRight resource -> case exitCase of
               ExitCaseSuccess (VRight b) -> runFlow (release resource (ExitCaseSuccess b))
               ExitCaseException e        -> runFlow (release resource (ExitCaseException e))
               _                          -> runFlow (release resource ExitCaseAbort))
         (veitherCont (return . VLeft) (runFlow . use))
      runFlow $ do
         -- The order in which we perform those two 'Flow' effects determines
         -- which error will win if they are both erroring. We want the error from
         -- 'release' to win.
         c <- Flow (return ec)
         b <- Flow (return eb)
         return (b, c)



-- | Success value
success :: Monad m => a -> Flow '[] m a
success = pure

-- | Signal an exception value @e@.
throwE :: forall e es a m. (Monad m, e :< es) => e -> Flow es m a
{-# INLINABLE throwE #-}
throwE = Flow . pure . VLeft . V

-- | Signal an exception value @e@.
failure :: forall e a m. Monad m => e -> Flow '[e] m a
{-# INLINABLE failure #-}
failure = throwE

-- | Handle an exception. Lift both normal and exceptional flows into the result
-- flow
catchE :: forall e es' es'' es a m.
   ( Monad m
   , e :< es
   , LiftVariant (Remove e es) es'
   , LiftVariant es'' es'
   ) =>
    Flow es m a -> (e -> Flow es'' m a) -> Flow es' m a
{-# INLINABLE catchE #-}
m `catchE` h = m `catchLiftBoth` h

-- | Handle an exception. Lift both normal and exceptional flows into the result
-- flow
catchLiftBoth :: forall e es' es'' es a m.
   ( Monad m
   , e :< es
   , LiftVariant (Remove e es) es'
   , LiftVariant es'' es'
   ) =>
    Flow es m a -> (e -> Flow es'' m a) -> Flow es' m a
{-# INLINABLE catchLiftBoth #-}
m `catchLiftBoth` h = Flow $ do
   a <- runFlow m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> runFlow (liftFlow (h l))
         Left rs -> return (VLeft (liftVariant rs))

-- | Handle an exception. Assume it is in the first position
catchRemove :: forall e es a m.
   ( Monad m
   ) =>
    Flow (e ': es) m a -> (e -> Flow es m a) -> Flow es m a
{-# INLINABLE catchRemove #-}
m `catchRemove` h = Flow $ do
   a <- runFlow m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariantHead ls of
         Right l -> runFlow (h l)
         Left rs -> return (VLeft rs)

-- | Handle an exception. Lift the remaining errors into the resulting flow
catchLiftLeft :: forall e es es' a m.
   ( Monad m
   , e :< es
   , LiftVariant (Remove e es) es'
   ) =>
    Flow es m a -> (e -> Flow es' m a) -> Flow es' m a
{-# INLINABLE catchLiftLeft #-}
m `catchLiftLeft` h = Flow $ do
   a <- runFlow m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> runFlow (h l)
         Left rs -> return (VLeft (liftVariant rs))

-- | Handle an exception. Lift the handler into the resulting flow
catchLiftRight :: forall e es es' a m.
   ( Monad m
   , e :< es
   , LiftVariant es' (Remove e es)
   ) =>
    Flow es m a -> (e -> Flow es' m a) -> Flow (Remove e es) m a
{-# INLINABLE catchLiftRight #-}
m `catchLiftRight` h = Flow $ do
   a <- runFlow m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> runFlow (liftFlow (h l))
         Left rs -> return (VLeft rs)

-- | Do something in case of error
catchAllE :: Monad m => Flow es m a -> (V es -> Flow es' m a) -> Flow es' m a
{-# INLINABLE catchAllE #-}
m `catchAllE` h = Flow $ do
   a <- runFlow m
   case a of
      VRight x  -> return (VRight x)
      VLeft xs  -> runFlow (h xs)

-- | Evaluate a Flow. Use the provided function to handle error cases.
evalCatchFlow :: Monad m => (V es -> m a) -> Flow es m a -> m a
{-# INLINABLE evalCatchFlow #-}
evalCatchFlow h m = do
   a <- runFlow m
   case a of
      VRight x  -> return x
      VLeft xs  -> h xs

-- | Evaluate a Flow. Use the provided function to handle error cases.
catchDieAll :: Monad m => Flow es m a -> (V es -> m a) -> m a
{-# INLINABLE catchDieAll #-}
catchDieAll m h = evalCatchFlow h m

-- | Catch and die in case of error
catchDie :: (e :< es, Monad m) => Flow es m a -> (e -> m ()) -> Flow (Remove e es) m a
{-# INLINABLE catchDie #-}
m `catchDie` h = Flow $ do
   a <- runFlow m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> h l >> error "catchDie"
         Left rs -> return (VLeft rs)

-- | Do something in case of error
onFlowError_ :: Monad m => Flow es m a -> m () -> Flow es m a
{-# INLINABLE onFlowError_ #-}
m `onFlowError_` h = Flow $ do
   a <- runFlow m
   case a of
      VRight _ -> return a
      VLeft _  -> h >> return a

-- | Do something in case of error
onFlowError :: Monad m => Flow es m a -> (V es -> m ()) -> Flow es m a
{-# INLINABLE onFlowError #-}
m `onFlowError` h = Flow $ do
   a <- runFlow m
   case a of
      VRight _  -> return a
      VLeft es  -> h es >> return a

-- | Finally for Flow
finallyFlow :: Monad m => Flow es m a -> m () -> Flow es m a
{-# INLINABLE finallyFlow #-}
m `finallyFlow` h = Flow $ do
   a <- runFlow m
   h
   return a

-- | Convert a Variant into a Flow
variantToFlow :: Monad m => V (a ': es) -> Flow es m a
{-# INLINABLE variantToFlow #-}
variantToFlow v = Flow (return (veitherFromVariant v))

instance MonadInIO m => MonadInIO (Flow es m) where
   {-# INLINABLE liftWith #-}
   liftWith wth f =
      Flow $ liftWith wth (\a -> runFlow (f a))

   {-# INLINABLE liftWith2 #-}
   liftWith2 wth f =
      Flow $ liftWith2 wth (\a b -> runFlow (f a b))
