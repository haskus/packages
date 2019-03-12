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
   ( FlowT
   , runFlowT
   , runFlowT_
   , evalFlowT
   , evalCatchFlowT
   , injectFlowT
   , mapFlowT
   , liftFlowT
   , variantToFlowT
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
-- FlowT
------------------------------------------------------------------------------
newtype FlowT es m a = FlowT (m (VEither es a))

deriving instance Show (m (VEither es a)) => Show (FlowT es m a)

runFlowT :: forall es a m.
   FlowT es m a -> m (VEither es a)
{-# INLINABLE runFlowT #-}
runFlowT (FlowT m) = m

runFlowT_ :: forall es a m.
   Functor m => FlowT es m a -> m ()
{-# INLINABLE runFlowT_ #-}
runFlowT_ m = void (runFlowT m)

injectFlowT :: forall es a m.
   Monad m => FlowT es m a -> FlowT es m (VEither es a)
{-# INLINABLE injectFlowT #-}
injectFlowT (FlowT m) = return =<< lift m

-- | Convert a flow without error into a value
evalFlowT :: Monad m => FlowT '[] m a -> m a
{-# INLINABLE evalFlowT #-}
evalFlowT v = veitherToValue <$> runFlowT v

mapFlowT :: (m (VEither es a) -> n (VEither es' b)) -> FlowT es m a -> FlowT es' n b
{-# INLINABLE mapFlowT #-}
mapFlowT f = FlowT . f . runFlowT

-- | Lift a FlowT into another
liftFlowT :: forall es' es a m.
   ( Monad m
   , VEitherLift es es'
   ) => FlowT es m a -> FlowT es' m a
{-# INLINABLE liftFlowT #-}
liftFlowT = mapFlowT (liftM veitherLift)

instance Functor m => Functor (FlowT es m) where
   {-# INLINABLE fmap #-}
   fmap f = mapFlowT (fmap (fmap f))

instance Foldable m => Foldable (FlowT es m) where
   {-# INLINABLE foldMap #-}
   foldMap f (FlowT m) = foldMap (veitherCont (const mempty) f) m

instance Traversable m => Traversable (FlowT es m) where
   {-# INLINABLE traverse #-}
   traverse f (FlowT m) =
      FlowT <$> traverse (veitherCont (pure . VLeft) (fmap VRight . f)) m

instance (Functor m, Monad m) => Applicative (FlowT es m) where
    {-# INLINABLE pure #-}
    pure a = FlowT $ return (VRight a)

    {-# INLINABLE (<*>) #-}
    FlowT mf <*> FlowT ma = FlowT $ do
      f <- mf
      a <- ma
      pure (f <*> a)

    {-# INLINABLE (*>) #-}
    m *> k = m >>= \_ -> k

instance (Monad m) => Monad (FlowT es m) where
    {-# INLINABLE (>>=) #-}
    m >>= k = FlowT $ do
        a <- runFlowT m
        case a of
            VLeft es -> return (VLeft es)
            VRight x -> runFlowT (k x)

    {-# INLINABLE fail #-}
    fail = FlowT . fail

instance MonadTrans (FlowT e) where
    {-# INLINABLE lift #-}
    lift = FlowT . liftM VRight

instance (MonadIO m) => MonadIO (FlowT es m) where
    {-# INLINABLE liftIO #-}
    liftIO = lift . liftIO


-- | Throws exceptions into the base monad.
instance MonadThrow m => MonadThrow (FlowT e m) where
   {-# INLINABLE throwM #-}
   throwM = lift . throwM

-- | Catches exceptions from the base monad.
instance MonadCatch m => MonadCatch (FlowT e m) where
   catch (FlowT m) f = FlowT $ catch m (runFlowT . f)

instance MonadMask m => MonadMask (FlowT e m) where
   mask f = FlowT $ mask $ \u -> runFlowT $ f (q u)
      where
         q :: (m (VEither e a) -> m (VEither e a)) -> FlowT e m a -> FlowT e m a
         q u (FlowT b) = FlowT (u b)

   uninterruptibleMask f = FlowT $ uninterruptibleMask $ \u -> runFlowT $ f (q u)
      where
         q :: (m (VEither e a) -> m (VEither e a)) -> FlowT e m a -> FlowT e m a
         q u (FlowT b) = FlowT (u b)

   generalBracket acquire release use = FlowT $ do
      (eb, ec) <- generalBracket
         (runFlowT acquire)
         (\eresource exitCase -> case eresource of
            VLeft e -> return (VLeft e) -- nothing to release, acquire didn't succeed
            VRight resource -> case exitCase of
               ExitCaseSuccess (VRight b) -> runFlowT (release resource (ExitCaseSuccess b))
               ExitCaseException e        -> runFlowT (release resource (ExitCaseException e))
               _                          -> runFlowT (release resource ExitCaseAbort))
         (veitherCont (return . VLeft) (runFlowT . use))
      runFlowT $ do
         -- The order in which we perform those two 'FlowT' effects determines
         -- which error will win if they are both erroring. We want the error from
         -- 'release' to win.
         c <- FlowT (return ec)
         b <- FlowT (return eb)
         return (b, c)



-- | Success value
success :: Monad m => a -> FlowT '[] m a
success = pure

-- | Signal an exception value @e@.
throwE :: forall e es a m. (Monad m, e :< es) => e -> FlowT es m a
{-# INLINABLE throwE #-}
throwE = FlowT . pure . VLeft . V

-- | Signal an exception value @e@.
failure :: forall e a m. Monad m => e -> FlowT '[e] m a
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
    FlowT es m a -> (e -> FlowT es'' m a) -> FlowT es' m a
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
    FlowT es m a -> (e -> FlowT es'' m a) -> FlowT es' m a
{-# INLINABLE catchLiftBoth #-}
m `catchLiftBoth` h = FlowT $ do
   a <- runFlowT m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> runFlowT (liftFlowT (h l))
         Left rs -> return (VLeft (liftVariant rs))

-- | Handle an exception. Assume it is in the first position
catchRemove :: forall e es a m.
   ( Monad m
   ) =>
    FlowT (e ': es) m a -> (e -> FlowT es m a) -> FlowT es m a
{-# INLINABLE catchRemove #-}
m `catchRemove` h = FlowT $ do
   a <- runFlowT m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariantHead ls of
         Right l -> runFlowT (h l)
         Left rs -> return (VLeft rs)

-- | Handle an exception. Lift the remaining errors into the resulting flow
catchLiftLeft :: forall e es es' a m.
   ( Monad m
   , e :< es
   , LiftVariant (Remove e es) es'
   ) =>
    FlowT es m a -> (e -> FlowT es' m a) -> FlowT es' m a
{-# INLINABLE catchLiftLeft #-}
m `catchLiftLeft` h = FlowT $ do
   a <- runFlowT m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> runFlowT (h l)
         Left rs -> return (VLeft (liftVariant rs))

-- | Handle an exception. Lift the handler into the resulting flow
catchLiftRight :: forall e es es' a m.
   ( Monad m
   , e :< es
   , LiftVariant es' (Remove e es)
   ) =>
    FlowT es m a -> (e -> FlowT es' m a) -> FlowT (Remove e es) m a
{-# INLINABLE catchLiftRight #-}
m `catchLiftRight` h = FlowT $ do
   a <- runFlowT m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> runFlowT (liftFlowT (h l))
         Left rs -> return (VLeft rs)

-- | Do something in case of error
catchAllE :: Monad m => FlowT es m a -> (V es -> FlowT es' m a) -> FlowT es' m a
{-# INLINABLE catchAllE #-}
m `catchAllE` h = FlowT $ do
   a <- runFlowT m
   case a of
      VRight x  -> return (VRight x)
      VLeft xs  -> runFlowT (h xs)

-- | Evaluate a FlowT. Use the provided function to handle error cases.
evalCatchFlowT :: Monad m => (V es -> m a) -> FlowT es m a -> m a
{-# INLINABLE evalCatchFlowT #-}
evalCatchFlowT h m = do
   a <- runFlowT m
   case a of
      VRight x  -> return x
      VLeft xs  -> h xs

-- | Evaluate a FlowT. Use the provided function to handle error cases.
catchDieAll :: Monad m => FlowT es m a -> (V es -> m a) -> m a
{-# INLINABLE catchDieAll #-}
catchDieAll m h = evalCatchFlowT h m

-- | Catch and die in case of error
catchDie :: (e :< es, Monad m) => FlowT es m a -> (e -> m ()) -> FlowT (Remove e es) m a
{-# INLINABLE catchDie #-}
m `catchDie` h = FlowT $ do
   a <- runFlowT m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> h l >> error "catchDie"
         Left rs -> return (VLeft rs)

-- | Do something in case of error
onFlowError_ :: Monad m => FlowT es m a -> m () -> FlowT es m a
{-# INLINABLE onFlowError_ #-}
m `onFlowError_` h = FlowT $ do
   a <- runFlowT m
   case a of
      VRight _ -> return a
      VLeft _  -> h >> return a

-- | Do something in case of error
onFlowError :: Monad m => FlowT es m a -> (V es -> m ()) -> FlowT es m a
{-# INLINABLE onFlowError #-}
m `onFlowError` h = FlowT $ do
   a <- runFlowT m
   case a of
      VRight _  -> return a
      VLeft es  -> h es >> return a

-- | Finally for FlowT
finallyFlow :: Monad m => FlowT es m a -> m () -> FlowT es m a
{-# INLINABLE finallyFlow #-}
m `finallyFlow` h = FlowT $ do
   a <- runFlowT m
   h
   return a

-- | Convert a Variant into a FlowT
variantToFlowT :: Monad m => V (a ': es) -> FlowT es m a
{-# INLINABLE variantToFlowT #-}
variantToFlowT v = FlowT (return (veitherFromVariant v))

instance MonadInIO m => MonadInIO (FlowT es m) where
   {-# INLINABLE liftWith #-}
   liftWith wth f =
      FlowT $ liftWith wth (\a -> runFlowT (f a))

   {-# INLINABLE liftWith2 #-}
   liftWith2 wth f =
      FlowT $ liftWith2 wth (\a b -> runFlowT (f a b))
