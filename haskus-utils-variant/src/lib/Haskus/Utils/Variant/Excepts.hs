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

module Haskus.Utils.Variant.Excepts
   ( Excepts
   , runExcepts
   , runExcepts_
   , evalExcepts
   , evalCatchExcepts
   , injectExcepts
   , mapExcepts
   , liftExcepts
   , variantToExcepts
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
   , onExceptsError_
   , onExceptsError
   , finallyExcepts
   -- * Reexport
   , module Haskus.Utils.Variant.VEither
   )
where

import Haskus.Utils.Monad
import Haskus.Utils.Variant.VEither

import Control.Monad.Catch

------------------------------------------------------------------------------
-- Excepts
------------------------------------------------------------------------------
newtype Excepts es m a = Excepts (m (VEither es a))

deriving instance Show (m (VEither es a)) => Show (Excepts es m a)

runExcepts :: forall es a m.
   Excepts es m a -> m (VEither es a)
{-# INLINABLE runExcepts #-}
runExcepts (Excepts m) = m

runExcepts_ :: forall es a m.
   Functor m => Excepts es m a -> m ()
{-# INLINABLE runExcepts_ #-}
runExcepts_ m = void (runExcepts m)

injectExcepts :: forall es a m.
   Monad m => Excepts es m a -> Excepts es m (VEither es a)
{-# INLINABLE injectExcepts #-}
injectExcepts (Excepts m) = return =<< lift m

-- | Convert a flow without error into a value
evalExcepts :: Monad m => Excepts '[] m a -> m a
{-# INLINABLE evalExcepts #-}
evalExcepts v = veitherToValue <$> runExcepts v

mapExcepts :: (m (VEither es a) -> n (VEither es' b)) -> Excepts es m a -> Excepts es' n b
{-# INLINABLE mapExcepts #-}
mapExcepts f = Excepts . f . runExcepts

-- | Lift a Excepts into another
liftExcepts :: forall es' es a m.
   ( Monad m
   , VEitherLift es es'
   ) => Excepts es m a -> Excepts es' m a
{-# INLINABLE liftExcepts #-}
liftExcepts = mapExcepts (liftM veitherLift)

instance Functor m => Functor (Excepts es m) where
   {-# INLINABLE fmap #-}
   fmap f = mapExcepts (fmap (fmap f))

instance Foldable m => Foldable (Excepts es m) where
   {-# INLINABLE foldMap #-}
   foldMap f (Excepts m) = foldMap (veitherCont (const mempty) f) m

instance Traversable m => Traversable (Excepts es m) where
   {-# INLINABLE traverse #-}
   traverse f (Excepts m) =
      Excepts <$> traverse (veitherCont (pure . VLeft) (fmap VRight . f)) m

instance (Functor m, Monad m) => Applicative (Excepts es m) where
    {-# INLINABLE pure #-}
    pure a = Excepts $ return (VRight a)

    {-# INLINABLE (<*>) #-}
    Excepts mf <*> Excepts ma = Excepts $ do
      f <- mf
      a <- ma
      pure (f <*> a)

    {-# INLINABLE (*>) #-}
    m *> k = m >>= \_ -> k

instance (Monad m) => Monad (Excepts es m) where
    {-# INLINABLE (>>=) #-}
    m >>= k = Excepts $ do
        a <- runExcepts m
        case a of
            VLeft es -> return (VLeft es)
            VRight x -> runExcepts (k x)

    {-# INLINABLE fail #-}
    fail = Excepts . fail

instance MonadTrans (Excepts e) where
    {-# INLINABLE lift #-}
    lift = Excepts . liftM VRight

instance (MonadIO m) => MonadIO (Excepts es m) where
    {-# INLINABLE liftIO #-}
    liftIO = lift . liftIO


-- | Throws exceptions into the base monad.
instance MonadThrow m => MonadThrow (Excepts e m) where
   {-# INLINABLE throwM #-}
   throwM = lift . throwM

-- | Catches exceptions from the base monad.
instance MonadCatch m => MonadCatch (Excepts e m) where
   catch (Excepts m) f = Excepts $ catch m (runExcepts . f)

instance MonadMask m => MonadMask (Excepts e m) where
   mask f = Excepts $ mask $ \u -> runExcepts $ f (q u)
      where
         q :: (m (VEither e a) -> m (VEither e a)) -> Excepts e m a -> Excepts e m a
         q u (Excepts b) = Excepts (u b)

   uninterruptibleMask f = Excepts $ uninterruptibleMask $ \u -> runExcepts $ f (q u)
      where
         q :: (m (VEither e a) -> m (VEither e a)) -> Excepts e m a -> Excepts e m a
         q u (Excepts b) = Excepts (u b)

   generalBracket acquire release use = Excepts $ do
      (eb, ec) <- generalBracket
         (runExcepts acquire)
         (\eresource exitCase -> case eresource of
            VLeft e -> return (VLeft e) -- nothing to release, acquire didn't succeed
            VRight resource -> case exitCase of
               ExitCaseSuccess (VRight b) -> runExcepts (release resource (ExitCaseSuccess b))
               ExitCaseException e        -> runExcepts (release resource (ExitCaseException e))
               _                          -> runExcepts (release resource ExitCaseAbort))
         (veitherCont (return . VLeft) (runExcepts . use))
      runExcepts $ do
         -- The order in which we perform those two 'Excepts' effects determines
         -- which error will win if they are both erroring. We want the error from
         -- 'release' to win.
         c <- Excepts (return ec)
         b <- Excepts (return eb)
         return (b, c)



-- | Success value
success :: Monad m => a -> Excepts '[] m a
success = pure

-- | Signal an exception value @e@.
throwE :: forall e es a m. (Monad m, e :< es) => e -> Excepts es m a
{-# INLINABLE throwE #-}
throwE = Excepts . pure . VLeft . V

-- | Signal an exception value @e@.
failure :: forall e a m. Monad m => e -> Excepts '[e] m a
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
    Excepts es m a -> (e -> Excepts es'' m a) -> Excepts es' m a
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
    Excepts es m a -> (e -> Excepts es'' m a) -> Excepts es' m a
{-# INLINABLE catchLiftBoth #-}
m `catchLiftBoth` h = Excepts $ do
   a <- runExcepts m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> runExcepts (liftExcepts (h l))
         Left rs -> return (VLeft (liftVariant rs))

-- | Handle an exception. Assume it is in the first position
catchRemove :: forall e es a m.
   ( Monad m
   ) =>
    Excepts (e ': es) m a -> (e -> Excepts es m a) -> Excepts es m a
{-# INLINABLE catchRemove #-}
m `catchRemove` h = Excepts $ do
   a <- runExcepts m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariantHead ls of
         Right l -> runExcepts (h l)
         Left rs -> return (VLeft rs)

-- | Handle an exception. Lift the remaining errors into the resulting flow
catchLiftLeft :: forall e es es' a m.
   ( Monad m
   , e :< es
   , LiftVariant (Remove e es) es'
   ) =>
    Excepts es m a -> (e -> Excepts es' m a) -> Excepts es' m a
{-# INLINABLE catchLiftLeft #-}
m `catchLiftLeft` h = Excepts $ do
   a <- runExcepts m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> runExcepts (h l)
         Left rs -> return (VLeft (liftVariant rs))

-- | Handle an exception. Lift the handler into the resulting flow
catchLiftRight :: forall e es es' a m.
   ( Monad m
   , e :< es
   , LiftVariant es' (Remove e es)
   ) =>
    Excepts es m a -> (e -> Excepts es' m a) -> Excepts (Remove e es) m a
{-# INLINABLE catchLiftRight #-}
m `catchLiftRight` h = Excepts $ do
   a <- runExcepts m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> runExcepts (liftExcepts (h l))
         Left rs -> return (VLeft rs)

-- | Do something in case of error
catchAllE :: Monad m => Excepts es m a -> (V es -> Excepts es' m a) -> Excepts es' m a
{-# INLINABLE catchAllE #-}
m `catchAllE` h = Excepts $ do
   a <- runExcepts m
   case a of
      VRight x  -> return (VRight x)
      VLeft xs  -> runExcepts (h xs)

-- | Evaluate a Excepts. Use the provided function to handle error cases.
evalCatchExcepts :: Monad m => (V es -> m a) -> Excepts es m a -> m a
{-# INLINABLE evalCatchExcepts #-}
evalCatchExcepts h m = do
   a <- runExcepts m
   case a of
      VRight x  -> return x
      VLeft xs  -> h xs

-- | Evaluate a Excepts. Use the provided function to handle error cases.
catchDieAll :: Monad m => Excepts es m a -> (V es -> m a) -> m a
{-# INLINABLE catchDieAll #-}
catchDieAll m h = evalCatchExcepts h m

-- | Catch and die in case of error
catchDie :: (e :< es, Monad m) => Excepts es m a -> (e -> m ()) -> Excepts (Remove e es) m a
{-# INLINABLE catchDie #-}
m `catchDie` h = Excepts $ do
   a <- runExcepts m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> h l >> error "catchDie"
         Left rs -> return (VLeft rs)

-- | Do something in case of error
onExceptsError_ :: Monad m => Excepts es m a -> m () -> Excepts es m a
{-# INLINABLE onExceptsError_ #-}
m `onExceptsError_` h = Excepts $ do
   a <- runExcepts m
   case a of
      VRight _ -> return a
      VLeft _  -> h >> return a

-- | Do something in case of error
onExceptsError :: Monad m => Excepts es m a -> (V es -> m ()) -> Excepts es m a
{-# INLINABLE onExceptsError #-}
m `onExceptsError` h = Excepts $ do
   a <- runExcepts m
   case a of
      VRight _  -> return a
      VLeft es  -> h es >> return a

-- | Finally for Excepts
finallyExcepts :: Monad m => Excepts es m a -> m () -> Excepts es m a
{-# INLINABLE finallyExcepts #-}
m `finallyExcepts` h = Excepts $ do
   a <- runExcepts m
   h
   return a

-- | Convert a Variant into a Excepts
variantToExcepts :: Monad m => V (a ': es) -> Excepts es m a
{-# INLINABLE variantToExcepts #-}
variantToExcepts v = Excepts (return (veitherFromVariant v))

instance MonadInIO m => MonadInIO (Excepts es m) where
   {-# INLINABLE liftWith #-}
   liftWith wth f =
      Excepts $ liftWith wth (\a -> runExcepts (f a))

   {-# INLINABLE liftWith2 #-}
   liftWith2 wth f =
      Excepts $ liftWith2 wth (\a b -> runExcepts (f a b))
