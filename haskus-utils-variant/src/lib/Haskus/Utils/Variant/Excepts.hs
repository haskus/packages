{-# LANGUAGE CPP #-}
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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Haskus.Utils.Variant.Excepts
   ( Excepts (..)
   , runE
   , runE_
   , liftE
   , appendE
   , prependE
   , failureE
   , successE
   , throwE
   , throwSomeE
   , catchE
   , catchEvalE
   , evalE
   , onE_
   , onE
   , finallyE
   , injectExcepts
   , withExcepts
   , withExcepts_
   , mapExcepts
   , variantToExcepts
   , veitherToExcepts
   , catchLiftBoth
   , catchLiftLeft
   , catchLiftRight
   , catchAllE
   , catchDieE
   , catchRemove
   , sequenceE
   , runBothE
   -- * Reexport
   , module Haskus.Utils.Variant.VEither
   )
where

import Haskus.Utils.Monad
import Haskus.Utils.Types
import Haskus.Utils.Variant.VEither

import Control.Monad.Catch
import Control.Monad.Reader.Class
#if MIN_VERSION_base(4,12,0) && !MIN_VERSION_base(4,13,0)
import qualified Control.Monad.Fail
import           Control.Monad.Fail ( MonadFail )
#endif

newtype Excepts es m a = Excepts (m (VEither es a))

deriving instance Show (m (VEither es a)) => Show (Excepts es m a)

-- | Run an Excepts
runE :: forall es a m.
   Excepts es m a -> m (VEither es a)
{-# INLINABLE runE #-}
runE (Excepts m) = m

-- | Run an Excepts, discard the result value
runE_ :: forall es a m.
   Functor m => Excepts es m a -> m ()
{-# INLINABLE runE_ #-}
runE_ m = void (runE m)

injectExcepts :: forall es a m.
   Monad m => Excepts es m a -> Excepts es m (VEither es a)
{-# INLINABLE injectExcepts #-}
injectExcepts (Excepts m) = lift m

withExcepts_ :: Monad m => (VEither es a -> m ()) -> Excepts es m a -> Excepts es m a
{-# INLINABLE withExcepts_ #-}
withExcepts_ f (Excepts m) = Excepts $ do
   v <- m
   f v
   return v

withExcepts :: Monad m => (VEither es a -> m b) -> Excepts es m a -> Excepts es m b
{-# INLINABLE withExcepts #-}
withExcepts f (Excepts m) = Excepts $ do
   v <- m
   VRight <$> f v

-- | Convert a flow without error into a value
evalE :: Monad m => Excepts '[] m a -> m a
{-# INLINABLE evalE #-}
evalE v = veitherToValue <$> runE v

mapExcepts :: (m (VEither es a) -> n (VEither es' b)) -> Excepts es m a -> Excepts es' n b
{-# INLINABLE mapExcepts #-}
mapExcepts f = Excepts . f . runE

-- | Lift a Excepts into another
liftE :: forall es' es a m.
   ( Monad m
   , VEitherLift es es'
   ) => Excepts es m a -> Excepts es' m a
{-# INLINABLE liftE #-}
liftE = mapExcepts (liftM veitherLift)

-- | Append errors to an Excepts
appendE :: forall ns es a m.
   ( Monad m
   ) => Excepts es m a -> Excepts (Concat es ns) m a
{-# INLINABLE appendE #-}
appendE = mapExcepts (liftM (veitherAppend @ns))

-- | Prepend errors to an Excepts
prependE :: forall ns es a m.
   ( Monad m
   , KnownNat (Length ns)
   ) => Excepts es m a -> Excepts (Concat ns es) m a
{-# INLINABLE prependE #-}
prependE = mapExcepts (liftM (veitherPrepend @ns))

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
      case f of
        VLeft e -> return (VLeft e)
        VRight k -> do
          a <- ma
          case a of
            VLeft e -> return (VLeft e)
            VRight x -> return (VRight (k x))

    {-# INLINABLE (*>) #-}
    m *> k = m >>= \_ -> k

instance (Monad m) => Monad (Excepts es m) where
    {-# INLINABLE (>>=) #-}
    m >>= k = Excepts $ do
        a <- runE m
        case a of
            VLeft es -> return (VLeft es)
            VRight x -> runE (k x)

#if MIN_VERSION_base(4,12,0)
instance (MonadFail m) => MonadFail (Excepts es m) where
#endif
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
   catch (Excepts m) f = Excepts $ catch m (runE . f)

instance MonadMask m => MonadMask (Excepts e m) where
   mask f = Excepts $ mask $ \u -> runE $ f (q u)
      where
         q :: (m (VEither e a) -> m (VEither e a)) -> Excepts e m a -> Excepts e m a
         q u (Excepts b) = Excepts (u b)

   uninterruptibleMask f = Excepts $ uninterruptibleMask $ \u -> runE $ f (q u)
      where
         q :: (m (VEither e a) -> m (VEither e a)) -> Excepts e m a -> Excepts e m a
         q u (Excepts b) = Excepts (u b)

   generalBracket acquire release use = Excepts $ do
      (eb, ec) <- generalBracket
         (runE acquire)
         (\eresource exitCase -> case eresource of
            VLeft e -> return (VLeft e) -- nothing to release, acquire didn't succeed
            VRight resource -> case exitCase of
               ExitCaseSuccess (VRight b) -> runE (release resource (ExitCaseSuccess b))
               ExitCaseException e        -> runE (release resource (ExitCaseException e))
               _                          -> runE (release resource ExitCaseAbort))
         (veitherCont (return . VLeft) (runE . use))
      runE $ do
         -- The order in which we perform those two 'Excepts' effects determines
         -- which error will win if they are both erroring. We want the error from
         -- 'release' to win.
         c <- Excepts (return ec)
         b <- Excepts (return eb)
         return (b, c)

instance MonadReader r m => MonadReader r (Excepts e m) where
  ask    = lift ask
  local  = mapExcepts . local
  reader = lift . reader


-- | Signal an exception value @e@.
throwE :: forall e es a m. (Monad m, e :< es) => e -> Excepts es m a
{-# INLINABLE throwE #-}
throwE = Excepts . pure . VLeft . V

-- | Throw some exception
throwSomeE :: forall es' es a m. (Monad m, LiftVariant es' es) => V es' -> Excepts es m a
{-# INLINABLE throwSomeE #-}
throwSomeE = Excepts . pure . VLeft . liftVariant

-- | Signal an exception value @e@.
failureE :: forall e a m. Monad m => e -> Excepts '[e] m a
{-# INLINABLE failureE #-}
failureE = throwE

-- | Signal a success
successE :: forall a m. Monad m => a -> Excepts '[] m a
{-# INLINABLE successE #-}
successE = pure

-- | Handle an exception. Lift both normal and exceptional flows into the result
-- flow
catchE :: forall e es' es'' es a m.
   ( Monad m
   , e :< es
   , LiftVariant (Remove e es) es'
   , LiftVariant es'' es'
   ) => (e -> Excepts es'' m a) -> Excepts es m a -> Excepts es' m a
{-# INLINABLE catchE #-}
catchE = catchLiftBoth

-- | Handle an exception. Lift both normal and exceptional flows into the result
-- flow
catchLiftBoth :: forall e es' es'' es a m.
   ( Monad m
   , e :< es
   , LiftVariant (Remove e es) es'
   , LiftVariant es'' es'
   ) => (e -> Excepts es'' m a) -> Excepts es m a -> Excepts es' m a
{-# INLINABLE catchLiftBoth #-}
catchLiftBoth h m = Excepts $ do
   a <- runE m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> runE (liftE (h l))
         Left rs -> return (VLeft (liftVariant rs))

-- | Handle an exception. Assume it is in the first position
catchRemove :: forall e es a m.
   ( Monad m
   ) => (e -> Excepts es m a) -> Excepts (e ': es) m a -> Excepts es m a
{-# INLINABLE catchRemove #-}
catchRemove h m = Excepts $ do
   a <- runE m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariantHead ls of
         Right l -> runE (h l)
         Left rs -> return (VLeft rs)

-- | Handle an exception. Lift the remaining errors into the resulting flow
catchLiftLeft :: forall e es es' a m.
   ( Monad m
   , e :< es
   , LiftVariant (Remove e es) es'
   ) => (e -> Excepts es' m a) -> Excepts es m a -> Excepts es' m a
{-# INLINABLE catchLiftLeft #-}
catchLiftLeft h m = Excepts $ do
   a <- runE m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> runE (h l)
         Left rs -> return (VLeft (liftVariant rs))

-- | Handle an exception. Lift the handler into the resulting flow
catchLiftRight :: forall e es es' a m.
   ( Monad m
   , e :< es
   , LiftVariant es' (Remove e es)
   ) => (e -> Excepts es' m a) -> Excepts es m a -> Excepts (Remove e es) m a
{-# INLINABLE catchLiftRight #-}
catchLiftRight h m = Excepts $ do
   a <- runE m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> runE (liftE (h l))
         Left rs -> return (VLeft rs)

-- | Do something in case of error
catchAllE :: Monad m => (V es -> Excepts es' m a) -> Excepts es m a -> Excepts es' m a
{-# INLINABLE catchAllE #-}
catchAllE h m = Excepts $ do
   a <- runE m
   case a of
      VRight x  -> return (VRight x)
      VLeft xs  -> runE (h xs)

-- | Evaluate a Excepts. Use the provided function to handle error cases.
catchEvalE :: Monad m => (V es -> m a) -> Excepts es m a -> m a
{-# INLINABLE catchEvalE #-}
catchEvalE h m = do
   a <- runE m
   case a of
      VRight x  -> return x
      VLeft xs  -> h xs

-- | Catch and die in case of error
catchDieE :: (e :< es, Monad m) => (e -> m ()) -> Excepts es m a -> Excepts (Remove e es) m a
{-# INLINABLE catchDieE #-}
catchDieE h m = Excepts $ do
   a <- runE m
   case a of
      VRight r -> return (VRight r)
      VLeft  ls -> case popVariant ls of
         Right l -> h l >> error "catchDieE"
         Left rs -> return (VLeft rs)

-- | Do something in case of error
onE_ :: Monad m => m () -> Excepts es m a -> Excepts es m a
{-# INLINABLE onE_ #-}
onE_ h m = Excepts $ do
   a <- runE m
   case a of
      VRight _ -> return a
      VLeft _  -> h >> return a

-- | Do something in case of error
onE :: Monad m => (V es -> m ()) -> Excepts es m a -> Excepts es m a
{-# INLINABLE onE #-}
onE h m = Excepts $ do
   a <- runE m
   case a of
      VRight _  -> return a
      VLeft es  -> h es >> return a

-- | Finally for Excepts
finallyE :: Monad m => m () -> Excepts es m a -> Excepts es m a
{-# INLINABLE finallyE #-}
finallyE h m = Excepts $ do
   a <- runE m
   h
   return a

-- | Convert a Variant into a Excepts
variantToExcepts :: Monad m => V (a ': es) -> Excepts es m a
{-# INLINABLE variantToExcepts #-}
variantToExcepts v = Excepts (return (veitherFromVariant v))

-- | Convert a VEither into a Excepts
veitherToExcepts :: Monad m => VEither es a -> Excepts es m a
{-# INLINABLE veitherToExcepts #-}
veitherToExcepts v = Excepts (return v)

instance MonadInIO m => MonadInIO (Excepts es m) where
   {-# INLINABLE liftWith #-}
   liftWith wth f =
      Excepts $ liftWith wth (\a -> runE (f a))

   {-# INLINABLE liftWith2 #-}
   liftWith2 wth f =
      Excepts $ liftWith2 wth (\a b -> runE (f a b))

-- | Product of the execution of two Excepts
--
-- You can use a generic monad combinator such as
-- `Control.Concurrent.Async.concurrently` (in "async" package) to get
-- concurrent execution.
--
-- >> concurrentE = runBothE concurrently
runBothE ::
   ( KnownNat (Length (b:e2))
   , Monad m
   ) => (forall x y. m x -> m y -> m (x,y)) -> Excepts e1 m a -> Excepts e2 m b -> Excepts (Tail (Product (a:e1) (b:e2))) m (a,b)
runBothE exec f g = Excepts do
   (v1,v2) <- exec (runE f) (runE g)
   pure (veitherProduct v1 v2)

-- | Product of the sequential execution of two Excepts
--
-- The second one is run even if the first one failed!
sequenceE ::
   ( KnownNat (Length (b:e2))
   , Monad m
   ) => Excepts e1 m a -> Excepts e2 m b -> Excepts (Tail (Product (a:e1) (b:e2))) m (a,b)
sequenceE = runBothE exec
   where
      exec f g = do
         v1 <- f
         v2 <- g
         pure (v1,v2)
