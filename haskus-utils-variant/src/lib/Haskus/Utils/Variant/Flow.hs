{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskus.Utils.Variant.Flow
   ( Flow
   , runFlow
   -- * FlowT
   , FlowT
   , runFlowT
   , evalFlowT
   , evalCatchFlowT
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
   , catchRemove
   , onError_
   -- * Reexport
   , module Haskus.Utils.Variant
   )
where

import Haskus.Utils.Monad
import Haskus.Utils.Variant
import Data.Functor.Identity

import Control.Monad.Catch

------------------------------------------------------------------------------
-- Flow
------------------------------------------------------------------------------
type Flow es     = FlowT es Identity

runFlow :: Flow es a -> V (a ': es)
{-# INLINE runFlow #-}
runFlow (FlowT m) = runIdentity m

------------------------------------------------------------------------------
-- FlowT
------------------------------------------------------------------------------
newtype FlowT es m a = FlowT (m (V (a ': es)))

deriving instance Show (m (V (a ': es))) => Show (FlowT es m a)

runFlowT :: FlowT es m a -> m (V (a ': es))
{-# INLINE runFlowT #-}
runFlowT (FlowT m) = m

-- | Convert a flow without error into a value
evalFlowT :: Monad m => FlowT '[] m a -> m a
{-# INLINE evalFlowT #-}
evalFlowT v = variantToValue <$> runFlowT v

mapFlowT :: (m (V (a ': es)) -> n (V (b ': es'))) -> FlowT es m a -> FlowT es' n b
{-# INLINE mapFlowT #-}
mapFlowT f m = FlowT $ f (runFlowT m)

-- | Lift a FlowT into another
liftFlowT :: (Monad m, LiftVariant es es') => FlowT es m a -> FlowT es' m a
{-# INLINE liftFlowT #-}
liftFlowT (FlowT m) = FlowT $ do
   a <- m
   return (mapVariantHeadTail id liftVariant a)

instance Functor m => Functor (FlowT es m) where
   {-# INLINE fmap #-}
   fmap f = FlowT . fmap (mapVariantHeadTail f id) . runFlowT

instance Foldable m => Foldable (FlowT es m) where
   {-# INLINE foldMap #-}
   foldMap f (FlowT m) = foldMap (variantHeadTail f (const mempty)) m

instance Traversable m => Traversable (FlowT es m) where
   {-# INLINE traverse #-}
   traverse f (FlowT m) =
      FlowT <$> traverse (variantHeadTail (fmap toVariantHead . f) (pure . toVariantTail)) m

instance (Functor m, Monad m) => Applicative (FlowT es m) where
    {-# INLINE pure #-}
    pure a = FlowT $ return (toVariantHead a)

    {-# INLINEABLE (<*>) #-}
    FlowT f <*> FlowT v = FlowT $ do
        mf <- f
        case popVariantHead mf of
            Left es -> return (toVariantTail es)
            Right k -> do
                mv <- v
                case popVariantHead mv of
                    Left es -> return (toVariantTail es)
                    Right x -> return (toVariantHead (k x))

    {-# INLINE (*>) #-}
    m *> k = m >>= \_ -> k

instance (Monad m) => Monad (FlowT es m) where
    {-# INLINE (>>=) #-}
    m >>= k = FlowT $ do
        a <- runFlowT m
        case popVariantHead a of
            Left es -> return (toVariantTail es)
            Right x -> runFlowT (k x)

    {-# INLINE fail #-}
    fail = FlowT . fail

instance MonadTrans (FlowT e) where
    {-# INLINE lift #-}
    lift = FlowT . liftM toVariantHead

instance (MonadIO m) => MonadIO (FlowT es m) where
    {-# INLINE liftIO #-}
    liftIO = lift . liftIO


-- | Throws exceptions into the base monad.
instance MonadThrow m => MonadThrow (FlowT e m) where
   {-# INLINE throwM #-}
   throwM = lift . throwM

-- | Catches exceptions from the base monad.
instance MonadCatch m => MonadCatch (FlowT e m) where
   catch (FlowT m) f = FlowT $ catch m (runFlowT . f)

instance MonadMask m => MonadMask (FlowT e m) where
   mask f = FlowT $ mask $ \u -> runFlowT $ f (q u)
      where
         q :: (m (V (a ': e)) -> m (V (a ': e))) -> FlowT e m a -> FlowT e m a
         q u (FlowT b) = FlowT (u b)

   uninterruptibleMask f = FlowT $ uninterruptibleMask $ \u -> runFlowT $ f (q u)
      where
         q :: (m (V (a ': e)) -> m (V (a ': e))) -> FlowT e m a -> FlowT e m a
         q u (FlowT b) = FlowT (u b)

   generalBracket acquire release use = FlowT $ do
      (eb, ec) <- generalBracket
         (runFlowT acquire)
         (\eresource exitCase -> case popVariantHead eresource of
            Left e -> return (toVariantTail e) -- nothing to release, acquire didn't succeed
            Right resource -> case exitCase of
               ExitCaseSuccess v
                  | Just b <- fromVariantAt @0 v -> runFlowT (release resource (ExitCaseSuccess b))
               ExitCaseException e               -> runFlowT (release resource (ExitCaseException e))
               _                                 -> runFlowT (release resource ExitCaseAbort))
         (variantHeadTail (runFlowT . use) (return . toVariantTail))
      return $ runFlow $ do
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
throwE :: (Monad m, e :< es) => e -> FlowT es m a
{-# INLINE throwE #-}
throwE = FlowT . return . toVariantTail . V

-- | Signal an exception value @e@.
failure :: Monad m => e -> FlowT '[e] m a
{-# INLINE failure #-}
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
{-# INLINE catchE #-}
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
{-# INLINE catchLiftBoth #-}
m `catchLiftBoth` h = FlowT $ do
   a <- runFlowT m
   case popVariantHead a of
      Right r -> return (toVariantHead r)
      Left  ls -> case popVariant ls of
         Right l -> runFlowT (liftFlowT (h l))
         Left rs -> return (toVariantTail (liftVariant rs))

-- | Handle an exception. Assume it is in the first position
catchRemove :: forall e es a m.
   ( Monad m
   ) =>
    FlowT (e ': es) m a -> (e -> FlowT es m a) -> FlowT es m a
{-# INLINE catchRemove #-}
m `catchRemove` h = FlowT $ do
   a <- runFlowT m
   case popVariantHead a of
      Right r -> return (toVariantHead r)
      Left  ls -> case popVariantHead ls of
         Right l -> runFlowT (h l)
         Left rs -> return (toVariantTail rs)

-- | Handle an exception. Lift the remaining errors into the resulting flow
catchLiftLeft :: forall e es es' a m.
   ( Monad m
   , e :< es
   , LiftVariant (Remove e es) es'
   ) =>
    FlowT es m a -> (e -> FlowT es' m a) -> FlowT es' m a
{-# INLINE catchLiftLeft #-}
m `catchLiftLeft` h = FlowT $ do
   a <- runFlowT m
   case popVariantHead a of
      Right r -> return (toVariantHead r)
      Left  ls -> case popVariant ls of
         Right l -> runFlowT (h l)
         Left rs -> return (toVariantTail (liftVariant rs))

-- | Handle an exception. Lift the handler into the resulting flow
catchLiftRight :: forall e es es' a m.
   ( Monad m
   , e :< es
   , LiftVariant es' (Remove e es)
   ) =>
    FlowT es m a -> (e -> FlowT es' m a) -> FlowT (Remove e es) m a
{-# INLINE catchLiftRight #-}
m `catchLiftRight` h = FlowT $ do
   a <- runFlowT m
   case popVariantHead a of
      Right r -> return (toVariantHead r)
      Left  ls -> case popVariant ls of
         Right l -> runFlowT (liftFlowT (h l))
         Left rs -> return (toVariantTail rs)

-- | Do something in case of error
catchAllE :: Monad m => FlowT es m a -> (V es -> FlowT es' m a) -> FlowT es' m a
{-# INLINE catchAllE #-}
m `catchAllE` h = FlowT $ do
   a <- runFlowT m
   case popVariantAt @0 a of
      Right x  -> return (toVariantHead x)
      Left xs  -> runFlowT (h xs)

-- | Evaluate a FlowT. Use the provided fucntion to handle error cases.
evalCatchFlowT :: Monad m => (V es -> m a) -> FlowT es m a -> m a
{-# INLINE evalCatchFlowT #-}
evalCatchFlowT h m = do
   a <- runFlowT m
   case popVariantAt @0 a of
      Right x  -> return x
      Left xs  -> h xs

-- | Catch and die in case of error
catchDie :: (e :< es, Monad m) => FlowT es m a -> (e -> m ()) -> FlowT (Remove e es) m a
{-# INLINE catchDie #-}
m `catchDie` h = FlowT $ do
   a <- runFlowT m
   case popVariantHead a of
      Right r -> return (toVariantHead r)
      Left  ls -> case popVariant ls of
         Right l -> h l >> error "catchDie"
         Left rs -> return (toVariantTail rs)

-- | Do something in case of error
onError_ :: Monad m => FlowT es m a -> m () -> FlowT es m a
{-# INLINE onError_ #-}
m `onError_` h = FlowT $ do
   a <- runFlowT m
   case fromVariantAt @0 a of
      Just _  -> return a
      Nothing -> h >> return a

-- | Convert a Variant into a FlowT
variantToFlowT :: Monad m => V (a ': es) -> FlowT es m a
variantToFlowT v = FlowT (return v)

instance MonadInIO m => MonadInIO (FlowT es m) where
   {-# INLINE liftWith #-}
   liftWith wth f =
      FlowT $ liftWith wth (\a -> runFlowT (f a))

   {-# INLINE liftWith2 #-}
   liftWith2 wth f =
      FlowT $ liftWith2 wth (\a b -> runFlowT (f a b))
