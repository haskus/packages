{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
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
   , mapFlowT
   , liftFlowT
   , success
   , throwE
   , catchE
   -- * Reexport
   , module Haskus.Utils.Variant
   )
where

import Haskus.Utils.Variant
import Data.Functor.Identity

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad

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


-- | Success value
success :: Monad m => a -> FlowT '[] m a
success = pure

-- | Signal an exception value @e@.
throwE :: (Monad m, e :< es) => e -> FlowT es m a
{-# INLINE throwE #-}
throwE = FlowT . return . toVariantTail . V

-- | Handle an exception.
catchE :: forall e es' es'' es a m.
   ( Monad m
   , e :< es
   , LiftVariant (Remove e es) es'
   , LiftVariant es'' es'
   ) =>
    FlowT es m a -> (e -> FlowT es'' m a) -> FlowT es' m a
{-# INLINE catchE #-}
m `catchE` h = FlowT $ do
   a <- runFlowT m
   case popVariantHead a of
      Right r -> return (toVariantHead r)
      Left  ls -> case popVariant ls of
         Right l -> runFlowT (liftFlowT (h l))
         Left rs -> return (toVariantTail (liftVariant rs))
