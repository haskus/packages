-- | Control-flow
module Haskus.Utils.Flow
   ( MonadIO (..)
   , MonadInIO (..)
   -- * Basic operators
   , (|>)
   , (<|)
   , (||>)
   , (<||)
   , (|||>)
   , (<|||)
   -- * Monadic/applicative operators
   , when
   , unless
   , whenM
   , unlessM
   , ifM
   , guard
   , void
   , forever
   , foldM
   , foldM_
   , forM
   , forM_
   , forMaybeM
   , mapM
   , mapM_
   , sequence
   , replicateM
   , replicateM_
   , filterM
   , join
   , (<=<)
   , (>=>)
   , loopM
   , whileM
   -- * Variant based operators
   , module Haskus.Utils.Variant.Flow
   -- * Monad transformers
   , lift
   )
where

import Haskus.Utils.Variant
import Haskus.Utils.Variant.Flow
import Haskus.Utils.Monad
import Haskus.Utils.Maybe

import Control.Monad.Trans.Class (lift)

-- | Apply a function
(|>) :: a -> (a -> b) -> b
{-# INLINE (|>) #-}
x |> f = f x

infixl 0 |>

-- | Apply a function
(<|) :: (a -> b) -> a -> b
{-# INLINE (<|) #-}
f <| x = f x

infixr 0 <|

-- | Apply a function in a Functor
(||>) :: Functor f => f a -> (a -> b) -> f b
{-# INLINE (||>) #-}
x ||> f = fmap f x

infixl 0 ||>

-- | Apply a function in a Functor
(<||) :: Functor f => (a -> b) -> f a -> f b
{-# INLINE (<||) #-}
f <|| x = fmap f x

infixr 0 <||

-- | Apply a function in a Functor
(|||>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
{-# INLINE (|||>) #-}
x |||> f = fmap (fmap f) x

infixl 0 |||>

-- | Apply a function in a Functor
(<|||) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
{-# INLINE (<|||) #-}
f <||| x = fmap (fmap f) x

infixr 0 <|||

-- | Composition of catMaybes and forM
forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM xs f = catMaybes <|| forM xs f
