-- | Control-flow
module Haskus.Utils.Flow
   ( MonadIO (..)
   , MonadInIO (..)
   -- * Basic operators
   , (>.>)
   , (<.<)
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

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies

-- | Compose functions
--
-- >>> (+1) >.> (*7) <| 1
-- 14
(>.>) :: (a -> b) -> (b -> c) -> a -> c
f >.> g = \x -> g (f x)

infixl 9 >.>

-- | Compose functions
--
-- >>> (+1) <.< (*7) <| 1
-- 8
(<.<) :: (b -> c) -> (a -> b) -> a -> c
f <.< g = \x -> f (g x)

infixr 9 <.<


-- | Apply a function
--
-- >>> 5 |> (*2)
-- 10
(|>) :: a -> (a -> b) -> b
{-# INLINABLE (|>) #-}
x |> f = f x

infixl 0 |>

-- | Apply a function
--
-- >>> (*2) <| 5
-- 10
(<|) :: (a -> b) -> a -> b
{-# INLINABLE (<|) #-}
f <| x = f x

infixr 0 <|

-- | Apply a function in a Functor
--
-- >>> Just 5 ||> (*2)
-- Just 10
(||>) :: Functor f => f a -> (a -> b) -> f b
{-# INLINABLE (||>) #-}
x ||> f = fmap f x

infixl 0 ||>

-- | Apply a function in a Functor
--
-- >>> (*2) <|| Just 5
-- Just 10
(<||) :: Functor f => (a -> b) -> f a -> f b
{-# INLINABLE (<||) #-}
f <|| x = fmap f x

infixr 0 <||

-- | Apply a function in a Functor
--
-- >>> Just [5] |||> (*2)
-- Just [10]
(|||>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
{-# INLINABLE (|||>) #-}
x |||> f = fmap (fmap f) x

infixl 0 |||>

-- | Apply a function in a Functor
--
-- >>> (*2) <||| Just [5]
-- Just [10]
--
(<|||) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
{-# INLINABLE (<|||) #-}
f <||| x = fmap (fmap f) x

infixr 0 <|||

-- | Composition of catMaybes and forM
-- 
-- >>> let f x = if x > 3 then putStrLn "OK" >> return (Just x) else return Nothing
-- >>> forMaybeM [0..5] f
-- OK
-- OK
-- [4,5]
forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM xs f = catMaybes <|| forM xs f
