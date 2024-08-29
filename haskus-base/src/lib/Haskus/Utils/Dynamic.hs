{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Haskus.Utils.Dynamic
   ( -- * Dynamic
     module Data.Dynamic
   -- * Dynamic with equality
   , DynEq (..)
   , toDynEq
   , fromDynEq
   , fromDynEqMaybe
   )
where

import Data.Dynamic
import Type.Reflection

-- | Dynamic type with Eq and Ord instance
--
-- Can be used as Map keys for instance
data DynEq where
   DynEq :: forall a. (Eq a, Ord a) => TypeRep a -> a -> DynEq

instance Eq DynEq where
   (DynEq tra a) == (DynEq trb b) = case tra `eqTypeRep` trb of
      Nothing    -> False
      Just HRefl -> a == b

instance Ord DynEq where
   compare (DynEq tra a) (DynEq trb b) = case tra `eqTypeRep` trb of
      Nothing    -> compare (SomeTypeRep tra) (SomeTypeRep trb)
      Just HRefl -> compare a b

-- | Create a DynEq value
--
-- >>> toDynEq (10 :: Int) == toDynEq (12 :: Int)
-- False
-- >>> toDynEq (10 :: Int) <= toDynEq (12 :: Int)
-- True
-- >>> toDynEq (10 :: Int) /= toDynEq "Test"
-- True
toDynEq :: (Typeable a, Eq a, Ord a) => a -> DynEq
toDynEq a = DynEq typeRep a

-- | Get a value from a DynEq or the default one if the type doesn't match
fromDynEq :: Typeable a => DynEq -> a -> a
fromDynEq (DynEq tr a) def = case tr `eqTypeRep` typeOf def of
   Nothing    -> def
   Just HRefl -> a

-- | Get a value from a DynEq if the type matches
fromDynEqMaybe :: forall a. Typeable a => DynEq -> Maybe a
fromDynEqMaybe (DynEq tr a) = case tr `eqTypeRep` (typeRep :: TypeRep a) of
   Nothing    -> Nothing
   Just HRefl -> Just a
