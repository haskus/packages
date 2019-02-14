{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE CPP #-}

-- We need this otherwise GHC chokes on the export of
-- "type (*)"
#if MIN_VERSION_GLASGOW_HASKELL (8,6,0,0)
{-# LANGUAGE NoStarIsType #-}
#endif

-- | Type-level Nat
module Haskus.Utils.Types.Nat
   ( Nat
   , natValue
   , natValue'
   , KnownNat
   , SomeNat (..)
   , someNatVal
   , sameNat
   -- * Comparisons
   , CmpNat
   , type (<=?)
   , type (<=)
   , Same
   , Max
   , Min
   , IsZero
   , IsNotZero
   -- * Operations
   , type (+)
   , type (-)
   , type (*)
   , type (^)
   , Mod
   , Log2
   , Div
   -- * Helpers
   , NatBitCount
   )
where

import GHC.TypeNats
import Haskus.Utils.Types.Bool
import Data.Proxy

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> import Haskus.Utils.Types

-- | Get a Nat value
natValue :: forall (n :: Nat) a. (KnownNat n, Num a) => a
{-# INLINABLE natValue #-}
natValue = fromIntegral (natVal (Proxy :: Proxy n))

-- | Get a Nat value as a Word
natValue' :: forall (n :: Nat). KnownNat n => Word
{-# INLINABLE natValue' #-}
natValue' = natValue @n


-- | Type equality to Nat
type family Same a b :: Nat where
   Same a a = 1
   Same a b = 0

-- | Max of two naturals
type family Max (a :: Nat) (b :: Nat) where
   Max a b = If (a <=? b) b a

-- | Min of two naturals
type family Min (a :: Nat) (b :: Nat) where
   Min a b = If (a <=? b) a b

-- | Number of bits (>= 1) required to store a Nat value
--
-- >>> natValue' @(NatBitCount 0)
-- 1
--
-- >>> natValue' @(NatBitCount 1)
-- 1
--
-- >>> natValue' @(NatBitCount 2)
-- 2
--
-- >>> natValue' @(NatBitCount 15)
-- 4
--
-- >>> natValue' @(NatBitCount 16)
-- 5
--
type family NatBitCount (n :: Nat) :: Nat where
   NatBitCount 0 = 1
   NatBitCount n = Log2 (n+1) + Mod (n+1) 2

-- | Return 1 if 0, and 0 otherwise
type family IsZero (n :: Nat) :: Nat where
   IsZero 0 = 1
   IsZero _ = 0

-- | Return 0 if 0, and 1 otherwise
type family IsNotZero (n :: Nat) :: Nat where
   IsNotZero 0 = 0
   IsNotZero _ = 1
