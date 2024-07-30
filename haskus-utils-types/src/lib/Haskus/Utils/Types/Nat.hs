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
   , NatEq
   , NatNotEq
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
-- >>> :seti -XDataKinds
-- >>> :seti -XTypeApplications
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeFamilies
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
type family NatEq a b :: Nat where
   NatEq a a = 1
   NatEq a b = 0

-- | Type inequality to Nat
type family NatNotEq a b :: Nat where
   NatNotEq a a = 0
   NatNotEq a b = 1

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
-- >>> natValue' @(NatBitCount 5)
-- 3
--
-- >>> natValue' @(NatBitCount 15)
-- 4
--
-- >>> natValue' @(NatBitCount 16)
-- 5
--
type family NatBitCount (n :: Nat) :: Nat where
   NatBitCount 0 = 1
   NatBitCount n = NatBitCount' (n+1) (Log2 (n+1))

type family NatBitCount' v log2 where
   NatBitCount' v log2 = log2 + NatNotEq v (2^log2)

-- | Return 1 if 0, and 0 otherwise
type family IsZero (n :: Nat) :: Nat where
   IsZero 0 = 1
   IsZero _ = 0

-- | Return 0 if 0, and 1 otherwise
type family IsNotZero (n :: Nat) :: Nat where
   IsNotZero 0 = 0
   IsNotZero _ = 1
