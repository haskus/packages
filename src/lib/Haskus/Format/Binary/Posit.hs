{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Posit (type III unum)
module Haskus.Format.Binary.Posit
   ( Posit (..)
   , PositKind (..)
   , PositK (..)
   , positKind
   , isZero
   , isInfinity
   , isPositive
   , isNegative
   , positAbs
   , positFields
   , positToRational
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Bits
import Haskus.Format.Binary.Bits.Order
import Haskus.Utils.Types

import Data.Ratio
import qualified GHC.Real as Ratio

newtype Posit (nbits :: Nat) (es :: Nat) = Posit (IntAtLeast nbits)

-- | Show posit
instance
   ( Bits (IntAtLeast n)
   , FiniteBits (IntAtLeast n)
   , Ord (IntAtLeast n)
   , Num (IntAtLeast n)
   , KnownNat n
   , KnownNat es
   , ReversableBits (IntAtLeast n)
   , Integral (IntAtLeast n)
   ) => Show (Posit n es)
   where
   show p = case positKind p of
      SomePosit Zero      -> "0"
      SomePosit Infinity  -> "Infinity"
      SomePosit (Value v) -> show (positToRational v)

data PositKind
   = ZeroK
   | InfinityK
   | NormalK
   deriving (Show,Eq)

-- | Kinded Posit
--
-- GADT that can be used to ensure at the type level that we deal with
-- non-infinite/non-zero Posit values
data PositK k nbits es where
   Zero     :: PositK 'ZeroK nbits es
   Infinity :: PositK 'InfinityK nbits es
   Value    :: Posit nbits es -> PositK 'NormalK nbits es

data SomePosit n es where
   SomePosit :: PositK k n es -> SomePosit n es

type PositValue n es = PositK 'NormalK n es

-- | Get the kind of the posit at the type level
positKind :: forall n es.
   ( Bits (IntAtLeast n)
   , KnownNat n
   , Eq (IntAtLeast n)
   ) => Posit n es -> SomePosit n es
positKind p
   | isZero p     = SomePosit Zero
   | isInfinity p = SomePosit Infinity
   | otherwise    = SomePosit (Value p)

-- | Check if a posit is zero
isZero :: forall n es.
   ( Bits (IntAtLeast n)
   , Eq (IntAtLeast n)
   , KnownNat n
   ) => Posit n es -> Bool
{-# INLINE isZero #-}
isZero (Posit i) = i == zeroBits

-- | Check if a posit is infinity
isInfinity :: forall n es.
   ( Bits (IntAtLeast n)
   , Eq (IntAtLeast n)
   , KnownNat n
   ) => Posit n es -> Bool
{-# INLINE isInfinity #-}
isInfinity (Posit i) = i == bit (natValue @n - 1)

-- | Check if a posit is positive
isPositive :: forall n es.
   ( Bits (IntAtLeast n)
   , Ord (IntAtLeast n)
   , KnownNat n
   ) => PositValue n es -> Bool
{-# INLINE isPositive #-}
isPositive (Value (Posit i)) = i > zeroBits

-- | Check if a posit is negative
isNegative :: forall n es.
   ( Bits (IntAtLeast n)
   , Ord (IntAtLeast n)
   , KnownNat n
   ) => PositValue n es -> Bool
{-# INLINE isNegative #-}
isNegative (Value (Posit i)) = i < zeroBits

-- | Posit absolute value
positAbs :: forall n es.
   ( Num (IntAtLeast n)
   , KnownNat n
   ) => PositValue n es -> PositValue n es
positAbs (Value (Posit i)) = Value (Posit (abs i))


data PositFields = PositFields
   { positNegative         :: Bool
   , positRegimeBitCount   :: Word
   , positExponentBitCount :: Word
   , positFractionBitCount :: Word
   , positRegime           :: Int
   , positExponent         :: Word
   , positFraction         :: Word
   }
   deriving (Show)

-- | Decode posit fields
positFields :: forall n es.
   ( Bits (IntAtLeast n)
   , FiniteBits (IntAtLeast n)
   , Ord (IntAtLeast n)
   , Num (IntAtLeast n)
   , KnownNat n
   , KnownNat es
   , ReversableBits (IntAtLeast n)
   , KnownNat (BitSize (IntAtLeast n))
   , Bitwise (IntAtLeast n)
   , Integral (IntAtLeast n)
   ) => PositValue n es -> PositFields
positFields p = PositFields
      { positNegative         = isNegative p
      , positRegimeBitCount   = rs
      , positExponentBitCount = es
      , positFractionBitCount = fs
      , positRegime           = regime
      , positExponent         = expo
      , positFraction         = frac
      }
   where
      -- get absolute value
      Value (Posit v) = positAbs p

      (negativeRegime,regimeLen) = 
         if v `testBit` (natValue @n - 2)
            -- regime has shape 111...[0|end of word], subtract 1 for sign bit
            then (False, countLeadingZeros (complement v `clearBit` (natValue @n - 1)) - 1)
            -- regime has shape 00000...[1|end of word], subtract 1 for sign bit
            else (True, countLeadingZeros v - 1)

      regime = if negativeRegime
         then negate (fromIntegral regimeLen)
         else fromIntegral regimeLen - 1 -- we encode the 0 regime

      -- length of regime bits (with stop bit)
      rs = min (natValue @n - 1) (regimeLen + 1)

      -- real exponent size (regime bits can reduce the size of the exponent)
      es = min (natValue @n - rs - 1) (natValue @es)

      -- fraction size
      fs = natValue @n - es - rs - 1

      expo = fromIntegral (getBitRange BB (1+rs) es v)
      frac = fromIntegral (maskLeastBits fs v)


-- | Convert a Posit into a Rational
positToRational :: forall n es.
   ( Bits (IntAtLeast n)
   , KnownNat n
   , KnownNat es
   , Eq (IntAtLeast n)
   , ReversableBits (IntAtLeast n)
   , Integral (IntAtLeast n)
   ) => Posit n es -> Rational
positToRational p
   | isZero p     = 0 Ratio.:% 1
   | isInfinity p = Ratio.infinity
   | otherwise    = (fromIntegral useed ^^ r) * (2 ^^ e) * (1 + (f % useed))
      where
         fields = positFields (Value p)
         r = positRegime fields
         e = positExponent fields
         f = fromIntegral (positFraction fields)
         useed = 1 `shiftL` (1 `shiftL` natValue @es) :: Integer
         -- useed = 2 ^ (2 ^ natValue @es)

