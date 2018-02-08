{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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
   , PositEncoding (..)
   , PositFields (..)
   , positEncoding
   , positFields
   , positToRational
   , positFromRational
   , positApproxFactor
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Bits
import Haskus.Utils.Types
import Haskus.Utils.Tuple
import Haskus.Utils.Flow

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

data PositEncoding
   = PositInfinity
   | PositZero
   | PositEncoding PositFields
   deriving (Show)

positEncoding :: forall n es.
   ( Bits (IntAtLeast n)
   , Ord (IntAtLeast n)
   , Num (IntAtLeast n)
   , KnownNat n
   , KnownNat es
   , Integral (IntAtLeast n)
   ) => Posit n es -> PositEncoding
positEncoding p = case positKind p of
   SomePosit Zero        -> PositZero
   SomePosit Infinity    -> PositInfinity
   SomePosit v@(Value _) -> PositEncoding (positFields v)

-- | Decode posit fields
positFields :: forall n es.
   ( Bits (IntAtLeast n)
   , Ord (IntAtLeast n)
   , Num (IntAtLeast n)
   , KnownNat n
   , KnownNat es
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

      expo = fromIntegral (maskLeastBits es (v `shiftR` fs))
      frac = fromIntegral (maskLeastBits fs v)


-- | Convert a Posit into a Rational
positToRational :: forall n es.
   ( KnownNat n
   , KnownNat es
   , Eq (IntAtLeast n)
   , Bits (IntAtLeast n)
   , Integral (IntAtLeast n)
   ) => Posit n es -> Rational
positToRational p
   | isZero p     = 0 Ratio.:% 1
   | isInfinity p = Ratio.infinity
   | otherwise    = (fromIntegral useed ^^ r) * (2 ^^ e) * (1 + (f % fd))
      where
         fields = positFields (Value p)
         r      = positRegime fields
         e      = positExponent fields
         f      = fromIntegral (positFraction fields)
         fd     = 1 `shiftL` positFractionBitCount fields
         useed  = 1 `shiftL` (1 `shiftL` natValue @es) :: Integer -- 2^(2^es)

-- | Convert a rational into the approximate Posit
positFromRational :: forall p n es.
   ( Posit n es ~ p
   , Num (IntAtLeast n)
   , Bits (IntAtLeast n)
   , KnownNat es
   , KnownNat n
   ) => Rational -> Posit n es
positFromRational x = if
      | x == 0              -> Posit 0
      | x == Ratio.infinity -> Posit (bit (natValue @n - 1))
      | otherwise           -> computeRegime
                              |> uncurry3 computeExponent
                              |> uncurry3 computeFraction
                              |> uncurry  computeRounding
                              |> computeSign
                              |> Posit
   where
      useed = fromIntegral (1 `shiftL` (1 `shiftL` es) :: Integer) -- 2^(2^es)

      nbits = natValue @n
      es    = natValue @es

      -- compute regime bits of the posit, return (y,p,i)
      --    y: remaining value to convert, in [1,useed) if there are enough available bits
      --    p: current posit bits
      --    i: number of set bits in p
      computeRegime
         | absx >= 1 = regime111 absx 1 2
         | otherwise = regime000 absx 1
         where
            absx = abs x

            -- push regime bits 111..1110
            regime111 y p i
               | y >= useed && i < nbits = regime111 (y / useed) ((p `uncheckedShiftL` 1) .|. 1) (i+1)
               | otherwise               = (y, p `uncheckedShiftL` 1, i+1)

            -- push regime bits 000..0001 (or 000...00010 if the full word
            -- (including the sign bit) is set)
            regime000 y i
               | y < 1 && i <= nbits = regime000 (y*useed) (i+1)
               | i >= nbits          = (y,2,nbits+1)
               | otherwise           = (y,1,i+1)

      -- compute exponent bits; return (y,p,i)
      --    y: remaining value to convert, in [1,2) if there are enough available bits
      --    p: current posit bits
      --    i: number of set bits in p
      computeExponent
            | es == 0   = (,,)
            | otherwise = go (1 `shiftL` (es - 1))
         where
            go e y p i
               | i > nbits || e == 0 = (y,p,i)
               | y >= pow2e          = go (e `uncheckedShiftR` 1) (y / pow2e) ((p `uncheckedShiftL` 1) .|. 1) (i+1)
               | otherwise           = go (e `uncheckedShiftR` 1) y            (p `uncheckedShiftL` 1)        (i+1)
               where
                  pow2e = fromIntegral (1 `shiftL` e :: Integer)

      -- compute fraction bits; return (y,p)
      --    y: remaining value to convert
      --    p: current posit bits
      computeFraction y' = go (y'-1) -- subtract hidden bit. Now y is in [0,1) if there are enough available bits
         where
            go y p i
               | i > nbits = (y,p)
               | y <= 0    = (y, p `shiftL` (nbits+1-i)) -- add remaining 0s fraction bits
               | y2 > 1    = go (y2-1) (p `shiftL` 1 + 1) (i+1)
               | otherwise = go y2     (p `shiftL` 1)     (i+1)
               where
                  y2 = 2*y

      -- at this stage, p contains an additional fraction bit.
      -- We remove it and we round accordingly.
      computeRounding y p =
         let p' = p `uncheckedShiftR` 1
         in if | not (p `testBit` 0) -> p'                                     -- closer to lower value
               | y == 1 || y == 0    -> p' + (if p' `testBit` 0 then 1 else 0) -- tie goes to nearest even
               | otherwise           -> p' + 1                                 -- closer to upper value


      -- fixup the sign bit (and use 2's complement for the other bits)
      computeSign p
         | x < 0     = negate p
         | otherwise = p


-- | Factor of approximation for a given Rational when encoded as a Posit.
-- The closer to 1, the better.
--
-- Usage:
--
--    positApproxFactor @(Posit 8 2) (52 % 137)
--
positApproxFactor :: forall p n es.
   ( Posit n es ~ p
   , Num (IntAtLeast n)
   , Bits (IntAtLeast n)
   , Integral (IntAtLeast n)
   , KnownNat es
   , KnownNat n
   ) => Rational -> Double
positApproxFactor r = fromRational (r / (positToRational (positFromRational r ::  p)))
