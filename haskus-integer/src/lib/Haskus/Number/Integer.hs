{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Multi-precision Integer
module Haskus.Number.Integer
   ( Integer (..)
   , integerEq
   , integerCompare
   , integerFromInt#
   , integerFromInt
   , integerFromInteger
   , integerAbs
   )
where

import Prelude hiding (Integer)
import qualified Prelude as P
import Haskus.Number.BigNat
import GHC.Exts

-- | Integer
--
-- Invariant: small values (and 0) use ISmall
data Integer
    = ISmall !Int#
    | IPos   {-# UNPACK #-} !BigNat
    | INeg   {-# UNPACK #-} !BigNat

instance Eq Integer where
   (==) = integerEq

instance Ord Integer where
   compare = integerCompare

instance Num Integer where
   fromInteger = integerFromInteger
   abs         = integerAbs


-- | Eq for Integer
integerEq :: Integer -> Integer -> Bool
integerEq (ISmall i1) (ISmall i2) = isTrue# (i1 ==# i2)
integerEq (IPos n1)   (IPos n2)   = bigNatEq n1 n2
integerEq (INeg n1)   (INeg n2)   = bigNatEq n1 n2
integerEq _           _           = False

-- | Compare for Integer
integerCompare :: Integer -> Integer -> Ordering
integerCompare (ISmall a) (ISmall b) = compare (I# a) (I# b)
integerCompare (IPos a)   (IPos b)   = bigNatCompare a b
integerCompare (INeg a)   (INeg b)   = bigNatCompare b a
integerCompare (IPos _)   (INeg _)   = GT
integerCompare (INeg _)   (IPos _)   = LT
integerCompare (ISmall _) (IPos _)   = LT
integerCompare (ISmall _) (INeg _)   = GT
integerCompare (IPos _)   (ISmall _) = GT
integerCompare (INeg _)   (ISmall _) = LT

-- | Create an Integer from an Int#
integerFromInt# :: Int# -> Integer
integerFromInt# i = ISmall i

-- | Create an Integer from an Int
integerFromInt :: Int -> Integer
integerFromInt (I# i) = ISmall i

-- | Create an Integer from a prelude Integer
integerFromInteger :: P.Integer -> Integer
integerFromInteger x
   | x < fromIntegral (minBound :: Int) = INeg (bigNatFromInteger (abs x))
   | x > fromIntegral (maxBound :: Int) = IPos (bigNatFromInteger x)
   | otherwise                          = ISmall i
      where
         !(I# i) = fromIntegral x

-- | Abs for Integer
integerAbs :: Integer -> Integer
integerAbs a@(ISmall x)
   | isTrue# (x >=# 0#) = a
   | I# x /= minBound   = ISmall (negateInt# x)
   | otherwise          = IPos (bigNatFromWord (fromIntegral (minBound :: Int)))
integerAbs a@(IPos _)   = a
integerAbs   (INeg x)   = IPos x
