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
    | IBig   !Sign {-# UNPACK #-} !BigNat

data Sign
   = Pos
   | Neg
   deriving (Show,Eq,Ord)


instance Eq Integer where
   (==) = integerEq

instance Ord Integer where
   compare = integerCompare

instance Num Integer where
   fromInteger = integerFromInteger
   abs         = integerAbs


-- | Eq for Integer
integerEq :: Integer -> Integer -> Bool
integerEq (ISmall i1)  (ISmall i2) = isTrue# (i1 ==# i2)
integerEq (IBig s1 n1) (IBig s2 n2)
   = s1 == s2 && bigNatEq n1 n2
integerEq (IBig _ _)   (ISmall _) = False
integerEq (ISmall _)   (IBig _ _) = False

-- | Compare for Integer
integerCompare :: Integer -> Integer -> Ordering
integerCompare (ISmall a)     (ISmall b) = compare (I# a) (I# b)
integerCompare (IBig Pos a) (IBig Pos b) = bigNatCompare a b
integerCompare (IBig Neg a) (IBig Neg b) = bigNatCompare b a
integerCompare (IBig Pos _) (IBig Neg _) = GT
integerCompare (IBig Neg _) (IBig Pos _) = LT
integerCompare (ISmall _)   (IBig Pos _) = LT
integerCompare (ISmall _)   (IBig Neg _) = GT
integerCompare (IBig Pos _) (ISmall _)   = GT
integerCompare (IBig Neg _) (ISmall _)   = LT

-- | Create an Integer from an Int#
integerFromInt# :: Int# -> Integer
integerFromInt# i = ISmall i

-- | Create an Integer from an Int
integerFromInt :: Int -> Integer
integerFromInt (I# i) = ISmall i

-- | Create an Integer from a prelude Integer
integerFromInteger :: P.Integer -> Integer
integerFromInteger x
   | x < fromIntegral (minBound :: Int) = IBig Neg (bigNatFromInteger (abs x))
   | x > fromIntegral (maxBound :: Int) = IBig Pos (bigNatFromInteger x)
   | otherwise                          = ISmall i
      where
         !(I# i) = fromIntegral x

-- | Abs for Integer
integerAbs :: Integer -> Integer
integerAbs a@(ISmall x)
   | isTrue# (x >=# 0#) = a
   | I# x /= minBound   = ISmall (negateInt# x)
   | otherwise          = IBig Pos (bigNatFromWord (fromIntegral (minBound :: Int)))
integerAbs a@(IBig Pos _) = a
integerAbs   (IBig _   x) = IBig Pos x
