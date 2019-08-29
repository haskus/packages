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
   )
where

import Prelude hiding (Integer)
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
