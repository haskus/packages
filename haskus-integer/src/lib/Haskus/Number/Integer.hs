{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

-- | Multi-precision Integer
module Haskus.Number.Integer
   ( Integer (..)
   , integerEq
   , integerCompare
   , integerFromInt#
   , integerFromInt
   , integerFromInteger
   , integerToInteger
   , integerAbs
   , integerNegate
   , integerCanon
   , integerAdd
   , integerAddInt#
   , integerSub
   , integerSubInt#
   , integerSignum
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

instance Show Integer where
   show = show . integerToInteger

instance Eq Integer where
   (==) = integerEq

instance Ord Integer where
   compare = integerCompare

instance Num Integer where
   fromInteger = integerFromInteger
   abs         = integerAbs
   negate      = integerNegate
   (+)         = integerAdd
   (-)         = integerSub
   signum      = integerSignum


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

-- | Create a prelude Integer from an Integer
integerToInteger :: Integer -> P.Integer
integerToInteger (ISmall i) = fromIntegral (I# i)
integerToInteger (IPos   n) = bigNatToInteger n
integerToInteger (INeg   n) = negate (bigNatToInteger n)

-- | Abs for Integer
integerAbs :: Integer -> Integer
integerAbs a@(ISmall x)
   | isTrue# (x >=# 0#) = a
   | I# x == minBound   = IPos (bigNatFromWord# (int2Word# x))
   | otherwise          = ISmall (negateInt# x)
integerAbs a@(IPos _)   = a
integerAbs   (INeg x)   = IPos x

-- | Negate for Integer
integerNegate :: Integer -> Integer
integerNegate (ISmall x)
   | I# x == minBound   = IPos (bigNatFromWord# (int2Word# x))
   | otherwise          = ISmall (negateInt# x)
integerNegate (IPos x)  = INeg x
integerNegate (INeg x)  = IPos x

-- | Convert an integer which may not be in canonical form into a valid integer
integerCanon :: Integer -> Integer
integerCanon a@(ISmall _) = a
integerCanon a@(IPos b)
   | bigNatLimbCount b == 0 = ISmall 0#
   | bigNatLimbCount b == 1
   , bigNatLimb b 0# <= fromIntegral (maxBound :: Int)
   , I# r <- fromIntegral (bigNatLimb b 0#)
   = ISmall r
   | otherwise = a
integerCanon a@(INeg b)
   | bigNatLimbCount b == 0 = ISmall 0#
   | bigNatLimbCount b == 1
   , bigNatLimb b 0# <= fromIntegral (minBound :: Int)
   , I# r <- fromIntegral (bigNatLimb b 0#)
   = ISmall (negateInt# r)
   | otherwise = a

-- | Add for Integer
integerAdd :: Integer -> Integer -> Integer
integerAdd x          (ISmall y)  = integerAddInt# x y
integerAdd (ISmall x) y           = integerAddInt# y x
integerAdd (IPos x)   (IPos y)    = IPos (bigNatAdd x y)
integerAdd (INeg x)   (INeg y)    = INeg (bigNatAdd x y)
integerAdd (IPos x)   (INeg y)    = case bigNatCompare x y of
   EQ -> ISmall 0#
   GT -> integerCanon (IPos (bigNatSub_nocheck x y))
   LT -> integerCanon (INeg (bigNatSub_nocheck y x))
integerAdd (INeg y)   (IPos x)    = case bigNatCompare x y of
   EQ -> ISmall 0#
   GT -> integerCanon (IPos (bigNatSub_nocheck x y))
   LT -> integerCanon (INeg (bigNatSub_nocheck y x))

-- | Subtract integers
integerSub :: Integer -> Integer -> Integer
integerSub x (ISmall y) = integerSubInt# x y
integerSub x (IPos y)   = integerAdd x (INeg y)
integerSub x (INeg y)   = integerAdd x (IPos y)

-- | Make a big Integer
--
-- Warning: the returned Integer is not a valid one!
-- This function allows the use of BigNat functions for small Integer
makeBigInteger# :: Int# -> Integer
makeBigInteger# x
   | isTrue# (x <# 0#)  = INeg (bigNatFromWord# (int2Word# (negateInt# x)))
   | otherwise          = IPos (bigNatFromWord# (int2Word# x))

-- | Integer add Int#
integerAddInt# :: Integer -> Int# -> Integer
integerAddInt# (ISmall x) y   = case addIntC# x y of
   (# s, 0# #) -> ISmall s
   _           -> integerAddInt# (makeBigInteger# x) y
integerAddInt# a        0# = a
integerAddInt# (IPos x) y
   | isTrue# (y ># 0#) = IPos (bigNatAddWord# x (int2Word# y))
   | I# y == minBound  = IPos (bigNatSubWord# x (int2Word# y))
   | otherwise         = integerCanon (IPos (bigNatSubWord# x (int2Word# (negateInt# y))))
integerAddInt# (INeg x) y
   | I# y == minBound  = INeg (bigNatAddWord# x (int2Word# y))
   | isTrue# (y <# 0#) = INeg (bigNatAddWord# x (int2Word# (negateInt# y)))
   | otherwise         = integerCanon (INeg (bigNatSubWord# x (int2Word# y)))

-- | Subtract an Int#
integerSubInt# :: Integer -> Int# -> Integer
integerSubInt# (ISmall x) y = case subIntC# x y of
   (# s, 0# #) -> ISmall s
   _           -> integerSubInt# (makeBigInteger# x) y
integerSubInt# a        0# = a
integerSubInt# (IPos x) y
   | isTrue# (y ># 0#) = integerCanon (IPos (bigNatSubWord# x (int2Word# y)))
   | I# y == minBound  = IPos (bigNatAddWord# x (int2Word# y))
   | otherwise         = IPos (bigNatAddWord# x (int2Word# (negateInt# y)))
integerSubInt# (INeg x) y
   | I# y == minBound  = integerCanon (INeg (bigNatSubWord# x (int2Word# y)))
   | isTrue# (y <# 0#) = integerCanon (INeg (bigNatSubWord# x (int2Word# (negateInt# y))))
   | otherwise         = INeg (bigNatAddWord# x (int2Word# y))

-- | Signum for Integer
integerSignum :: Integer -> Integer
integerSignum (ISmall 0#) = ISmall 0#
integerSignum (ISmall i)
   | isTrue# (i ># 0#) = ISmall 1#
   | otherwise         = ISmall -1#
integerSignum (IPos _) = ISmall 1#
integerSignum (INeg _) = ISmall -1#
