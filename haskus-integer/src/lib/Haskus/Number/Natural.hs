{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Multi-precision natural
--
-- Compared to BigNat, it uses a simple Word# for smaller values
module Haskus.Number.Natural
   ( Natural (..)
   , naturalFromWord
   , naturalFromWord#
   , naturalFromBigNat
   , naturalFromLimbsMS
   , naturalToInteger
   , naturalAdd
   , naturalSub
   , naturalMul
   , naturalQuotRem
   , naturalShiftL
   , naturalShiftR
   , naturalOr
   , naturalAnd
   , naturalXor
   , naturalTestBit
   , naturalPopCount
   )
where

#include "MachDeps.h"

import Haskus.Number.BigNat
import GHC.Exts
import Data.Bits
import Data.Maybe

-- | A Natural
--
-- Invariant: small values (and 0) use NSmall
data Natural
    = NSmall Word#
    | NBig   {-# UNPACK #-} !BigNat

-- Using an UnboxedSum seems to be slower for now
--
-- data Natural = Natural (# Word# | ByteArray# #)
-- 
-- {-# COMPLETE NSmall, NBig #-}
-- pattern NSmall :: Word# -> Natural
-- pattern NSmall w = Natural (# w | #)
-- 
-- pattern NBig :: BigNat -> Natural
-- pattern NBig bn <- Natural (# | (BigNat -> bn) #)
--    where
--       NBig (BigNat ba) = Natural (# | ba #)

instance Show Natural where
   show = show . naturalToInteger

instance Eq Natural where
   (==) = naturalEq

instance Num Natural where
   (+)         = naturalAdd
   (*)         = naturalMul
   x - y       = fromMaybe (error "Can't subtract these naturals") (naturalSub x y)
   abs         = id
   signum _    = NSmall 1##
   negate _    = error "Can't negate a Natural"
   fromInteger = naturalFromInteger

instance Bits Natural where
   shiftL w n     = naturalShiftL w (fromIntegral n)
   shiftR w n     = naturalShiftR w (fromIntegral n)
   isSigned _     = False
   zeroBits       = NSmall 0##
   complement     = error "Can't complement a Natural"
   bitSizeMaybe _ = Nothing
   bitSize        = error "Can't use bitsize on Natural"
   popCount       = fromIntegral . naturalPopCount
   rotate         = error "Can't rotate a Natural"
   bit i@(I# i#)
      | i < WORD_SIZE_IN_BITS = NSmall (1## `uncheckedShiftL#` i#)
      | otherwise             = NBig (bit i)
   (.|.)          = naturalOr
   (.&.)          = naturalAnd
   xor            = naturalXor
   testBit w n    = naturalTestBit w (fromIntegral n)

instance Integral Natural where
   toInteger   = naturalToInteger
   quotRem a b = fromJust (naturalQuotRem a b)

instance Real Natural where
   toRational n = toRational (naturalToInteger n)

instance Ord Natural where
   compare = naturalCompare

instance Enum Natural where
   toEnum   = naturalFromInteger . toInteger
   fromEnum = fromInteger . naturalToInteger

-- | Eq for Natural
naturalEq :: Natural -> Natural -> Bool
naturalEq (NSmall a) (NSmall b) = isTrue# (a `eqWord#` b)
naturalEq (NBig   a) (NBig   b) = bigNatEq a b
naturalEq (NSmall _) (NBig   _) = False
naturalEq (NBig   _) (NSmall _) = False

-- | Compare for Natural
naturalCompare :: Natural -> Natural -> Ordering
naturalCompare (NSmall a) (NSmall b) = compare (W# a) (W# b)
naturalCompare (NBig   a) (NBig   b) = bigNatCompare a b
naturalCompare (NSmall _) (NBig   _) = LT
naturalCompare (NBig   _) (NSmall _) = GT

-- | Add two naturals
naturalAdd :: Natural -> Natural -> Natural
naturalAdd (NSmall 0##) b            = b
naturalAdd a            (NSmall 0##) = a
naturalAdd (NSmall a)   (NSmall b)   = case plusWord2# a b of
   (# 0##,r0 #) -> NSmall r0
   (#  r1,r0 #) -> NBig (bigNatFrom2LimbsMS r1 r0)
naturalAdd (NSmall a)   (NBig b)    = NBig (bigNatAddWord# b a)
naturalAdd (NBig a)     (NSmall b)  = NBig (bigNatAddWord# a b)
naturalAdd (NBig a)     (NBig b)    = NBig (bigNatAdd a b)

-- | Sub two naturals
naturalSub :: Natural -> Natural -> Maybe Natural
naturalSub a          (NSmall 0##) = Just a
naturalSub (NSmall _) (NBig _)     = Nothing
naturalSub (NSmall a) (NSmall b)
   | isTrue# (a `ltWord#` b) = Nothing
   | otherwise               = Just (NSmall (a `minusWord#` b))
naturalSub (NBig a) (NBig b) = case bigNatSub a b of
   Nothing -> Nothing
   Just r  -> Just (naturalFromBigNat r)
-- TODO: implement faster bigNatMinusWord
naturalSub (NBig a) (NSmall b) = Just (naturalFromBigNat (a - bigNatFromWord# b))

-- | Multiply two naturals
naturalMul :: Natural -> Natural -> Natural
naturalMul a@(NSmall 0##) _              = a
naturalMul _              b@(NSmall 0##) = b
naturalMul (NSmall 1##)   b              = b
naturalMul a              (NSmall 1##)   = a
naturalMul (NSmall a)   (NSmall b)   = case timesWord2# a b of
   (# 0##,r0 #) -> NSmall r0
   (#  r1,r0 #) -> NBig (bigNatFrom2LimbsMS r1 r0)
naturalMul (NBig a)     (NBig b)    = NBig (bigNatMul a b)
naturalMul (NSmall a)   (NBig b)    = NBig (bigNatMulByWord# b a)
naturalMul (NBig a)     (NSmall b)  = NBig (bigNatMulByWord# a b)


-- | QuotRem two naturals
naturalQuotRem :: Natural -> Natural -> Maybe (Natural, Natural)
naturalQuotRem _              (NSmall 0##) = Nothing
naturalQuotRem a@(NSmall 0##) _            = Just (a, a)
naturalQuotRem a              (NSmall 1##) = Just (a, NSmall 0##)
naturalQuotRem (NSmall a)     (NSmall b  ) = case quotRemWord# a b of
   (# q, r #) -> Just (NSmall q, NSmall r)
naturalQuotRem a@(NSmall _)   (NBig _  )   = Just (NSmall 0##, a)
naturalQuotRem (NBig a)       (NBig b  )   = case bigNatQuotRem a b of
  Nothing    -> Nothing
  Just (q,r) -> Just (naturalFromBigNat q, naturalFromBigNat r)

-- TODO: implement faster bigNatQuotRemWord
naturalQuotRem (NBig a)    (NSmall b  )    = case bigNatQuotRem a (bigNatFromWord# b) of
  Nothing    -> Nothing
  Just (q,r) -> Just (naturalFromBigNat q, naturalFromBigNat r)

-- | Create a Natural from a Word#
naturalFromWord# :: Word# -> Natural
naturalFromWord# w = NSmall w

-- | Create a Natural from a Word
naturalFromWord :: Word -> Natural
naturalFromWord (W# w) = NSmall w

-- | Create a natural from a BigNat
naturalFromBigNat :: BigNat -> Natural
{-# INLINABLE naturalFromBigNat #-}
naturalFromBigNat !r@(BigNat ba) = case bigNatLimbCount# ba of
   0# -> NSmall 0##
   1# -> NSmall (indexWordArray# ba 0#)
   _  -> NBig r

-- | Convert a Natural into an Integer
naturalToInteger :: Natural -> Integer
{-# INLINABLE naturalToInteger #-}
naturalToInteger (NSmall w) = fromIntegral (W# w)
naturalToInteger (NBig w)   = bigNatToInteger w

-- | Convert an Integer into a Natural
naturalFromInteger :: Integer -> Natural
naturalFromInteger k
   | k < 0     = error "bigNatFromInteger: negative integer"
   | k <= fromIntegral (maxBound :: Word)
   , W# w <- fromIntegral k = NSmall w
   | otherwise = NBig (bigNatFromInteger k)

-- | Convert a list of Word into a Natural
naturalFromLimbsMS :: [Word] -> Natural
naturalFromLimbsMS xs = case dropWhile (== 0) xs of
   []     -> NSmall 0##
   [W# x] -> NSmall x
   ks     -> NBig (bigNatFromLimbsMS ks)

-- | Shift left
naturalShiftL :: Natural -> Word -> Natural
naturalShiftL a              0 = a
naturalShiftL (NBig b)       c = NBig (bigNatShiftL b c)
naturalShiftL a@(NSmall 0##) _ = a
naturalShiftL (NSmall b)     c@(W# c#)
   | isTrue# (clz# b `geWord#` c#) = NSmall (b `uncheckedShiftL#` i)
   | otherwise                     = NBig (bigNatShiftL (bigNatFromWord# b) c)
   where
      !i = word2Int# c#

-- | Shift right
naturalShiftR :: Natural -> Word -> Natural
naturalShiftR a              0  = a
naturalShiftR a@(NSmall 0##) _  = a
naturalShiftR (NSmall b)(W# c#) = NSmall (b `shiftRL#` (word2Int# c#))
naturalShiftR (NBig   b)     c  = naturalFromBigNat (bigNatShiftR b c)

-- | Pop count
naturalPopCount :: Natural -> Word
naturalPopCount (NSmall w) = fromIntegral (popCount (W# w))
naturalPopCount (NBig   w) = bigNatPopCount w

-- | Or two naturals
naturalOr :: Natural -> Natural -> Natural
naturalOr (NSmall a) (NSmall b) = NSmall (a `or#` b)
naturalOr (NBig a)   (NBig b)   = NBig (bigNatOr a b)
naturalOr (NBig a)   (NSmall b) = NBig (bigNatOr a (bigNatFromWord# b))
naturalOr (NSmall a) (NBig b)   = NBig (bigNatOr b (bigNatFromWord# a))

-- | And two naturals
naturalAnd :: Natural -> Natural -> Natural
naturalAnd (NSmall a) (NSmall b) = NSmall (a `and#` b)
naturalAnd (NBig a)   (NBig b)   = NBig (bigNatAnd a b)
naturalAnd (NBig a)   (NSmall b) = NBig (bigNatAnd a (bigNatFromWord# b))
naturalAnd (NSmall a) (NBig b)   = NBig (bigNatAnd b (bigNatFromWord# a))

-- | Xor two naturals
naturalXor :: Natural -> Natural -> Natural
naturalXor (NSmall a) (NSmall b) = NSmall (a `xor#` b)
naturalXor (NBig a)   (NBig b)   = NBig (bigNatXor a b)
naturalXor (NBig a)   (NSmall b) = NBig (bigNatXor a (bigNatFromWord# b))
naturalXor (NSmall a) (NBig b)   = NBig (bigNatXor b (bigNatFromWord# a))

-- | Natural test bit
naturalTestBit :: Natural -> Word -> Bool
naturalTestBit (NSmall a) k
   | k >= WORD_SIZE_IN_BITS = False
   | otherwise              = testBit (W# a) (fromIntegral k)
naturalTestBit (NBig a) k   = bigNatTestBit a k
