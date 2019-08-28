{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fobject-code #-}

-- | Multi-precision natural
module Haskus.Number.BigNat
   ( BigNat (..)
   , bigNatFromWord
   , bigNatFromLimbsMS
   , bigNatFromInteger
   , bigNatIsZero
   , bigNatIsOne
   , bigNatZero
   , bigNatShowHex
   , bigNatEq
   , bigNatAdd
   , bigNatSub
   , bigNatMul
   , bigNatQuotRem
   , bigNatCompare
   , bigNatOr
   , bigNatAnd
   , bigNatXor
   , bigNatPopCount
   , bigNatShiftR
   , bigNatShiftL
   , bigNatLimbCount
   -- * Primitives
   , bigNatFromWord#
   , bigNatFrom2LimbsMS
   , bigNatAddWord#
   , bigNatWithNormalized
   , bigNatLimbCount#
   , bigNatLimbCountMutable#
   , bigNatLimbsLS
   , bigNatLimbsMS
   , bigNatComparePrefix
   , add1by1_small#
   , add1by1_large#
   , add1by2_small#
   , add1by2_large#
   , add2by2_small#
   , sub2by1#
   , sub2by2#
   , sub3by1#
   , mul1by1_small#
   , mul1by1_large#
   , mul1by2#
   , div1by1#
   , div2by1_small#
   , div2by1_large#
   , div3by2_small#
   , div3by2_large#
   , div3by2#
   , divNby1#
   , div4by2_small#
   , div4by2_large#
   , sub#
   , cmpDropped#
   , shrinkMS
   , subAtInplace#
   , bigNatSub_nocheck
   , mostSignificantLimb#
   , bigNatQuotRem_normalized
   , checkDiv
   )
where

import GHC.Exts
import GHC.ST
import Data.Char
import Data.Bits
import Data.Maybe

#include "MachDeps.h"

#define WSBITS WORD_SIZE_IN_BITS
-- FIXME:
#define WS 8
#define WSSHIFT 3

-- | A BigNat
--
-- Stored as an array of Word limbs, lower limbs first, limbs use host
-- endianness.
--
-- Invariant:
--  - no empty high limb
--     ==> zero is zero size array
--     ==> canonical representation
data BigNat = BigNat ByteArray#

instance Show BigNat where
   show = show . bigNatToInteger

instance Eq BigNat where
   (==) = bigNatEq

instance Ord BigNat where
   compare = bigNatCompare

instance Num BigNat where
   (+)         = bigNatAdd
   (*)         = bigNatMul
   x - y       = fromMaybe (error "Can't subtract these bigNats") (bigNatSub x y)
   abs         = id
   signum _    = bigNatFromWord 1
   negate _    = error "Can't negate a BigNat"
   fromInteger = bigNatFromInteger

instance Bits BigNat where
   (.&.)          = bigNatAnd
   (.|.)          = bigNatOr
   xor            = bigNatXor
   complement     = error "Can't complement a BigNat"
   shiftL w n     = bigNatShiftL w (fromIntegral n)
   shiftR w n     = bigNatShiftR w (fromIntegral n)
   isSigned _     = False
   zeroBits       = bigNatZero
   bitSizeMaybe _ = Nothing
   popCount       = fromIntegral . bigNatPopCount
   bitSize        = error "Can't use bitsize on BigNat"
   bit i
      | i < WS    = bigNatFromWord (bit i)
      | otherwise = bigNatFromWord 1 `shiftL` i
   testBit w n    = bigNatTestBit w (fromIntegral n)
   rotate         = error "Can't rotate a BigNat"

instance Integral BigNat where
   toInteger   = bigNatToInteger
   quotRem a b = fromJust (bigNatQuotRem a b)

instance Real BigNat where
   toRational n = toRational (bigNatToInteger n)

instance Enum BigNat where
   toEnum   = bigNatFromInteger . toInteger
   fromEnum = fromInteger . bigNatToInteger

-- | Convert limb count into byte count
limbsToBytes# :: Int# -> Int#
limbsToBytes# i = i `iShiftL#` WSSHIFT#

-- | Convert byte count into limb count
bytesToLimbs# :: Int# -> Int#
bytesToLimbs# i = i `iShiftRL#` WSSHIFT#

-- | Count limbs
bigNatLimbCount :: BigNat -> Word
bigNatLimbCount (BigNat ba) = W# (int2Word# (bigNatLimbCount# ba))

-- | Count limbs
bigNatLimbCount# :: ByteArray# -> Int#
bigNatLimbCount# ba = bytesToLimbs# (sizeofByteArray# ba)

-- | Count limbs
bigNatLimbCountMutable# :: MutableByteArray# s-> State# s -> (# State# s, Int# #)
bigNatLimbCountMutable# mba s = case getSizeofMutableByteArray# mba s of
   (# s2, sz #) -> (# s2, bytesToLimbs# sz #)

-- | BigNat Zero
bigNatZero :: BigNat
bigNatZero = runST $ ST $ \s0 ->
   case newByteArray# 0# s0 of
      (# s1, mba #) -> case unsafeFreezeByteArray# mba s1 of
            (# s2, ba #) -> (# s2, BigNat ba #)

-- | Indicate if a bigNat is zero
bigNatIsZero :: BigNat -> Bool
{-# INLINABLE bigNatIsZero #-}
bigNatIsZero (BigNat ba) = isTrue# (bigNatLimbCount# ba ==# 0#)

-- | Indicate if a bigNat is one
bigNatIsOne :: BigNat -> Bool
bigNatIsOne n@(BigNat ba) = bigNatLimbCount n == 1 && isTrue# (indexWordArray# ba 0# `eqWord#` 1##)

-- | Create a BigNat from a Word
bigNatFromWord :: Word -> BigNat
bigNatFromWord (W# w) = bigNatFromWord# w

-- | Create a BigNat from a Word
bigNatFromWord# :: Word# -> BigNat
bigNatFromWord# 0## = bigNatZero
bigNatFromWord# w   = runST $ ST \s0 ->
   case newByteArray# WS# s0 of
      (# s1, mba #) -> case writeWordArray# mba 0# w s1 of
         s2 -> case unsafeFreezeByteArray# mba s2 of
            (# s3, ba #) -> (# s3, BigNat ba #)

-- | Convert an Integer into a BigNat
bigNatFromInteger :: Integer -> BigNat
bigNatFromInteger k
   | k < 0     = error "bigNatFromInteger: negative integer"
   | otherwise = go bigNatZero 0 k
   where
      -- FIXME: be clever and allocate log2(k)/WSBITS ByteArray directly. Then
      -- fill it.
      go c _ 0 = c
      go c s n = go (c `bigNatOr` bigNatShiftL (bigNatFromWord (fromInteger n)) s) (s + WSBITS) (n `shiftR` WSBITS)

-- | Convert a list of non-zero Words (most-significant first) into a BigNat
bigNatFromLimbsMS :: [Word] -> BigNat
bigNatFromLimbsMS = doit . dropWhile (== 0)
   where
   doit [] = bigNatZero
   doit xs = runST $ ST \s0 ->
      let !(I# sz)       = lxs * WS
          !lxs@(I# lxs#) = length xs
      in case newByteArray# sz s0 of
         (# s1, mba #) ->
            let
               go []        _ s = s
               go (W# w:ws) i s = case writeWordArray# mba i w s of
                  s' -> go ws (i -# 1#) s'
            in case go xs (lxs# -# 1#) s1 of
               s2 -> case unsafeFreezeByteArray# mba s2 of
                  (# s3, ba #) -> (# s3, BigNat ba #)

-- | Convert two Word# (MSL first) into a BigNat
bigNatFrom2LimbsMS :: Word# -> Word# -> BigNat
bigNatFrom2LimbsMS 0## 0## = bigNatZero
bigNatFrom2LimbsMS 0## n   = bigNatFromWord# n
bigNatFrom2LimbsMS w1 w2   = runST $ ST \s0 ->
   case newByteArray# (WS# +# WS#) s0 of
      (# s1, mba #) -> case writeWordArray# mba 0# w2 s1 of
         s2 -> case writeWordArray# mba 1# w1 s2 of
            s3 -> case unsafeFreezeByteArray# mba s3 of
               (# s4, ba #) -> (# s4, BigNat ba #)

-- | Convert a BigNat into an Integer
bigNatToInteger :: BigNat -> Integer
bigNatToInteger = go 0 . bigNatLimbsMS
   where
      go c []     = c
      go c (x:xs) = go (c `shiftL` WSBITS .|. toInteger x) xs

-- | Show a BigNat in hexadecimal
bigNatShowHex :: BigNat -> String
bigNatShowHex n
   | bigNatIsZero n = "0x0"
   | otherwise       = '0' : 'x' : add0 (fmap hex16 (dropWhile (==0) (concatMap limb4MS (bigNatLimbsMS n))))
   where
      add0 [] = ['0']
      add0 xs = xs
      hex16 x
         | x <= 9    = chr (48+fromIntegral x)
         | otherwise = chr (55+fromIntegral x)

      -- limbs in 4-bit chunk, most significant first
      limb4MS w = goLimbs4 (WS*8) w
      goLimbs4 0 _ = []
      goLimbs4 k x = (x `unsafeShiftR` (k-4)) .&. 0xF : goLimbs4 (k-4) x

-- | Get the most significant limb
mostSignificantLimb# :: ByteArray# -> Word#
mostSignificantLimb# ba = indexWordArray# ba (bytesToLimbs# (sizeofByteArray# ba) -# 1#)

-- Limbs: most significant first
bigNatLimbsMS :: BigNat -> [Word]
bigNatLimbsMS n@(BigNat ba)
   | bigNatIsZero n = []
   | otherwise       = goLimbs (fromIntegral (bigNatLimbCount n - 1))
   where
      goLimbs 0         = [W# (indexWordArray# ba 0#)]
      goLimbs i@(I# i#) = W# (indexWordArray# ba i#) : goLimbs (i-1)

-- Limbs: less significant first
bigNatLimbsLS :: BigNat -> [Word]
bigNatLimbsLS n@(BigNat ba)
   | bigNatIsZero n = []
   | otherwise       = goLimbs 0
   where
      lc = fromIntegral (bigNatLimbCount n)
      goLimbs i@(I# i#)
         | i == lc   = []
         | otherwise = W# (indexWordArray# ba i#) : goLimbs (i+1)

-- | Equality
bigNatEq :: BigNat -> BigNat -> Bool
bigNatEq n1 n2
   | bigNatLimbCount n1 /= bigNatLimbCount n2 = False
   | otherwise = all (uncurry (==)) (bigNatLimbsMS n1 `zip` bigNatLimbsMS n2)

-- | Compare
bigNatCompare :: BigNat -> BigNat -> Ordering
bigNatCompare a b
   | lcA > lcB = GT
   | lcA < lcB = LT
   | otherwise = go (bigNatLimbsMS a) (bigNatLimbsMS b)
      where
         lcA = bigNatLimbCount a
         lcB = bigNatLimbCount b
         go [] []         = EQ
         go ~(x:xs) ~(y:ys) = case compare x y of
            EQ -> go xs ys
            r  -> r

-- | Add two bigNats
bigNatAdd :: BigNat -> BigNat -> BigNat
bigNatAdd a@(BigNat lA) b@(BigNat lB)
   | bigNatIsZero a = b
   | bigNatIsZero b = a
   | otherwise       = runST $ ST \s0 ->
      case newByteArray# sz s0 of
         (# s1, mba #) -> case addLimbsNoCarry 0 mba s1 of
            s2 -> case unsafeFreezeByteArray# mba s2 of
               (# s3, ba #) -> (# s3, BigNat ba #)

   where
      !lcA     = fromIntegral $ bigNatLimbCount a
      !lcB     = fromIntegral $ bigNatLimbCount b
      !lc      = max lcA lcB + 1
      !(I# sz) = lc*WS

      addLimbsNoCarry !l@(I# l#) !mba !s
         | l == lc-1 = shrinkMS mba 1# s
         | l >= lcA  = case copyByteArray# lB off mba off csz s of
               s2 -> shrinkMS mba 1# s2
         | l >= lcB  = case copyByteArray# lA off mba off csz s of
               s2 -> shrinkMS mba 1# s2
         | otherwise = case plusWord2# (indexWordArray# lA l#) (indexWordArray# lB l#) of
               (# c, r #) -> case writeWordArray# mba l# r s of
                  s2 -> addLimbs (l+1) c mba s2
            where
               !(I# off) = l*WS
               !(I# csz) = (lcB+1-l)*WS

      addLimbs !l@(I# l#) !c !mba !s
         | isTrue# (eqWord# c 0##) = addLimbsNoCarry l mba s
         | l == lc-1               = writeWordArray# mba l# c s
         | l >= lcA = case plusWord2# c (indexWordArray# lB l#) of
               (# c2, r #) -> case writeWordArray# mba l# r s of
                  s2 -> addLimbs (l+1) c2 mba s2
         | l >= lcB = case plusWord2# c (indexWordArray# lA l#) of
               (# c2, r #) -> case writeWordArray# mba l# r s of
                  s2 -> addLimbs (l+1) c2 mba s2
         | otherwise = case plusWord2# (indexWordArray# lA l#) (indexWordArray# lB l#) of
               (# c2, r #) -> case plusWord2# r c of
                  (# c3, r2 #) -> case writeWordArray# mba l# r2 s of
                     s2 -> addLimbs (l+1) (plusWord# c2 c3) mba s2

-- | Add a bigNat and a Word#
bigNatAddWord# :: BigNat -> Word# -> BigNat
bigNatAddWord# a             0## = a
bigNatAddWord# a@(BigNat lA) b
   | bigNatIsZero a = bigNatFromWord# b
   | otherwise      = runST $ ST \s0 ->
      let
         !lcA = bigNatLimbCount# lA
         !lc  = lcA +# 1#
         !sz  = lc *# WS#
      in case newByteArray# sz s0 of
         (# s1, mba #) -> 
            let
               go !k !carry !s
                  | isTrue# (k ==# lcA) =
                        if isTrue# (carry `eqWord#` 0##)
                           then shrinkMS mba 1# s
                           else writeWordArray# mba k carry s
                  | isTrue# (carry `eqWord#` 0##) =
                     let
                        !(# c1, c0 #) = plusWord2# (indexWordArray# lA k) b
                     in case writeWordArray# mba k c0 s of
                           s' -> go (k +# 1#) c1 s'
                  | otherwise =
                     let
                        !(# c1' ,  r #) = plusWord2# (indexWordArray# lA k) b
                        !(# c1'', c0 #) = plusWord2# r carry
                        !c1              = plusWord# c1' c1''
                     in case writeWordArray# mba k c0 s of
                           s' -> go (k +# 1#) c1 s'
            in case go 0# 0## s1 of
                  s2 -> case unsafeFreezeByteArray# mba s2 of
                     (# s3, ba #) -> (# s3, BigNat ba #)


-- | Bitwise OR
bigNatOr :: BigNat -> BigNat -> BigNat
bigNatOr a@(BigNat lA) b@(BigNat lB) = runST $ ST \s0 ->
      case newByteArray# sz s0 of
         (# s1, mba #) -> case go mba 0 lcA lcB s1 of
            s2 -> case unsafeFreezeByteArray# mba s2 of
               (# s3, ba #) -> (# s3, BigNat ba #)

   where
      !lcA     = fromIntegral $ bigNatLimbCount a
      !lcB     = fromIntegral $ bigNatLimbCount b
      !lc      = max lcA lcB
      !(I# sz) = lc*WS

      go :: MutableByteArray# s -> Int -> Int -> Int -> State# s -> State# s
      go mba i c1 c2 s
         | c1 == 0 && c2 == 0 = s
         | c1 == 0            = let !(I# csz) = (lcB-i) * WS
                                in copyByteArray# lB off mba off csz s
         | c2 == 0            = let !(I# csz) = (lcA-i) * WS
                                in copyByteArray# lA off mba off csz s
         | otherwise          =
            case writeWordArray# mba i# (indexWordArray# lA i# `or#` indexWordArray# lB i#) s of
               s2 -> go mba (i+1) (c1-1) (c2-1) s2
         where
            !(I# off) = i * WS
            !(I# i#)  = i

-- | Bitwise XOR
bigNatXor :: BigNat -> BigNat -> BigNat
bigNatXor n1@(BigNat lA) n2@(BigNat lB) = runST $ ST \s0 ->
      case newByteArray# sz s0 of
         (# s1, mba #) -> case go mba 0 lcA lcB s1 of
            s2 -> case unsafeFreezeByteArray# mba s2 of
               (# s3, ba #) -> (# s3, BigNat ba #)

   where
      !lcA     = fromIntegral $ bigNatLimbCount n1
      !lcB     = fromIntegral $ bigNatLimbCount n2
      !lc      = max lcA lcB
      !(I# sz) = lc*WS

      go :: MutableByteArray# s -> Int -> Int -> Int -> State# s -> State# s
      go mba i c1 c2 s
         | c1 == 0 && c2 == 0 = s
         | c1 == 0            = let !(I# csz) = (lcB-i) * WS
                                in copyByteArray# lB off mba off csz s
         | c2 == 0            = let !(I# csz) = (lcA-i) * WS
                                in copyByteArray# lA off mba off csz s
         | otherwise          =
            case writeWordArray# mba i# (indexWordArray# lA i# `xor#` indexWordArray# lB i#) s of
               s2 -> go mba (i+1) (c1-1) (c2-1) s2
         where
            !(I# off) = i * WS
            !(I# i#)  = i

-- | Bitwise And
bigNatAnd :: BigNat -> BigNat -> BigNat
bigNatAnd n1@(BigNat lA) n2@(BigNat lB) = runST $ ST \s0 ->
      case newByteArray# sz s0 of
         (# s1, mba #) -> case go mba 0 lc s1 of
            s2 -> case unsafeFreezeByteArray# mba s2 of
               (# s3, ba #) -> (# s3, BigNat ba #)

   where
      !lcA     = fromIntegral $ bigNatLimbCount n1
      !lcB     = fromIntegral $ bigNatLimbCount n2
      !lc      = min lcA lcB
      !(I# sz) = lc*WS

      go :: MutableByteArray# s -> Int -> Int -> State# s -> State# s
      go _   _ 0 s = s
      go mba i c s =
            case writeWordArray# mba i# (indexWordArray# lA i# `and#` indexWordArray# lB i#) s of
               s2 -> go mba (i+1) (c-1) s2
         where
            !(I# i#)  = i

-- | Pop count
bigNatPopCount :: BigNat -> Word
bigNatPopCount n = sum (fmap (fromIntegral . popCount) (bigNatLimbsLS n))

-- | Bit shift right
bigNatShiftR :: BigNat -> Word -> BigNat
bigNatShiftR n 0                  = n
bigNatShiftR n _ | bigNatIsZero n = n
bigNatShiftR n@(BigNat ba) k      = runST $ ST \s0 ->
      case newByteArray# szOut# s0 of
         (# s1, mba #) -> case bitOff of
             -- we drop full limbs
            0 -> case copyByteArray# ba limbOffByte# mba 0# szOut# s1 of
                  s2 -> case unsafeFreezeByteArray# mba s2 of
                     (# s3, lB #) -> (# s3, BigNat lB #)

            _ -> case go mba 0 s1 of
               s2 -> case unsafeFreezeByteArray# mba s2 of
                  (# s3, lB #) -> (# s3, BigNat lB #)

   where
      lc                 = bigNatLimbCount n
      (limbOff,bitOff)   = k `quotRem` WSBITS
      lcOut              = lc - limbOff
      szOut              = lcOut * WS
      !(I# szOut#)       = fromIntegral szOut
      !(I# bitOff#)      = fromIntegral bitOff
      !(I# limbOff#)     = fromIntegral limbOff
      !(I# limbOffByte#) = fromIntegral (limbOff*WS)

      go :: MutableByteArray# s -> Word -> State# s -> State# s
      go _   limbIdx s | limbIdx == lcOut = s
      go mba limbIdx s =
         let
            !(I# limbIdx#) = fromIntegral limbIdx
            srcLimbIdx#    = limbIdx# +# limbOff#
            u = indexWordArray# ba srcLimbIdx#
            v = if limbIdx == lcOut-1 then 0## else indexWordArray# ba (srcLimbIdx# +# 1#)
            w = (u `uncheckedShiftRL#` bitOff#) `or#` (v `uncheckedShiftL#` (WSBITS# -# bitOff#))
         in case writeWordArray# mba limbIdx# w s of
            s2 -> go mba (limbIdx+1) s2

-- | Bit shift left
bigNatShiftL :: BigNat -> Word -> BigNat
bigNatShiftL n 0                   = n
bigNatShiftL n _ | bigNatIsZero n = n
bigNatShiftL n@(BigNat ba) k      = runST $ ST \s0 ->
      case newByteArray# szOut# s0 of
         (# s1, mba #) ->
            -- insert full empty limbs
            case setByteArray# mba 0# limbOffByte# 0# s1 of
               s2 -> case bitOff of
                  0 -> case copyByteArray# ba 0# mba limbOffByte# szIn# s2 of
                        s3 -> case unsafeFreezeByteArray# mba s3 of
                           (# s4, lB #) -> (# s4, BigNat lB #)

                  _ -> case go mba 0 s2 of
                     s3 -> case unsafeFreezeByteArray# mba s3 of
                        (# s4, lB #) -> (# s4, BigNat lB #)

   where
      lc                 = bigNatLimbCount n
      !(I# lc#)          = fromIntegral lc
      (limbOff,bitOff)   = k `quotRem` WSBITS

      -- if the bits we shift in the highest limb are 0, we don't need an
      -- additional limb (which would be null and would break the invariant).
      lastLimb           = indexWordArray# ba (lc# -# 1#)
      needAdditionalLimb = isTrue# ((lastLimb `uncheckedShiftRL#` (WSBITS# -# bitOff#)) `neWord#` 0##)

      lcOutReal          = lc + if bitOff /= 0 && needAdditionalLimb then 1 else 0
      lcOut              = lcOutReal + limbOff
      szOut              = lcOut * WS
      szIn               = lc * WS
      !(I# szIn#)        = fromIntegral szIn
      !(I# szOut#)       = fromIntegral szOut
      !(I# bitOff#)      = fromIntegral bitOff
      !(I# limbOff#)     = fromIntegral limbOff
      !(I# limbOffByte#) = fromIntegral (limbOff*WS)

      go :: MutableByteArray# s -> Word -> State# s -> State# s
      go _   limbIdx s | limbIdx == lcOutReal = s
      go mba limbIdx s =
         let
            !(I# limbIdx#) = fromIntegral limbIdx
            u = if limbIdx == 0 then 0## else indexWordArray# ba (limbIdx# -# 1#)
            v = if limbIdx == lc then 0## else indexWordArray# ba limbIdx#
            w = (v `uncheckedShiftL#` bitOff#) `or#` (u `uncheckedShiftRL#` (WSBITS# -# bitOff#))
         in case writeWordArray# mba (limbIdx# +# limbOff#) w s of
            s2 -> go mba (limbIdx+1) s2

-- | Multiplication (classical algorithm)
bigNatMul :: BigNat -> BigNat -> BigNat
bigNatMul n1@(BigNat lA) n2@(BigNat lB)
   | bigNatLimbCount n2 > bigNatLimbCount n1 = bigNatMul n2 n1 -- optimize loops
   | bigNatIsZero n1 = n1
   | bigNatIsZero n2 = n2
   | bigNatIsOne  n1 = n2
   | bigNatIsOne  n2 = n1
   | otherwise        = runST $ ST \s0 ->
      case newByteArray# sz# s0 of
         (# s1, mba #) -> case loopj0 mba 0 s1 of
               s2 -> case unsafeFreezeByteArray# mba s2 of
                  (# s3, ba #) -> (# s3, BigNat ba #)

   where
      !lcA@(I# lcA#) = fromIntegral $ bigNatLimbCount n1
      !lcB@(I# lcB#) = fromIntegral $ bigNatLimbCount n2
      !lc@(I# lc#)   = lcA + lcB
      !(I# sz#)      = lc*WS

      loopj mba j@(I# j#) s
         | isTrue# (j# ==# lcB#) = case readWordArray# mba (lc# -# 1#) s of
                                    (# s2, 0## #) -> shrinkMS mba 1# s2
                                    (# s2, _   #) -> s2
         | otherwise             = case indexWordArray# lB j# of
                                       0## -> loopj mba (j+1) s
                                       vj  -> loopi mba vj j 0 0## s


      loopi mba vj j@(I# j#) i@(I# i#) k s
         | isTrue# (i# ==# lcA#) = case writeWordArray# mba (i# +# j#) k s of
                                       s2 -> loopj mba (j+1) s2
         | otherwise = case readWordArray# mba (i# +# j#) s of
            (# s2, wij #) ->
               let ui             = indexWordArray# lA i#
                   !(# k1,r1 #)   = timesWord2# ui vj
                   !(# k2,r2 #)   = plusWord2# wij k
                   !(# k3,wij' #) = plusWord2# r1 r2
                   k'             = plusWord# (plusWord# k1 k2) k3
               in case writeWordArray# mba (i# +# j#) wij' s2 of
                     s3 -> loopi mba vj j (i+1) k' s3

      -- loopj0 and loopi0 are executed first when we haven't initialized the
      -- result array.
      loopj0 mba j@(I# j#) s = case indexWordArray# lB j# of
                                 0## -> loopj0 mba (j+1) s -- n2 /= 0 so we can loop safely
                                 vj  -> loopi0 mba vj j 0 0## s

      loopi0 mba vj j@(I# j#) i@(I# i#) k s
         | isTrue# (i# ==# lcA#) = case writeWordArray# mba (i# +# j#) k s of
                                       s2 -> loopj mba (j+1) s2
         | otherwise =
               let ui             = indexWordArray# lA i#
                   !(# k1,r1 #)   = timesWord2# ui vj
                   !(# k2,wij #)  = plusWord2# r1 k
                   k'             = plusWord# k1 k2
               in case writeWordArray# mba (i# +# j#) wij s of
                     s2 -> loopi0 mba vj j (i+1) k' s2

-- | BigNat bit test
bigNatTestBit :: BigNat -> Word -> Bool
bigNatTestBit n@(BigNat ba) i
      | q >= lc   = False
      | otherwise = testBit (W# (indexWordArray# ba q#)) (fromIntegral r)
   where
      lc       = bigNatLimbCount n
      (q,r)    = quotRem i WSBITS
      !(I# q#) = fromIntegral q

-- | Subtract two bigNats (classical algorithm)
bigNatSub :: BigNat -> BigNat -> Maybe BigNat
bigNatSub n1@(BigNat lA) n2@(BigNat lB)
   | bigNatIsZero n2      = Just n1
   | n1 < n2               = Nothing
   | otherwise             = runST $ ST \s0 ->
      case newByteArray# (sizeofByteArray# lA) s0 of
         (# s1, mba #) -> case sub# lA lB mba s1 of
            s2 -> case unsafeFreezeByteArray# mba s2 of
               (# s3, ba #) -> (# s3, Just (BigNat ba) #)

-- | Subtract two bigNats (don't check if a >= b)
bigNatSub_nocheck :: BigNat -> BigNat -> BigNat
bigNatSub_nocheck (BigNat lA) (BigNat lB) = runST $ ST \s0 ->
   case newByteArray# (sizeofByteArray# lA) s0 of
      (# s1, mba #) -> case sub# lA lB mba s1 of
         s2 -> case unsafeFreezeByteArray# mba s2 of
            (# s3, ba #) -> (# s3, BigNat ba #)

sub# :: ByteArray# -> ByteArray# -> MutableByteArray# s -> State# s -> State# s
sub# lA lB mba = go 0# 0# 0# 
   where
      !lcA# = bigNatLimbCount# lA
      !lcB# = bigNatLimbCount# lB

      go carry zeroMSCount i s
         | isTrue# (i ==# lcA#)               = shrinkMS mba zeroMSCount s
         | isTrue# (i >=# lcB#) , 0# <- carry = copyLimbs lA i mba i (lcA# -# i) s
         | otherwise =
            let
               ui = indexWordArray# lA i
               vi = case i <# lcB# of
                        0# -> 0##
                        _  -> indexWordArray# lB i
               !(# wi, carry', zeroMSCount' #) = subWordCTrail# ui vi carry zeroMSCount 
            in case writeWordArray# mba i wi s of
                  s2 -> go carry' zeroMSCount' (i +# 1#) s2

-- | Sub two bigNats starting at specified indices
--
-- Requires:
--    shifted A > shifted B
subAtInplace# :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> State# s -> State# s
subAtInplace# mba idxA lB idxB s1 = case bigNatLimbCountMutable# mba s1 of
   (# s2, lcA# #) -> go 0# 0# idxA idxB s2
      where
         !lcB# = bigNatLimbCount# lB

         go carry zeroMSCount iA iB s
            | isTrue# (iB >=# lcB#)
            , isTrue# (carry ==# 0#) = if isTrue# (iA ==# lcA#)
                                          then shrinkMS mba zeroMSCount s
                                          else s -- there are still non-zero greater limbs in A

            | otherwise = case readWordArray# mba iA s of
               (# s3, ui #) ->
                  let
                     vi = case iB <# lcB# of
                              0# -> 0##
                              _  -> indexWordArray# lB iB
                     !(# wi, carry', zeroMSCount' #) = subWordCTrail# ui vi carry zeroMSCount 
                  in case writeWordArray# mba iA wi s3 of
                        s4 -> go carry' zeroMSCount' (iA +# 1#) (iB +# 1#) s4

-- | Copy limbs
copyLimbs :: ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyLimbs src srcIdx dst dstIdx n s =
   copyByteArray# src (limbsToBytes# srcIdx) dst (limbsToBytes# dstIdx) (limbsToBytes# n) s

-- | Shrink of i MSL
shrinkMS :: MutableByteArray# s -> Int# -> State# s -> State# s
shrinkMS mba i s = case i of
   0# -> s
   _  -> case getSizeofMutableByteArray# mba s of
      (# s2, n #) -> shrinkMutableByteArray# mba (n -# (i `iShiftL#` WSSHIFT#)) s2


-- | Subtraction with carry and zero-trail adjustment
--
-- Return (# w, carry', zeroMSCount' #)
subWordCTrail# :: Word# -> Word# -> Int# -> Int# -> (# Word#, Int#, Int# #)
subWordCTrail# u v carry zeroMSCount = case subWordC# u v of
   (# we, c #) -> case carry of
      0# -> case we of
             -- (# w                  , carry', zeroMSCount'      #)
         0## -> (# we                 , c     , zeroMSCount +# 1# #)
         _   -> (# we                 , c     , 0#                #)
      _  -> case we of
         0## -> (# maxLimb            , 1#    , 0#                #)
         1## -> (# 0##                , c     , zeroMSCount +# 1# #)
         _   -> (# we `minusWord#` 1##, c     , 0#                #)

   where
      !(W# maxLimb) = maxBound


-- | BigNat division returning (quotient,remainder)
--
-- See Note [Multi-Precision Division]
bigNatQuotRem :: BigNat -> BigNat -> Maybe (BigNat,BigNat)
bigNatQuotRem n1 n2@(BigNat lB)
   | bigNatIsZero n2         = Nothing
   | bigNatIsOne n2          = Just (n1, bigNatZero)
   | lcA < lcB                = Just (bigNatZero, n1)
   | lcB == 1                 = case divNby1# n1 (indexWordArray# lB 0#) of
                                    Nothing    -> Nothing
                                    Just (q,r) -> Just (q, bigNatFromWord r)

   | otherwise = Just (bigNatWithNormalized bigNatQuotRem_normalized n1 n2)
      where
         lcA      = bigNatLimbCount n1
         lcB      = bigNatLimbCount n2

-- | 1-by-1 small addition
--
-- Requires:
--    a0+b0 < B
add1by1_small# :: Word# -> Word# -> Word#
add1by1_small# a0 b0 = plusWord# a0 b0

-- | 1-by-1 large addition
add1by1_large# :: Word# -> Word# -> (# Word#,Word# #)
add1by1_large# a0 b0 = plusWord2# a0 b0

-- | 1-by-2 small addition
--
-- Requires:
--    a0+(b1,b0) < B^2
add1by2_small# :: Word# -> (# Word#,Word# #) -> (# Word#,Word# #)
add1by2_small# a0 (# b1,b0 #) = (# m1, m0 #)
   where
      !(# t, m0 #) = add1by1_large# a0 b0
      !m1          = add1by1_small# t b1

-- | 1-by-2 large addition
add1by2_large# :: Word# -> (# Word#,Word# #) -> (# Word#,Word#,Word# #)
add1by2_large# a0 (# b1,b0 #) = (# m2,m1,m0 #)
   where
      !(# t, m0 #) = add1by1_large# a0 b0
      !(# m2,m1 #) = add1by1_large# t b1

-- | 2-by-2 small addition
--
-- Requires:
--    (a1,a0)+(b1,b0) < B^2
add2by2_small# :: (# Word#,Word# #) -> (# Word#,Word# #) -> (# Word#,Word# #)
add2by2_small# (# a1,a0 #) (# b1,b0 #) = (# m1, m0 #)
   where
      !(# c0, m0 #) = add1by1_large# a0 b0
      !c1           = add1by1_small# c0 b1
      !m1           = add1by1_small# c1 a1

-- | 2-by-1 small subtraction
--
-- Requires:
--    (a1,a0)>=b0
sub2by1# :: (# Word#,Word# #) -> Word# -> (# Word#,Word# #)
sub2by1# (# a1,a0 #) b0 = (# m1, m0 #)
   where
      !(# m0,c #) = subWordC# a0 b0
      !m1         = if isTrue# c then minusWord# a1 1## else a1

-- | 2-by-2 small subtraction
--
-- Requires:
--    (a1,a0)>=(b1,b0)
sub2by2# :: (# Word#,Word# #) -> (# Word#,Word# #) -> (# Word#,Word# #)
sub2by2# (# a1,a0 #) (# b1,b0 #) = (# m1, m0 #)
   where
      !(# t,m0 #) = sub2by1# (# a1, a0 #) b0
      !m1         = minusWord# t b1

-- | 3-by-1 small subtraction
--
-- Requires:
--    (a2,a1,a0)>=b0
sub3by1# :: (# Word#,Word#,Word# #) -> Word# -> (# Word#,Word#,Word# #)
sub3by1# (# a2,a1,a0 #) b0 = (# m2,m1,m0 #)
   where
      !(# m0,c #)  = subWordC# a0 b0
      !(# m2,m1 #) = if isTrue# c then sub2by1# (# a2,a1 #) 1## else (# a2, a1 #)


-- | 1-by-1 small multiplication
--
-- Requires:
--    a0*b0 < B
mul1by1_small# :: Word# -> Word# -> Word#
mul1by1_small# a0 b0 = timesWord# a0 b0

-- | 1-by-1 large multiplication
mul1by1_large# :: Word# -> Word# -> (# Word#,Word# #)
mul1by1_large# a0 b0 = timesWord2# a0 b0

-- | 1-by-2
mul1by2# :: Word# -> (# Word#,Word# #) -> (# Word#,Word#,Word# #)
mul1by2# a0 (# b1,b0 #) = (# m2,m1,m0 #)
   where
      !(# t0, m0 #) = mul1by1_large# a0 b0
      !(# t2, t1 #) = mul1by1_large# a0 b1
      -- if a0 = b1 = maxBound = B-1 then a0*b1 = (B-1)^2 = B^2 + 1 - 2*B
      -- t0 < B hence a0*b1+t0 < B^2 + 1 - B
      -- B > 1 hence a0*b1+t0 < B^2
      -- Conclusion: we can use add1by2_small#
      !(# m2, m1 #) = add1by2_small# t0 (# t2,t1 #)

-- | Compare 2-word bigNats
cmp2by2# :: (# Word#,Word# #) -> (# Word#,Word# #) -> Ordering
cmp2by2# (# a1,a0 #) (# b1, b0 #)
   | isTrue# (a1 `gtWord#` b1) = GT
   | isTrue# (b1 `gtWord#` a1) = LT
   | isTrue# (a0 `gtWord#` b0) = GT
   | isTrue# (b0 `gtWord#` a0) = LT
   | otherwise                 = EQ


-- | 1-by-1 division
--
-- Requires:
--    b0 /= 0
div1by1# :: Word# -> Word# -> (# Word#,Word# #)
div1by1# a0 b0 = (# q0, r0 #)
   where 
      !(# q0,r0 #) = quotRemWord# a0 b0

-- | 2-by-1 small division (a1 < b0)
-- 
-- Requires:
--    b0 /= 0
--    a1 < b0
div2by1_small# :: (# Word#,Word# #) -> Word# -> (# Word#,Word# #)
div2by1_small# (# a1,a0 #) b0 = (# q0, r0 #)
   where
      !(# q0, r0 #) = quotRemWord2# a1 a0 b0

-- | 2-by-1 large division (a1 >= b0)
-- 
-- Requires:
--    b0 /= 0
--    a1 >= b0 (not required, but if not q1=0)
div2by1_large# :: (# Word#,Word# #) -> Word# -> (# (# Word#,Word# #),Word# #)
div2by1_large# (# a1,a0 #) b0 = (# (# q1, q0 #), r0 #)
   where
      !(# q1, r' #) = div1by1# a1 b0
      !(# q0, r0 #) = div2by1_small# (# r',a0 #) b0

-- | 3-by-2 small division
-- 
-- Requires:
--    b1 /= 0
--    (a2,a1) < (b1,b0)
div3by2_small# :: (# Word#,Word#,Word# #) -> (# Word#,Word# #) -> (# Word#, (# Word#,Word# #) #)
div3by2_small# (# a2,a1,a0 #) (# b1,b0 #) = (# q0, (# r1,r0 #) #)
   where
      -- candidate quotient qe. The real quotient is <= qe0
      !(# qe, re0 #) = div2by1_small# (# a2,a1 #) b1
      -- high remainder: remainder obtained by dividing (a2,a1,a0) by (b1,0##)
      !hr = (# re0,a0 #)

      -- we sub 1 to the quotient q until q*b0 <= hr
      !(# q0, (# r1,r0 #) #) = go qe hr (mul1by1_large# qe b0)

      go qc rc c = case cmp2by2# rc c of
         EQ -> (# qc, (# 0##, 0## #) #)
         LT -> go (qc `minusWord#` 1##) (add2by2_small# rc (# b1,b0 #)) (sub2by1# c b0)
         GT -> (# qc, rc #)

-- | 3-by-2 large division
-- 
-- Requires:
--    b1 /= 0
div3by2_large# :: (# Word#,Word#,Word# #) -> (# Word#,Word# #) -> (# (# Word#,Word# #), (# Word#,Word# #) #)
div3by2_large# (# a2,a1,a0 #) (# b1,b0 #) = (# (# q1,q0 #), (# r1,r0 #) #)
   where
      -- candidate quotient qe. The real quotient is <= qe0
      !(# (# qe1, qe0 #), re0 #) = div2by1_large# (# a2,a1 #) b1
      -- high remainder: remainder obtained by dividing (a2,a1,a0) by (b1,0##)
      !hr = (# re0,a0 #)

      -- We sub 1 to the quotient q until q*b0 <= hr
      -- We don't require normalization as we loop, but normalization implies
      -- less loop cycles.
      -- Normalization = make b1 larger by left-shifting b and a by
      -- "countLeadingZeros b1". The quotient stays the same but the remainder
      -- as to be shifted right of the same amount of bits.
      !(# (# q1,q0 #), (# r1,r0 #) #) = go (# qe1, qe0 #) hr (mul1by2# b0 (# qe1,qe0 #))

      go qc rc c@(# c2,c1,c0 #) = 
         case c2 of
            0## -> go2 qc rc (# c1,c0 #)
            _   -> go (sub2by1# qc 1##) (add2by2_small# rc (# b1,b0 #)) (sub3by1# c b0)

      go2 qc rc c = case cmp2by2# rc c of
         EQ -> (# qc, (# 0##, 0## #) #)
         LT -> go2 (sub2by1# qc 1##) (add2by2_small# rc (# b1,b0 #)) (sub2by1# c b0)
         GT -> (# qc, rc #)

-- | 3-by-2 division
-- 
-- Requires:
--    b1 /= 0
div3by2# :: (# Word#,Word#,Word# #) -> (# Word#,Word# #) -> (# (# Word#,Word# #),(# Word#,Word# #) #)
div3by2# a@(# a2,a1,_ #) b@(# b1,b0 #) = case cmp2by2# (# a2,a1 #) (# b1,b0 #) of
   LT -> case div3by2_small# a b of
            (# q0, r #) -> (# (# 0##,q0 #), r #)
   _  -> div3by2_large# a b


-- | 4-by-2 small division
--
-- Requires:
--    b1 /= 0
--    (a3,a2) < (b1,b0)
div4by2_small# :: (# Word#,Word#,Word#,Word# #) -> (# Word#,Word# #) -> (# (# Word#,Word# #),(# Word#,Word# #) #)
div4by2_small# (# a3,a2,a1,a0 #) (# b1,b0 #) = (# (# q1,q0 #), (# r1,r0 #) #)
   where
      -- we require that (b1,b0) > (a3,a2) so we can use small division
      !(# q1, (# c1,c0 #) #) = div3by2_small# (# a3,a2,a1 #) (# b1,b0 #)
      -- we know that (b1,b0) > (c1,c0) so we can use small division
      !(# q0, (# r1,r0 #) #) = div3by2_small# (# c1,c0,a0 #) (# b1,b0 #)

-- | 4-by-2 large division
--
-- Requires:
--    b1 /= 0
div4by2_large# :: (# Word#,Word#,Word#,Word# #) -> (# Word#,Word# #) -> (# (# Word#,Word#,Word# #),(# Word#,Word# #) #)
div4by2_large# (# a3,a2,a1,a0 #) (# b1,b0 #) = (# (# q2,q1,q0 #), (# r1,r0 #) #)
   where
      !(# (# q2,q1 #), (# c1,c0 #) #) = div3by2_large# (# a3,a2,a1 #) (# b1,b0 #)
      -- we know that (b1,b0) > (c1,c0) so we can use small division
      !(# q0, (# r1,r0 #) #)          = div3by2_small# (# c1,c0,a0 #) (# b1,b0 #)


-- | N-by-1 division
--
-- Require:
--    b /= 0
divNby1# :: BigNat -> Word# -> Maybe (BigNat, Word)
divNby1# n@(BigNat a) b = runST $ ST \s0 ->
   let
      lc       = fromIntegral $ bigNatLimbCount n
      !(I# sz) = lc * WS

      go mba i@(I# i#) trailing zeroMSCount@(I# zt) r s
         | i == 0 = case shrinkMS mba zt s of
                        s2 -> (# s2, r #)
         | otherwise =
            let
               off         = i# -# 1#
               an          = indexWordArray# a off
               !(# q,r' #) = quotRemWord2# r an b
               qZero       = isTrue# (q `eqWord#` 0##)
               trailing'   = trailing && qZero
               zeroMSCount'  = if trailing' then zeroMSCount+1 else zeroMSCount
            in case writeWordArray# mba off q s of
                  s2 -> go mba (I# off) trailing' zeroMSCount' r' s2
   in case newByteArray# sz s0 of
      (# s1, mba #) -> case go mba lc True 0 0## s1 of
         (# s2, r #) -> case unsafeFreezeByteArray# mba s2 of
            (# s3, ba #) -> (# s3, Just (BigNat ba, W# r) #)



-- | Perform the division operation with normalized values.
--
-- Given fdiv, A and B:
--    * compute normalized values A' and B'
--    * compute (Q',R') = fdiv A' B'
--    * return unormalized (Q,R)
bigNatWithNormalized :: (BigNat -> BigNat -> (BigNat,BigNat)) -> BigNat -> BigNat -> (BigNat,BigNat)
{-# INLINABLE bigNatWithNormalized #-}
bigNatWithNormalized f a b@(BigNat ba)
   | isTrue# (k# `eqWord#` 0##) = f a b
   | otherwise                  = let (q,r') = f (bigNatShiftL a k) (bigNatShiftL b k)
                                  in (q, bigNatShiftR r' k)
   where
      lc        = bigNatLimbCount b
      !(I# lc#) = fromIntegral lc
      !k#       = clz# (indexWordArray# ba (lc# -# 1#))
      k         = W# k#

-- | Compare a and b where limbCount a >= limbCount b after dropping the
-- superfluous least-significant limbs of a so that limbCount a == limbCount b
--
-- Inputs: A (m+n limbs) and B (n limbs). Compare (A>>(m<<WSBITS)) and B
bigNatComparePrefix :: BigNat -> BigNat -> Ordering
bigNatComparePrefix a b = go (take n (bigNatLimbsMS a)) (bigNatLimbsMS b)
   where
      n  = fromIntegral $ bigNatLimbCount b
      go (x:xs) (y:ys)
         | x > y     = GT
         | x < y     = LT
         | otherwise = go xs ys
      go ~[] ~[]     = EQ

-- | Compare a' and b where:
--    a' = dropLSL i a 
--    limbCount a' >= limbCount b
cmpDropped# :: MutableByteArray# s -> Int# -> ByteArray# -> State# s -> (# State# s, Ordering #)
cmpDropped# mba idx ba s1 = case bigNatLimbCountMutable# mba s1 of
   (# s2, lca #) -> case compare (I# (lca -# idx)) (I# lcb) of
      GT -> (# s2, GT #)
      LT -> (# s2, LT #)
      EQ -> go (lca -# 1#) (lcb -# 1#) s2
   where
      !lcb = bigNatLimbCount# ba
      go i j s =
         let
            !v = indexWordArray# ba j
         in case readWordArray# mba i s of
            (# s2, u #) -> if
               | isTrue# (u `gtWord#` v) -> (# s2, GT #)
               | isTrue# (u `ltWord#` v) -> (# s2, LT #)
               | isTrue# (j ==# 0#)      -> (# s2, EQ #)
               | otherwise               -> go (i -# 1#) (j -# 1#) s2


-- | BigNat quot/rem
--
-- Requires:
--    a > b
--    b /= 0
--    b normalized
bigNatQuotRem_normalized :: BigNat -> BigNat -> (BigNat,BigNat)
bigNatQuotRem_normalized a@(BigNat lA) b@(BigNat lB) = runST $ ST \s0 ->
      case initQ s0 of
         (# s1, qa #) -> case initR s1 of
            (# s2, ra #) -> case loop ra qa (lcAmB -# 1#) s2 of
               s3 -> case unsafeFreezeByteArray# qa s3 of
                  (# s4, q #) -> case unsafeFreezeByteArray# ra s4 of
                     (# s5, r #) -> (# s5, (BigNat q, BigNat r) #)

   where

      -- We require a > b so lcA >= lcB: E.g.
      --    lA: A9 A8 A7 A6 ... A1 A0
      --    lB:          A6 ... A1 A0

      lcA = bigNatLimbCount# lA
      lcB = bigNatLimbCount# lB

      -- The difference lcAmB between the two limb counts gives the number of
      -- steps of the algorithm. For each step k from lcAmB to 0 we compare the
      -- remainder (= A at the beginning) with B left-shifted of k limbs and we
      -- find how many shifted Bs can be subtracted from the remainder: this is
      -- qk (the k-th limb of the quotient).

      !lcAmB = lcA -# lcB

      -- The first step (k = lcAmB) is particular because as B is normalized its
      -- most-significant bit is set. Moreover A and B' (= k-left-shifted B)
      -- have the same number of limbs. Suppose we have Qk >= 2. Then Qk*B' has
      -- at least an additional limb, so Qk*B'>A. Hence Qk=0 or Qk=1.
      -- We can compute Qk very cheaply by doing a prefix comparison of A and B:
      --    if prefix of A>B then Qk=1 else Qk=0
      --
      -- Note that if Qk=0 then we mustn't allocate a 0 most-significant limb
      -- for Q.
      --
      -- Depending on Qk, the initial remainder is either:
      --    Qk=0 ==> A
      --    Qk=1 ==> A-B'

      prefixCmpAB = bigNatComparePrefix a b

      initQ s = case prefixCmpAB of
         LT -> newByteArray# (limbsToBytes# lcAmB) s
         _  -> case newByteArray# (limbsToBytes# (lcAmB +# 1#)) s of
            (# s2,mba #) -> case writeWordArray# mba lcAmB 1## s2 of -- write the carry
               s3 -> (# s3,mba #)

      initR s = case newByteArray# (sizeofByteArray# lA) s of
         (# s2,mba #) -> case copyByteArray# lA 0# mba 0# (sizeofByteArray# lA) s2 of
            s3 -> case prefixCmpAB of
               LT -> (# s3, mba #)
               _  -> case subAtInplace# mba lcAmB lB 0# s3 of
                  s4 -> (# s4, mba #)

      -- Now the loop per se. Note that if A and B have the same number of
      -- limbs, the loop isn't executed at all.
      -- We have k iterating from (lcAmB-1) to 0. Each time we get qk and an
      -- updated R.
      -- First we compute an upper bound qMax for qk: qk <= qMax
      -- Then the idea is to iterate qe from qMax to 0 until
      --    qe*B k-left-shifted <= R
      --  or equivalently:
      --    qe*B <= R k-right-shifted
      --
      -- As multiplication is costly, we implement it as follows:
      --    qe = qMax
      --    c  = qe*B
      --    while c > R k-right-shifted do
      --       c  -= B
      --       qe -= 1
      --    qk = qe
      --
      -- To determine qMax we compute:
      --    (qMaxCarry,qMax') = (R{k+lcB},R{k+lcB-1}) `div` B{lcB-1}
      --    If qMaxCarry /= 0 then
      --       qMax = maxLimbValue
      --    else
      --       qMax = qMax'
      -- qk can only be a single digit number because q(k+1) is already computed
      -- and the prefix of R is < B after the previous step.

      computeQMax lR k s1 =
         case bigNatLimbCountMutable# lR s1 of
            (# s2, lcR #) ->
               let
                  !o1 = lcB +# k
                  !o2 = o1  -# 1#
                  !(# s3, ro1 #)    = case isTrue# (o1 <# lcR) of
                                         True  -> readWordArray# lR o1 s2
                                         False -> (# s2, 0## #)
                  !(# s4, ro2 #)  =  case isTrue# (o2 <# lcR) of
                                         True  -> readWordArray# lR o2 s3
                                         False -> (# s3, 0## #)
                  !qMax = case div2by1_large# (# ro1, ro2 #) bMSL of
                     (# (# qMaxCarry, qMax' #), _ #) -> case qMaxCarry of
                        0## -> qMax'
                        _   -> maxLimb
               in (# s4, qMax #)


      -- most significant limb of b
      bMSL = mostSignificantLimb# lB
      -- maximal limb value
      !(W# maxLimb) = maxBound

      loop ra qa k s
         | 0# <- k   = computeQuot ra qa k s
         | otherwise = case computeQuot ra qa k s of
            s2 -> loop ra qa (k -# 1#) s2
   
      -- compute qj (in qa) and remainder in ra
      computeQuot ra qa k s1 =
         case computeQMax ra k s1 of
            (# s2, qe #) ->
               let
                  -- Now we can compute qe*b and check whether it is < (a >> k limbs)
                  -- If not, it means we need to decrease qe and recheck until it is
                  -- true.
                  -- Note: we only compute qe*b once and then we subtract b from it.
                  vinit          = bigNatMul b (bigNatFromWord (W# qe)) -- TODO: implement and use mulNby1#

                  go qc c@(BigNat ca) s = case cmpDropped# ra k ca s of
                     (# s', LT #) -> go (qc `minusWord#` 1##) (bigNatSub_nocheck c b) s'
                     (# s', EQ #) -> -- we can just drop the higher MSLs of a
                                     case shrinkMS ra (bigNatLimbCount# ca) s' of
                                        s'' -> (# s'', qc #)
                     (# s', GT #) -> -- we need to compute the subtraction of b
                                     case subAtInplace# ra k ca 0# s' of
                                        s'' -> (# s'', qc #)

               in case go qe vinit s2 of
                  (# s3, q #) -> writeWordArray# qa k q s3


checkDiv :: [Word] -> [Word] -> Bool
checkDiv xs ys
   | bigNatIsZero y = True
   | otherwise       = q*y +r == x
   where
      x = bigNatFromLimbsMS xs
      y = bigNatFromLimbsMS ys
      (q,r) = x `quotRem` y
