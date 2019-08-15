{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

module Haskus.Integer
   ( Natural
   , naturalFromWord
   , naturalIsZero
   , naturalZero
   , naturalShowHex
   , naturalEq
   , naturalAdd
   , naturalCompare
   , naturalOr
   , naturalAnd
   , naturalXor
   , naturalPopCount
   , naturalShiftR
   , limbCount
   )
where

import GHC.Exts
import GHC.ST
import Data.Char
import Data.Bits

-- Word size in bytes
#define WS 8
#define WSSHIFT 3
#define WSBITS 64

-- | A Natural
--
-- Stored as an array of Word64 limbs, lower limbs first, limbs use host
-- endianness.
--
-- Invariant:
--  - no empty high limb
--     ==> zero is zero size array
--     ==> canonical representation
data Natural = Natural ByteArray#

instance Show Natural where
   show = naturalShowHex

instance Eq Natural where
   (==) = naturalEq

instance Ord Natural where
   compare = naturalCompare

-- | Count limbs
limbCount :: Natural -> Word
limbCount (Natural ba) =
   W# (uncheckedShiftRL# (int2Word# (sizeofByteArray# ba)) 3#)

-- | Natural Zero
naturalZero :: Natural
naturalZero = runST $ ST $ \s0 ->
   case newByteArray# 0# s0 of
      (# s1, mba #) -> case unsafeFreezeByteArray# mba s1 of
            (# s2, ba #) -> (# s2, Natural ba #)

-- | Indicate if a natural is zero
naturalIsZero :: Natural -> Bool
naturalIsZero n = limbCount n == 0

-- | Create a Natural from a Word
naturalFromWord :: Word -> Natural
naturalFromWord (W# 0##) = naturalZero
naturalFromWord (W# w)   = runST $ ST \s0 ->
   case newByteArray# WS# s0 of
      (# s1, mba #) -> case writeWord64Array# mba 0# w s1 of
         s2 -> case unsafeFreezeByteArray# mba s2 of
            (# s3, ba #) -> (# s3, Natural ba #)

-- | Show a Natural
naturalShowHex :: Natural -> String
naturalShowHex n
   | naturalIsZero n = "0x0"
   | otherwise       = '0' : 'x' : add0 (fmap hex16 (dropWhile (==0) (concatMap limb4MS (naturalLimbsMS n))))
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

-- Limbs: most significant first
naturalLimbsMS :: Natural -> [Word]
naturalLimbsMS n@(Natural ba)
   | naturalIsZero n = []
   | otherwise       = goLimbs (fromIntegral (limbCount n - 1))
   where
      goLimbs 0         = [W# (indexWord64Array# ba 0#)]
      goLimbs i@(I# i#) = W# (indexWord64Array# ba i#) : goLimbs (i-1)

-- Limbs: less significant first
naturalLimbsLS :: Natural -> [Word]
naturalLimbsLS n@(Natural ba)
   | naturalIsZero n = []
   | otherwise       = goLimbs 0
   where
      lc = fromIntegral (limbCount n)
      goLimbs i@(I# i#)
         | i == lc   = []
         | otherwise = W# (indexWord64Array# ba i#) : goLimbs (i+1)

-- | Equality
naturalEq :: Natural -> Natural -> Bool
naturalEq n1 n2
   | limbCount n1 /= limbCount n2 = False
   | otherwise                    = all (uncurry (==)) (naturalLimbsMS n1 `zip` naturalLimbsMS n2)

-- | Compare
naturalCompare :: Natural -> Natural -> Ordering
naturalCompare n1 n2
   | limbCount n1 > limbCount n2 = GT
   | limbCount n1 < limbCount n2 = LT
   | otherwise                   = go (naturalLimbsMS n1) (naturalLimbsMS n2)
      where
         go [] []         = EQ
         go ~(x:xs) ~(y:ys) = case compare x y of
            EQ -> go xs ys
            r  -> r

-- | Add two naturals
naturalAdd :: Natural -> Natural -> Natural
naturalAdd n1@(Natural ba1) n2@(Natural ba2)
   | naturalIsZero n1 = n2
   | naturalIsZero n2 = n1
   | otherwise        = runST $ ST \s0 ->
      case newByteArray# sz s0 of
         (# s1, mba #) -> case addLimbsNoCarry 0 mba s1 of
            s2 -> case unsafeFreezeByteArray# mba s2 of
               (# s3, ba #) -> (# s3, Natural ba #)

   where
      lc1      = fromIntegral $ limbCount n1
      lc2      = fromIntegral $ limbCount n2
      lc       = max lc1 lc2 + 1
      !(I# sz) = lc*WS

      addLimbsNoCarry l@(I# l#) mba s
         | l == lc-1 = shrinkMutableByteArray# mba (sz -# 1#) s
         | l >= lc1  = case copyByteArray# ba2 off mba off csz s of
               s2 -> shrinkMutableByteArray# mba (sz -# 1#) s2
         | l >= lc2  = case copyByteArray# ba1 off mba off csz s of
               s2 -> shrinkMutableByteArray# mba (sz -# 1#) s2
         | otherwise = case plusWord2# (indexWord64Array# ba1 l#) (indexWord64Array# ba2 l#) of
               (# c, r #) -> case writeWord64Array# mba l# r s of
                  s2 -> addLimbs (l+1) c mba s2
            where
               !(I# off) = l*WS
               !(I# csz) = (lc2+1-l)*WS

      addLimbs l@(I# l#) c mba s
         | isTrue# (eqWord# c 0##) = addLimbsNoCarry l mba s
         | l == lc-1               = writeWord64Array# mba l# c s
         | l >= lc1 = case plusWord2# c (indexWord64Array# ba2 l#) of
               (# c2, r #) -> case writeWord64Array# mba l# r s of
                  s2 -> addLimbs (l+1) c2 mba s2
         | l >= lc2 = case plusWord2# c (indexWord64Array# ba1 l#) of
               (# c2, r #) -> case writeWord64Array# mba l# r s of
                  s2 -> addLimbs (l+1) c2 mba s2
         | otherwise = case plusWord2# (indexWord64Array# ba1 l#) (indexWord64Array# ba2 l#) of
               (# c2, r #) -> case plusWord2# r c of
                  (# c3, r2 #) -> case writeWord64Array# mba l# r2 s of
                     s2 -> addLimbs (l+1) (plusWord# c2 c3) mba s2

-- | Bitwise OR
naturalOr :: Natural -> Natural -> Natural
naturalOr n1@(Natural ba1) n2@(Natural ba2) = runST $ ST \s0 ->
      case newByteArray# sz s0 of
         (# s1, mba #) -> case go mba 0 lc1 lc2 s1 of
            s2 -> case unsafeFreezeByteArray# mba s2 of
               (# s3, ba #) -> (# s3, Natural ba #)

   where
      lc1      = fromIntegral $ limbCount n1
      lc2      = fromIntegral $ limbCount n2
      lc       = max lc1 lc2
      !(I# sz) = lc*WS

      go :: MutableByteArray# s -> Int -> Int -> Int -> State# s -> State# s
      go mba i c1 c2 s
         | c1 == 0 && c2 == 0 = s
         | c1 == 0            = let !(I# csz) = (lc2-i) * WS
                                in copyByteArray# ba2 off mba off csz s
         | c2 == 0            = let !(I# csz) = (lc1-i) * WS
                                in copyByteArray# ba1 off mba off csz s
         | otherwise          =
            case writeWord64Array# mba i# (indexWord64Array# ba1 i# `or#` indexWord64Array# ba2 i#) s of
               s2 -> go mba (i+1) (c1-1) (c2-1) s2
         where
            !(I# off) = i * WS
            !(I# i#)  = i

-- | Bitwise XOR
naturalXor :: Natural -> Natural -> Natural
naturalXor n1@(Natural ba1) n2@(Natural ba2) = runST $ ST \s0 ->
      case newByteArray# sz s0 of
         (# s1, mba #) -> case go mba 0 lc1 lc2 s1 of
            s2 -> case unsafeFreezeByteArray# mba s2 of
               (# s3, ba #) -> (# s3, Natural ba #)

   where
      lc1      = fromIntegral $ limbCount n1
      lc2      = fromIntegral $ limbCount n2
      lc       = max lc1 lc2
      !(I# sz) = lc*WS

      go :: MutableByteArray# s -> Int -> Int -> Int -> State# s -> State# s
      go mba i c1 c2 s
         | c1 == 0 && c2 == 0 = s
         | c1 == 0            = let !(I# csz) = (lc2-i) * WS
                                in copyByteArray# ba2 off mba off csz s
         | c2 == 0            = let !(I# csz) = (lc1-i) * WS
                                in copyByteArray# ba1 off mba off csz s
         | otherwise          =
            case writeWord64Array# mba i# (indexWord64Array# ba1 i# `xor#` indexWord64Array# ba2 i#) s of
               s2 -> go mba (i+1) (c1-1) (c2-1) s2
         where
            !(I# off) = i * WS
            !(I# i#)  = i

-- | Bitwise And
naturalAnd :: Natural -> Natural -> Natural
naturalAnd n1@(Natural ba1) n2@(Natural ba2) = runST $ ST \s0 ->
      case newByteArray# sz s0 of
         (# s1, mba #) -> case go mba 0 lc s1 of
            s2 -> case unsafeFreezeByteArray# mba s2 of
               (# s3, ba #) -> (# s3, Natural ba #)

   where
      lc1      = fromIntegral $ limbCount n1
      lc2      = fromIntegral $ limbCount n2
      lc       = min lc1 lc2
      !(I# sz) = lc*WS

      go :: MutableByteArray# s -> Int -> Int -> State# s -> State# s
      go _   _ 0 s = s
      go mba i c s =
            case writeWord64Array# mba i# (indexWord64Array# ba1 i# `and#` indexWord64Array# ba2 i#) s of
               s2 -> go mba (i+1) (c-1) s2
         where
            !(I# i#)  = i

-- | Pop count
naturalPopCount :: Natural -> Word
naturalPopCount n = sum (fmap (fromIntegral . popCount) (naturalLimbsLS n))

-- | Bit shift right
naturalShiftR :: Natural -> Word -> Natural
naturalShiftR n 0                   = n
naturalShiftR n _ | naturalIsZero n = n
naturalShiftR n@(Natural ba) k      = runST $ ST \s0 ->
      case newByteArray# szOut# s0 of
         (# s1, mba #) -> case bitOff of
             -- we drop full limbs
            0 -> case copyByteArray# ba limbOff# mba 0# szOut# s1 of
                  s2 -> case unsafeFreezeByteArray# mba s2 of
                     (# s3, ba2 #) -> (# s3, Natural ba2 #)

            _ -> case go mba 0 s1 of
               s2 -> case unsafeFreezeByteArray# mba s2 of
                  (# s3, ba2 #) -> (# s3, Natural ba2 #)

   where
      lc               = limbCount n
      (limbOff,bitOff) = k `quotRem` WSBITS
      lcOut            = lc - limbOff
      szOut            = lcOut * WS
      !(I# szOut#)     = fromIntegral szOut
      !(I# bitOff#)    = fromIntegral bitOff
      !(I# limbOff#)   = fromIntegral limbOff

      go :: MutableByteArray# s -> Word -> State# s -> State# s
      go _   limbIdx s | limbIdx == lcOut = s
      go mba limbIdx s =
         let
            !(I# limbIdx#) = fromIntegral limbIdx
            srcLimbIdx#    = limbIdx# +# limbOff#
            u = indexWord64Array# ba srcLimbIdx#
            v = if limbIdx == lcOut-1 then 0## else indexWord64Array# ba (srcLimbIdx# +# 1#)
            w = (u `uncheckedShiftRL#` bitOff#) `or#` (v `uncheckedShiftL#` (WSBITS# -# bitOff#))
         in case writeWord64Array# mba limbIdx# w s of
            s2 -> go mba (limbIdx+1) s2
