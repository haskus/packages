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
   , limbCount
   )
where

import GHC.Exts
import GHC.ST
import Data.Char
import Data.Bits

-- Word size in bytes
#define WS 8

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
   | naturalIsZero n = "0"
   | otherwise       = '0' : 'x' : fmap hex16 (dropWhile (==0) (concatMap limb4MS (naturalLimbsMS n)))
   where
      hex16 x
         | x <= 9    = chr (48+fromIntegral x)
         | otherwise = chr (55+fromIntegral x)

      -- limbs in 4-bit chunk, most significant first
      limb4MS w = goLimbs4 64 w
      goLimbs4 0 _ = []
      goLimbs4 k x = (x `unsafeShiftR` (k-4)) .&. 0xF : goLimbs4 (k-4) x

-- limbs: most significant first
naturalLimbsMS :: Natural -> [Word]
naturalLimbsMS n@(Natural ba)
   | naturalIsZero n = []
   | otherwise       = goLimbs (fromIntegral (limbCount n - 1))
   where
      goLimbs 0         = [W# (indexWord64Array# ba 0#)]
      goLimbs i@(I# i#) = W# (indexWord64Array# ba i#) : goLimbs (i-1)

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
               !(I# csz) = (lc2+1-l)*8

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
