{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fobject-code #-}

-- | Multi-precision integers
--
-- Classical algorithms adapted from "The Art of Computer Programming, vol. 2,
-- Donald E. Knuth"
module Haskus.Integer
   ( Natural
   , naturalFromWord
   , naturalFromInteger
   , naturalIsZero
   , naturalIsOne
   , naturalZero
   , naturalShowHex
   , naturalEq
   , naturalAdd
   , naturalSub
   , naturalMul
   , naturalQuotRem
   , naturalCompare
   , naturalOr
   , naturalAnd
   , naturalXor
   , naturalPopCount
   , naturalShiftR
   , naturalShiftL
   , naturalLimbCount
   , naturalWithNormalized
   -- * Primitives
   , naturalLimbCount#
   , naturalLimbCountMutable#
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
   , subAt#
   , cmpMS
   , cmpDropped#
   , shrinkMS
   , subAtInplace#
   , naturalSub_nocheck
   , mostSignificantLimb#
   , naturalQuotRem_normalized
   )
where

import GHC.Exts
import GHC.ST
import Data.Char
import Data.Bits
import Data.Maybe

-- Word size in bytes
#define WS 8
#define WSSHIFT 3
#define WSBITS 64

-- | A Natural
--
-- Stored as an array of Word limbs, lower limbs first, limbs use host
-- endianness.
--
-- Invariant:
--  - no empty high limb
--     ==> zero is zero size array
--     ==> canonical representation
data Natural = Natural ByteArray#

instance Show Natural where
   show = show . naturalToInteger

instance Eq Natural where
   (==) = naturalEq

instance Ord Natural where
   compare = naturalCompare

instance Num Natural where
   (+)         = naturalAdd
   (*)         = naturalMul
   x - y       = fromMaybe (error "Can't subtract these naturals") (naturalSub x y)
   abs         = id
   signum _    = naturalFromWord 1
   negate _    = error "Can't negate a Natural"
   fromInteger = naturalFromInteger

instance Bits Natural where
   (.&.)          = naturalAnd
   (.|.)          = naturalOr
   xor            = naturalXor
   complement     = error "Can't complement a Natural"
   shiftL w n     = naturalShiftL w (fromIntegral n)
   shiftR w n     = naturalShiftR w (fromIntegral n)
   isSigned _     = False
   zeroBits       = naturalZero
   bitSizeMaybe _ = Nothing
   popCount       = fromIntegral . naturalPopCount
   bitSize        = error "Can't use bitsize on Natural"
   bit i
      | i < WS    = naturalFromWord (bit i)
      | otherwise = naturalFromWord 1 `shiftL` i
   testBit w n    = naturalTestBit w (fromIntegral n)
   rotate         = error "Can't rotate a Natural"

instance Integral Natural where
   toInteger   = naturalToInteger
   quotRem a b = fromJust (naturalQuotRem a b)

instance Real Natural where
   toRational n = toRational (naturalToInteger n)

instance Enum Natural where
   toEnum   = naturalFromInteger . toInteger
   fromEnum = fromInteger . naturalToInteger

-- | Convert limb count into byte count
limbsToBytes# :: Int# -> Int#
limbsToBytes# i = i `iShiftL#` WSSHIFT#

-- | Convert byte count into limb count
bytesToLimbs# :: Int# -> Int#
bytesToLimbs# i = i `iShiftRL#` WSSHIFT#

-- | Count limbs
naturalLimbCount :: Natural -> Word
naturalLimbCount (Natural ba) = W# (int2Word# (naturalLimbCount# ba))

-- | Count limbs
naturalLimbCount# :: ByteArray# -> Int#
naturalLimbCount# ba = bytesToLimbs# (sizeofByteArray# ba)

-- | Count limbs
naturalLimbCountMutable# :: MutableByteArray# s-> State# s -> (# State# s, Int# #)
naturalLimbCountMutable# mba s = case getSizeofMutableByteArray# mba s of
   (# s2, sz #) -> (# s2, bytesToLimbs# sz #)

-- | Natural Zero
naturalZero :: Natural
naturalZero = runST $ ST $ \s0 ->
   case newByteArray# 0# s0 of
      (# s1, mba #) -> case unsafeFreezeByteArray# mba s1 of
            (# s2, ba #) -> (# s2, Natural ba #)

-- | Indicate if a natural is zero
naturalIsZero :: Natural -> Bool
naturalIsZero n = naturalLimbCount n == 0

-- | Indicate if a natural is one
naturalIsOne :: Natural -> Bool
naturalIsOne n@(Natural ba) = naturalLimbCount n == 1 && isTrue# (indexWordArray# ba 0# `eqWord#` 1##)

-- | Create a Natural from a Word
naturalFromWord :: Word -> Natural
naturalFromWord (W# 0##) = naturalZero
naturalFromWord (W# w)   = runST $ ST \s0 ->
   case newByteArray# WS# s0 of
      (# s1, mba #) -> case writeWordArray# mba 0# w s1 of
         s2 -> case unsafeFreezeByteArray# mba s2 of
            (# s3, ba #) -> (# s3, Natural ba #)

-- | Convert an Integer into a Natural
naturalFromInteger :: Integer -> Natural
naturalFromInteger k
   | k < 0     = error "naturalFromInteger: negative integer"
   | otherwise = go naturalZero 0 k
   where
      -- FIXME: be clever and allocate log2(k)/WSBITS ByteArray directly. Then
      -- fill it.
      go c _ 0 = c
      go c s n = go (c `naturalOr` naturalShiftL (naturalFromWord (fromInteger n)) s) (s + WSBITS) (n `shiftR` WSBITS)

-- | Convert a Natural into an Integer
naturalToInteger :: Natural -> Integer
naturalToInteger = go 0 . naturalLimbsMS
   where
      go c []     = c
      go c (x:xs) = go (c `shiftL` WSBITS .|. toInteger x) xs

-- | Show a Natural in hexadecimal
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

-- | Get the most significant limb
mostSignificantLimb# :: ByteArray# -> Word#
mostSignificantLimb# ba = indexWordArray# ba (sizeofByteArray# ba -# 1#)

-- Limbs: most significant first
naturalLimbsMS :: Natural -> [Word]
naturalLimbsMS n@(Natural ba)
   | naturalIsZero n = []
   | otherwise       = goLimbs (fromIntegral (naturalLimbCount n - 1))
   where
      goLimbs 0         = [W# (indexWordArray# ba 0#)]
      goLimbs i@(I# i#) = W# (indexWordArray# ba i#) : goLimbs (i-1)

-- Limbs: less significant first
naturalLimbsLS :: Natural -> [Word]
naturalLimbsLS n@(Natural ba)
   | naturalIsZero n = []
   | otherwise       = goLimbs 0
   where
      lc = fromIntegral (naturalLimbCount n)
      goLimbs i@(I# i#)
         | i == lc   = []
         | otherwise = W# (indexWordArray# ba i#) : goLimbs (i+1)

-- | Equality
naturalEq :: Natural -> Natural -> Bool
naturalEq n1 n2
   | naturalLimbCount n1 /= naturalLimbCount n2 = False
   | otherwise = all (uncurry (==)) (naturalLimbsMS n1 `zip` naturalLimbsMS n2)

-- | Compare
naturalCompare :: Natural -> Natural -> Ordering
naturalCompare n1 n2
   | lc1 > lc2 = GT
   | lc1 < lc2 = LT
   | otherwise = go (naturalLimbsMS n1) (naturalLimbsMS n2)
      where
         lc1 = naturalLimbCount n1
         lc2 = naturalLimbCount n2
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
      lc1      = fromIntegral $ naturalLimbCount n1
      lc2      = fromIntegral $ naturalLimbCount n2
      lc       = max lc1 lc2 + 1
      !(I# sz) = lc*WS

      addLimbsNoCarry l@(I# l#) mba s
         | l == lc-1 = shrinkMS mba 1# s
         | l >= lc1  = case copyByteArray# ba2 off mba off csz s of
               s2 -> shrinkMS mba 1# s2
         | l >= lc2  = case copyByteArray# ba1 off mba off csz s of
               s2 -> shrinkMS mba 1# s2
         | otherwise = case plusWord2# (indexWordArray# ba1 l#) (indexWordArray# ba2 l#) of
               (# c, r #) -> case writeWordArray# mba l# r s of
                  s2 -> addLimbs (l+1) c mba s2
            where
               !(I# off) = l*WS
               !(I# csz) = (lc2+1-l)*WS

      addLimbs l@(I# l#) c mba s
         | isTrue# (eqWord# c 0##) = addLimbsNoCarry l mba s
         | l == lc-1               = writeWordArray# mba l# c s
         | l >= lc1 = case plusWord2# c (indexWordArray# ba2 l#) of
               (# c2, r #) -> case writeWordArray# mba l# r s of
                  s2 -> addLimbs (l+1) c2 mba s2
         | l >= lc2 = case plusWord2# c (indexWordArray# ba1 l#) of
               (# c2, r #) -> case writeWordArray# mba l# r s of
                  s2 -> addLimbs (l+1) c2 mba s2
         | otherwise = case plusWord2# (indexWordArray# ba1 l#) (indexWordArray# ba2 l#) of
               (# c2, r #) -> case plusWord2# r c of
                  (# c3, r2 #) -> case writeWordArray# mba l# r2 s of
                     s2 -> addLimbs (l+1) (plusWord# c2 c3) mba s2

-- | Bitwise OR
naturalOr :: Natural -> Natural -> Natural
naturalOr n1@(Natural ba1) n2@(Natural ba2) = runST $ ST \s0 ->
      case newByteArray# sz s0 of
         (# s1, mba #) -> case go mba 0 lc1 lc2 s1 of
            s2 -> case unsafeFreezeByteArray# mba s2 of
               (# s3, ba #) -> (# s3, Natural ba #)

   where
      lc1      = fromIntegral $ naturalLimbCount n1
      lc2      = fromIntegral $ naturalLimbCount n2
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
            case writeWordArray# mba i# (indexWordArray# ba1 i# `or#` indexWordArray# ba2 i#) s of
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
      lc1      = fromIntegral $ naturalLimbCount n1
      lc2      = fromIntegral $ naturalLimbCount n2
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
            case writeWordArray# mba i# (indexWordArray# ba1 i# `xor#` indexWordArray# ba2 i#) s of
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
      lc1      = fromIntegral $ naturalLimbCount n1
      lc2      = fromIntegral $ naturalLimbCount n2
      lc       = min lc1 lc2
      !(I# sz) = lc*WS

      go :: MutableByteArray# s -> Int -> Int -> State# s -> State# s
      go _   _ 0 s = s
      go mba i c s =
            case writeWordArray# mba i# (indexWordArray# ba1 i# `and#` indexWordArray# ba2 i#) s of
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
            0 -> case copyByteArray# ba limbOffByte# mba 0# szOut# s1 of
                  s2 -> case unsafeFreezeByteArray# mba s2 of
                     (# s3, ba2 #) -> (# s3, Natural ba2 #)

            _ -> case go mba 0 s1 of
               s2 -> case unsafeFreezeByteArray# mba s2 of
                  (# s3, ba2 #) -> (# s3, Natural ba2 #)

   where
      lc                 = naturalLimbCount n
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
naturalShiftL :: Natural -> Word -> Natural
naturalShiftL n 0                   = n
naturalShiftL n _ | naturalIsZero n = n
naturalShiftL n@(Natural ba) k      = runST $ ST \s0 ->
      case newByteArray# szOut# s0 of
         (# s1, mba #) ->
            -- insert full empty limbs
            case setByteArray# mba 0# limbOffByte# 0# s1 of
               s2 -> case bitOff of
                  0 -> case copyByteArray# ba 0# mba limbOffByte# szIn# s2 of
                        s3 -> case unsafeFreezeByteArray# mba s3 of
                           (# s4, ba2 #) -> (# s4, Natural ba2 #)

                  _ -> case go mba 0 s2 of
                     s3 -> case unsafeFreezeByteArray# mba s3 of
                        (# s4, ba2 #) -> (# s4, Natural ba2 #)

   where
      lc                 = naturalLimbCount n
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
naturalMul :: Natural -> Natural -> Natural
naturalMul n1@(Natural ba1) n2@(Natural ba2)
   | naturalLimbCount n2 > naturalLimbCount n1 = naturalMul n2 n1 -- optimize loops
   | naturalIsZero n1 = n1
   | naturalIsZero n2 = n2
   | naturalIsOne  n1 = n2
   | naturalIsOne  n2 = n1
   | otherwise        = runST $ ST \s0 ->
      case newByteArray# sz# s0 of
         (# s1, mba #) -> case loopj0 mba 0 s1 of
               s2 -> case unsafeFreezeByteArray# mba s2 of
                  (# s3, ba #) -> (# s3, Natural ba #)

   where
      !lc1@(I# lc1#) = fromIntegral $ naturalLimbCount n1
      !lc2@(I# lc2#) = fromIntegral $ naturalLimbCount n2
      lc             = lc1 + lc2
      !(I# sz#)      = lc*WS

      loopj mba j@(I# j#) s
         | isTrue# (j# ==# lc2#) = s
         | otherwise             = case indexWordArray# ba2 j# of
                                       0## -> loopj mba (j+1) s
                                       vj  -> loopi mba vj j 0 0## s


      loopi mba vj j@(I# j#) i@(I# i#) k s
         | isTrue# (i# ==# lc1#) = case writeWordArray# mba (i# +# j#) k s of
                                       s2 -> loopj mba (j+1) s2
         | otherwise = case readWordArray# mba (i# +# j#) s of
            (# s2, wij #) ->
               let ui             = indexWordArray# ba1 i#
                   !(# k1,r1 #)   = timesWord2# ui vj
                   !(# k2,r2 #)   = plusWord2# wij k
                   !(# k3,wij' #) = plusWord2# r1 r2
                   k'             = plusWord# (plusWord# k1 k2) k3
               in case writeWordArray# mba (i# +# j#) wij' s2 of
                     s3 -> loopi mba vj j (i+1) k' s3

      -- loopj0 and loopi0 are executed first when we haven't initialized the
      -- result array.
      loopj0 mba j@(I# j#) s = case indexWordArray# ba2 j# of
                                 0## -> loopj0 mba (j+1) s -- n2 /= 0 so we can loop safely
                                 vj  -> loopi0 mba vj j 0 0## s

      loopi0 mba vj j@(I# j#) i@(I# i#) k s
         | isTrue# (i# ==# lc1#) = case writeWordArray# mba (i# +# j#) k s of
                                       s2 -> loopj mba (j+1) s2
         | otherwise =
               let ui             = indexWordArray# ba1 i#
                   !(# k1,r1 #)   = timesWord2# ui vj
                   !(# k2,wij #)  = plusWord2# r1 k
                   k'             = plusWord# k1 k2
               in case writeWordArray# mba (i# +# j#) wij s of
                     s2 -> loopi0 mba vj j (i+1) k' s2

-- | Natural bit test
naturalTestBit :: Natural -> Word -> Bool
naturalTestBit n@(Natural ba) i
      | q >= lc   = False
      | otherwise = testBit (W# (indexWordArray# ba q#)) (fromIntegral r)
   where
      lc       = naturalLimbCount n
      (q,r)    = quotRem i WSBITS
      !(I# q#) = fromIntegral q

-- | Subtract two naturals (classical algorithm)
naturalSub :: Natural -> Natural -> Maybe Natural
naturalSub n1@(Natural ba1) n2@(Natural ba2)
   | naturalIsZero n2      = Just n1
   | n1 < n2               = Nothing
   | otherwise             = runST $ ST \s0 ->
      case newByteArray# (sizeofByteArray# ba1) s0 of
         (# s1, mba #) -> case sub# ba1 ba2 mba s1 of
            s2 -> case unsafeFreezeByteArray# mba s2 of
               (# s3, ba #) -> (# s3, Just (Natural ba) #)

-- | Subtract two naturals (don't check if a >= b)
naturalSub_nocheck :: Natural -> Natural -> Natural
naturalSub_nocheck (Natural ba1) (Natural ba2) = runST $ ST \s0 ->
   case newByteArray# (sizeofByteArray# ba1) s0 of
      (# s1, mba #) -> case sub# ba1 ba2 mba s1 of
         s2 -> case unsafeFreezeByteArray# mba s2 of
            (# s3, ba #) -> (# s3, Natural ba #)

sub# :: ByteArray# -> ByteArray# -> MutableByteArray# s -> State# s -> State# s
sub# ba1 ba2 mba = go 0# 0# 0# 
   where
      !lc1# = naturalLimbCount# ba1
      !lc2# = naturalLimbCount# ba2

      go carry zeroMSCount i s
         | isTrue# (i ==# lc1#)               = shrinkMS mba zeroMSCount s
         | isTrue# (i >=# lc2#) , 0# <- carry = copyLimbs ba1 i mba i (lc1# -# i) s
         | otherwise =
            let
               ui = indexWordArray# ba1 i
               vi = case i <# lc2# of
                        0# -> 0##
                        _  -> indexWordArray# ba2 i
               !(# wi, carry', zeroMSCount' #) = subWordCTrail# ui vi carry zeroMSCount 
            in case writeWordArray# mba i wi s of
                  s2 -> go carry' zeroMSCount' (i +# 1#) s2

-- | Sub two naturals starting at specified indices
subAt# :: ByteArray# -> Int# -> ByteArray# -> Int# -> MutableByteArray# s -> Int# -> State# s -> State# s
subAt# ba1 idx1 ba2 idx2 mba idxo = go 0# 0# idx1 idx2 idxo
   where
      !lc1# = naturalLimbCount# ba1
      !lc2# = naturalLimbCount# ba2

      go carry zeroMSCount i1 i2 io s
         | isTrue# (i1 ==# lc1#)               = shrinkMS mba zeroMSCount s
         | isTrue# (i2 >=# lc2#) , 0# <- carry = copyLimbs ba1 i1 mba io (lc1# -# i1) s
         | otherwise =
            let
               ui = indexWordArray# ba1 i1
               vi = case i2 <# lc2# of
                        0# -> 0##
                        _  -> indexWordArray# ba2 i2
               !(# wi, carry', zeroMSCount' #) = subWordCTrail# ui vi carry zeroMSCount 
            in case writeWordArray# mba io wi s of
                  s2 -> go carry' zeroMSCount' (i1 +# 1#) (i2 +# 1#) (io +# 1#) s2

-- | Sub two naturals starting at specified indices
subAtInplace# :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> State# s -> State# s
subAtInplace# mba idx1 ba2 idx2 s1 = case naturalLimbCountMutable# mba s1 of
      (# s2, lc1# #) -> go 0# 0# idx1 idx2 s2
         where
            !lc2# = naturalLimbCount# ba2

            go carry zeroMSCount i1 i2 s
               | isTrue# (i1 ==# lc1#) = shrinkMS mba zeroMSCount s
               | otherwise = case readWordArray# mba i1 s of
                  (# s3, ui #) ->
                     let
                        vi = case i2 <# lc2# of
                                 0# -> 0##
                                 _  -> indexWordArray# ba2 i2
                        !(# wi, carry', zeroMSCount' #) = subWordCTrail# ui vi carry zeroMSCount 
                     in case writeWordArray# mba i1 wi s3 of
                           s4 -> go carry' zeroMSCount' (i1 +# 1#) (i2 +# 1#) s4

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


-- | Natural division returning (quotient,remainder)
--
-- See Note [Multi-Precision Division]
naturalQuotRem :: Natural -> Natural -> Maybe (Natural,Natural)
naturalQuotRem n1 n2@(Natural ba2)
   | naturalIsZero n2         = Nothing
   | naturalIsOne n2          = Just (n1, naturalZero)
   | lc1 < lc2                = Just (naturalZero, n1)
   | lc2 == 1                 = case divNby1# n1 (indexWordArray# ba2 0#) of
                                    Nothing    -> Nothing
                                    Just (q,r) -> Just (q, naturalFromWord r)

   | otherwise = Just (naturalWithNormalized naturalQuotRem_normalized n1 n2)
      where
         lc1      = naturalLimbCount n1
         lc2      = naturalLimbCount n2

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

-- | Compare 2-word naturals
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
divNby1# :: Natural -> Word# -> Maybe (Natural, Word)
divNby1# n@(Natural a) b = runST $ ST \s0 ->
   let
      lc       = fromIntegral $ naturalLimbCount n
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
            (# s3, ba #) -> (# s3, Just (Natural ba, W# r) #)



-- | Perform the division operation with normalized values.
--
-- Given fdiv, A and B:
--    * compute normalized values A' and B'
--    * compute (Q',R') = fdiv A' B'
--    * return unormalized (Q,R)
naturalWithNormalized :: (Natural -> Natural -> (Natural,Natural)) -> Natural -> Natural -> (Natural,Natural)
{-# INLINABLE naturalWithNormalized #-}
naturalWithNormalized f a b@(Natural ba)
   | isTrue# (k# `eqWord#` 0##) = f a b
   | otherwise                  = let (q,r') = f (naturalShiftL a k) (naturalShiftL b k)
                                  in (q, naturalShiftR r' k)
   where
      lc        = naturalLimbCount b
      !(I# lc#) = fromIntegral lc
      !k#       = clz# (indexWordArray# ba (lc# -# 1#))
      k         = W# k#

-- | Compare a and b where limbCount a >= limbCount b after dropping the
-- superfluous least-significant limbs of a so that limbCount a == limbCount b
--
-- Inputs: A (m+n limbs) and B (n limbs). Compare (A>>(m<<WSBITS)) and B
cmpMS :: Natural -> Natural -> Ordering
cmpMS a b = go (take n (naturalLimbsMS a)) (naturalLimbsMS b)
   where
      n  = fromIntegral $ naturalLimbCount b
      go (x:xs) (y:ys)
         | x > y     = GT
         | x < y     = LT
         | otherwise = go xs ys
      go ~[] ~[]     = EQ

-- | Compare a' and b where:
--    a' = dropLSL i a 
--    limbCount a' >= limbCount b
cmpDropped# :: MutableByteArray# s -> Int# -> ByteArray# -> State# s -> (# State# s, Ordering #)
cmpDropped# mba idx ba s1 = case naturalLimbCountMutable# mba s1 of
   (# s2, lca #) -> case compare (I# (lca -# idx)) (I# lcb) of
      GT -> (# s2, GT #)
      LT -> (# s2, LT #)
      EQ -> go (lca -# 1#) (lcb -# 1#) s2
   where
      !lcb = naturalLimbCount# ba
      go i j s =
         let
            !v = indexWordArray# ba j
         in case readWordArray# mba i s of
            (# s2, u #) -> if
               | isTrue# (u `gtWord#` v) -> (# s2, GT #)
               | isTrue# (u `ltWord#` v) -> (# s2, LT #)
               | isTrue# (j ==# 0#)      -> (# s2, EQ #)
               | otherwise               -> go (i -# 1#) (j -# 1#) s2


-- | Compute A-(B<<(m<<WSBITS)) where A (m+n limbs) and B (n limbs)
-- 
-- Requires:
--    a{m MSL} >= b{m MSL}
--subMS :: Natural -> Natural -> State# s -> (# State# s, MutableByteArray# s #)
--subMS a@(Natural aa) a@(Natural ba) s =
--   where
--      go zeroCount i#

-- | Natural quot/rem
--
-- Requires:
--    a > b
--    a and b normalized
naturalQuotRem_normalized :: Natural -> Natural -> (Natural,Natural)
naturalQuotRem_normalized a@(Natural ba1) b@(Natural ba2) = runST $ ST \s0 ->
      case initQ s0 of
         (# s1, qa #) -> case initR s1 of
            (# s2, ra #) -> case loop ra qa (m -# 1#) s2 of
               s3 -> case unsafeFreezeByteArray# qa s3 of
                  (# s4, q #) -> case unsafeFreezeByteArray# ra s4 of
                     (# s5, r #) -> (# s5, (Natural q, Natural r) #)

   where
      lc1 = naturalLimbCount# ba1
      n   = naturalLimbCount# ba2
      !m  = lc1 -# n

      cmpms   = cmpMS a b

      -- initialize q array with the initial carry if necessary
      !lcq = case cmpms of
         LT -> m
         _  -> m +# 1#

      initQ s = case newByteArray# (limbsToBytes# lcq) s of
         (# s2,mba #) -> case cmpms of
            LT -> (# s2,mba #)
            _  -> case writeWordArray# mba m 1## s2 of -- write the carry
                     s3 -> (# s3,mba #)

      -- initialize r array:
      --    * either with a
      --    * or with a - (b<<WSBITS*m)
      initR s = case newByteArray# (sizeofByteArray# ba1) s of
         (# s2,mba #) -> case cmpms of
            LT -> case copyLimbs ba1 0# mba 0# (sizeofByteArray# ba1) s2 of
               s3 -> (# s3,mba #)
            _  -> case copyLimbs ba1 0# mba 0# (limbsToBytes# m) s2 of
               s3 -> case subAt# ba1 m ba2 0# mba m s3 of
                  s4 -> (# s4, mba #)

      -- most significant limb of b
      bMSL = mostSignificantLimb# ba2

      !(W# maxLimb) = maxBound

      loop ra qa j s
         | 0# <- j   = computeQuot ra qa j s
         | otherwise = case computeQuot ra qa j s of
            s2 -> loop ra qa (j -# 1#) s2
   
      -- compute qj (in qa) and remainder in ra
      computeQuot ra qa j s1 = case computeQuot' s1 of
            (# s2, q #) -> writeWordArray# qa j q s2
         where
            !anj   = indexWordArray# ba1 (n +# j)
            !anjm1 = indexWordArray# ba1 (n +# j -# 1#)

            -- get a upper bound on the quotient by dividing the two MSL of r by
            -- the MSL of b
            !(# (# qe1,qe0 #), _ #) = div2by1_large# (# anj, anjm1 #) bMSL

            -- The quotient can't be a two-digit quotient as the prefix of r is
            -- inferior to b (either because of initR or because it is the
            -- remainder of the previous step which by definition is < b).
            -- If the upper bound quotient above has two digits, use the maximum
            -- 1-digit quotient instead.
            !qe = case qe1 of
                     0## -> qe0
                     _   -> maxLimb

            -- Now we can compute qe*b and check whether it is < (a >> j limbs)
            -- If not, it means we need to decrese qe and recheck until it is
            -- true.
            -- Note: we only compute qe*b once and then we subtract b from it.
            vinit          = naturalMul b (naturalFromWord (W# qe)) -- TODO: implement and use mulNby1#
            computeQuot' s = go qe vinit s

            go qc c@(Natural ca) s = case cmpDropped# ra j ca s of
               (# s2, LT #) -> go (qc `minusWord#` 1##) (naturalSub_nocheck c b) s2
               (# s2, EQ #) -> -- we can just drop the n higher MSL of a
                               case shrinkMS ra n s2 of
                                  s3 -> (# s3, qc #)
               (# s2, GT #) -> -- we need to compute the subtraction of b
                               case subAtInplace# ra j ca 0# s2 of
                                  s3 -> (# s3, qc #)
         


--
-- Note [Multi-Precision Division]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- See:
--    * "Multiple-Length Division Revisited: A Tour of the Minefield", Per
--    Brinch Hansen, 1992,
--    https://surface.syr.edu/cgi/viewcontent.cgi?article=1162&context=eecs_techreports
--
--    * "Fast Recursive Division", Burnikel and Ziegler, 1998
--
--
-- k/1 division
-- ------------
--
-- For any base B. Suppose we want to divide u by v where v is composed of a
-- single non-zero digit:
--    u = (u{n-1},...,u0){B}
--    v = (v0){B}
--
-- 
--  Let u' = (0,u{n-1},...,u0) (equivalent to u)
--
--  We perform the division of u' by v by folding from left to right the digits
--  of u' and by using the 2/1 division (first case). We obtain the digits of q
--  from left to right. The last remainder is the overall remainder.
