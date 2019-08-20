{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

-- | Multi-precision integers
--
-- Classical algorithms adapted from "The Art of Computer Programming, vol. 2,
-- Donald E. Knuth"
module Haskus.Integer
   ( Natural
   , naturalFromWord
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
   show = naturalShowHex

instance Eq Natural where
   (==) = naturalEq

instance Ord Natural where
   compare = naturalCompare

instance Num Natural where
   (+)      = naturalAdd
   (*)      = naturalMul
   x - y    = fromMaybe (error "Can't subtract these naturals") (naturalSub x y)
   abs      = id
   signum _ = naturalFromWord 1
   negate _ = error "Can't negate a Natural"

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

-- | Count limbs
naturalLimbCount :: Natural -> Word
naturalLimbCount (Natural ba) =
   W# (uncheckedShiftRL# (int2Word# (sizeofByteArray# ba)) 3#)

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
   | naturalLimbCount n1 > naturalLimbCount n2 = GT
   | naturalLimbCount n1 < naturalLimbCount n2 = LT
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
      lc1      = fromIntegral $ naturalLimbCount n1
      lc2      = fromIntegral $ naturalLimbCount n2
      lc       = max lc1 lc2 + 1
      !(I# sz) = lc*WS

      addLimbsNoCarry l@(I# l#) mba s
         | l == lc-1 = shrinkMutableByteArray# mba (sz -# 1#) s
         | l >= lc1  = case copyByteArray# ba2 off mba off csz s of
               s2 -> shrinkMutableByteArray# mba (sz -# 1#) s2
         | l >= lc2  = case copyByteArray# ba1 off mba off csz s of
               s2 -> shrinkMutableByteArray# mba (sz -# 1#) s2
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
   | naturalIsZero n1 = n1
   | naturalIsZero n2 = n2
   | naturalIsOne  n1 = n2
   | naturalIsOne  n2 = n1
   | otherwise        = runST $ ST \s0 ->
      case newByteArray# sz# s0 of
         (# s1, mba #) -> case setByteArray# mba 0# sz# 0# s1 of
            s2 -> case loopj mba 0 s2 of
               s3 -> case unsafeFreezeByteArray# mba s3 of
                  (# s4, ba #) -> (# s4, Natural ba #)

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
   | naturalIsZero n2 = Just n1
   | lc2 > lc1        = Nothing
   | otherwise        = runST $ ST \s0 ->
      case newByteArray# sz s0 of
         (# s1, mba #) -> go mba False 0 0 s1

   where
      lc1       = fromIntegral $ naturalLimbCount n1
      lc2       = fromIntegral $ naturalLimbCount n2
      !(I# sz)  = lc1*WS
      !(W# bm1) = maxBound

      go mba carry zeroTrail@(I# zt) i@(I# i#) s
         | i == lc1 =
            if not carry
               then case (case zeroTrail of
                  0 -> s
                  _  -> shrinkMutableByteArray# mba (sz -# zt) s
                  ) of
                     s2 -> case unsafeFreezeByteArray# mba s2 of
                        (# s3, ba #) -> (# s3, Just (Natural ba) #)
               else (# s, Nothing #)
         | i >= lc2 && not carry =
               let off       = i# `iShiftL#` WSSHIFT#
                   !(I# csz) = (lc1 - i) `shiftL` WSSHIFT
               in case copyByteArray# ba1 off mba off csz s of
                   s2 -> go mba False 0 lc1 s2
         | not carry =
            let
               ui = indexWordArray# ba1 i#
               vi = indexWordArray# ba2 i#
               !(# wi, c #) = subWordC# ui vi
            in case writeWordArray# mba i# wi s of
                  s2 -> case wi of
                     0## -> go mba (isTrue# c) (zeroTrail+1) (i+1) s2
                     _   -> go mba (isTrue# c) 0             (i+1) s2

         | otherwise =
            let
               ui = indexWordArray# ba1 i#
               vi = if i < lc2 then indexWordArray# ba2 i# else 0##
               !(# wi, c #) = subWordC# ui vi
            in case wi of
                  0## -> case writeWordArray# mba i# bm1 s of
                     s2 -> go mba True        0 (i+1) s2
                  1## -> case writeWordArray# mba i# 0## s of
                     s2 -> go mba (isTrue# c) (zeroTrail+1) (i+1) s2
                  _   -> case writeWordArray# mba i# (wi `minusWord#` 1##) s of
                     s2 -> go mba (isTrue# c) 0 (i+1) s2


-- | Natural division returning (quotient,remainder)
--
-- See Note [Multi-Precision Division]
naturalQuotRem :: Natural -> Natural -> Maybe (Natural,Natural)
naturalQuotRem n1@(Natural ba1) n2@(Natural ba2)
   | naturalIsZero n2         = Nothing
   | naturalIsOne n2          = Just (n1, naturalZero)
   | naturalLimbCount n2 == 1 = runST $ ST \s0 ->
      let
         lc       = fromIntegral (naturalLimbCount n1)
         !(I# sz) = lc * WS
         d        = indexWordArray# ba2 0#

         go mba i@(I# i#) trailing zeroTrail r s
            | i == 0 = case zeroTrail of
                        0        -> (# s, r #)
                        (!I# zt) -> case shrinkMutableByteArray# mba (sz -# zt) s of
                           s2 -> (# s2, r #)
            | otherwise =
               let
                  off         = i# -# 1#
                  n           = indexWordArray# ba1 off
                  !(# q,r' #) = quotRemWord2# r n d
                  qZero       = isTrue# (q `eqWord#` 0##)
                  trailing'   = trailing && qZero
                  zeroTrail'  = if trailing' then zeroTrail+1 else zeroTrail
               in case writeWordArray# mba off q s of
                     s2 -> go mba (I# off) trailing' zeroTrail' r' s2
      in case newByteArray# sz s0 of
         (# s1, mba #) -> case go mba lc True 0 0## s1 of
            (# s2, r #) -> case unsafeFreezeByteArray# mba s2 of
               (# s3, ba #) -> case naturalFromWord (W# r) of
                  r' -> (# s3, Just (Natural ba, r') #)

   | otherwise = error "Long-division not implemented"

--
-- Note [Multi-Precision Division]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- See "Multiple-Length Division Revisited: A Tour of the Minefield", Per Brinch
-- Hansen, 1992,
-- https://surface.syr.edu/cgi/viewcontent.cgi?article=1162&context=eecs_techreports
--
-- 1/1 division
-- ------------
--
-- For any base B. Suppose we want to divide u by v where u and v are composed
-- of 1 digit each:
--    u = (u0){B}
--    v = (v0){B}
--    v /= 0
--
-- We want to compute (q,r) so that: u = q*v + r with r < v
--
-- Number of digits:
--    countDigits (r) <= 1 (proof: r < v so r is composed of at most one digit)
--    countDigits (q) <= 1 (proof: if q had more than one digit, u would have more than one digit)
--
-- 2/1 division
-- ------------
--
-- For any base B. Suppose we want to divide u by v where u and v are composed
-- of 2 and 1 digits respectively:
--    u = (u1,u0){B}
--    v = (v0){B}
--    v /= 0
--
-- We want to compute (q,r) so that: u = q*v + r with r < v
--    r=(r0){B}
--
-- Suppose v0 = 1 (smallest non-zero value). Then q = u. (x `div` y) is
-- decreasing over y, hence q has at most 2 digits in base B.
--
--    q = (q1,q0){B}
--
--    u1*B+u0 = q1*B*v0 + q0*v0 + r0
--    u1*B    = q1*B*v0 + q0*v0 + r0 - u0
--    u1      = q1*v0   + K/B       where K = q0*v0+r0-u0
--
-- First case: u1 < v0
-- ~~~~~~~~~~~~~~~~~~~
--    q1 = 0 hence countDigits(q) <= 1
--
--    We have primops to compute q0 and r0 in this case (e.g. X86_64 DIV
--    instruction, `quotRemWord2#` primop)
--
-- Second case: u1 >= v0
-- ~~~~~~~~~~~~~~~~~~~~~
--
--    q1 /= 0 hence countDigits(q) == 2
--
--    let u1 = q'*v0 + r' where r' < v0 using 1/1 division.
--
--    u1*B+u0           = q1*B*v0 + q0*v0 + r
--    (q'*v0+r')*B+u0   = q1*B*v0 + q0*v0 + r
--    q'*v0*B + r'*B+u0 = q1*B*v0 + q0*v0 + r
--    r'*B+u0           = (q1-q')*B*v0 + q0*v0 + r
--    r'*B+u0           = ((q1-q')*B + q0)*v0 + r
--
--    let u' = (r',u0){B}
--    The 2/1 division of u' by v=(v0){B} (with r' < v0) gives us q0, r and:
--       q' = q1
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
--
