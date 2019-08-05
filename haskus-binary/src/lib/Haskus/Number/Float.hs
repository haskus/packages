{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | IEEE754 floating-point numbers
module Haskus.Number.Float
   ( Float32
   , Float64
   , float32ToWord32
   , float64ToWord64
   , word32ToFloat32
   , word64ToFloat64
   )
where

import Haskus.Number.Word
import GHC.Float
import GHC.ST
import GHC.Prim

type Float32 = Float
type Float64 = Double


-- | Convert a Word32 into a Float32
word32ToFloat32 :: Word32 -> Float32
{-# INLINE word32ToFloat32 #-}
word32ToFloat32 (W32# x) = runST $ ST $ \s1 ->
   case newByteArray# 4# s1             of { (# s2, mbarr #) ->
   case writeWord32Array# mbarr 0# x s2 of { s3              ->
   case readFloatArray# mbarr 0# s3     of { (# s4, f #)     ->
      (# s4, F# f #) }}}

-- | Convert a Float32 into a Word32
float32ToWord32 :: Float32 -> Word32
{-# INLINE float32ToWord32 #-}
float32ToWord32 (F# x) = runST $ ST $ \s1 ->
   case newByteArray# 4# s1            of { (# s2, mbarr #) ->
   case writeFloatArray# mbarr 0# x s2 of { s3              ->
   case readWord32Array# mbarr 0# s3   of { (# s4, w #)     ->
      (# s4, W32# w #) }}}

-- | Convert a Word64 into a Float64
word64ToFloat64 :: Word64 -> Float64
{-# INLINE word64ToFloat64 #-}
word64ToFloat64 (W64# x) = runST $ ST $ \s1 ->
   case newByteArray# 8# s1             of { (# s2, mbarr #) ->
   case writeWord64Array# mbarr 0# x s2 of { s3              ->
   case readDoubleArray# mbarr 0# s3    of { (# s4, f #)     ->
      (# s4, D# f #) }}}

-- | Convert a Word64 into a Float64
float64ToWord64 :: Float64 -> Word64
{-# INLINE float64ToWord64 #-}
float64ToWord64 (D# x) = runST $ ST $ \s1 ->
   case newByteArray# 8# s1             of { (# s2, mbarr #) ->
   case writeDoubleArray# mbarr 0# x s2 of { s3              ->
   case readWord64Array# mbarr 0# s3    of { (# s4, w #)     ->
      (# s4, W64# w #) }}}
