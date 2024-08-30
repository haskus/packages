{-# LANGUAGE CPP #-}

-- | SizedWriter: a Writer which carries the size of what it writes.
module Haskus.Memory.Writer.SizedWriter
  ( SizedWriter (SizedWriter)
  , sizedWriterSize
  , sizedWriter
  , fromSizedWriter
  , sizedWriterEq
  , writeU8#
  , writeU16#
  , writeU16BE#
  , writeU16LE#
  , writeU32#
  , writeU32BE#
  , writeU32LE#
  , writeU64#
  , writeU64BE#
  , writeU64LE#
  , writeI8#
  , writeI16#
  , writeI16BE#
  , writeI16LE#
  , writeI32#
  , writeI32BE#
  , writeI32LE#
  , writeI64#
  , writeI64BE#
  , writeI64LE#
  -- ** Lifted
  , writeU8
  , writeU16
  , writeU16BE
  , writeU16LE
  , writeU32
  , writeU32BE
  , writeU32LE
  , writeU64
  , writeU64BE
  , writeU64LE
  , writeI8
  , writeI16
  , writeI16BE
  , writeI16LE
  , writeI32
  , writeI32BE
  , writeI32LE
  , writeI64
  , writeI64BE
  , writeI64LE
  )
where

import GHC.Exts

import Haskus.Binary.Word
import Haskus.Binary.Int
import Haskus.Binary.Cast
import qualified Haskus.Memory.Writer as W

-- Select between the Datatype implementation (that may allocate SizedWriter
-- objects just to pass two values) or the church-encoded version (that is just
-- a function returning unboxed values, i.e. no allocation).
#if 0
data SizedWriter s = SizedWriter
  { sizedWriterSize :: !U#
      -- ^ The number of bytes that will be written by the writer
  , sizedWriter     :: !(W.Writer s)
      -- ^ The Writer associated with this SizedWriter
  }
#else
-- | A writer that carries the size of the written value in bytes.
newtype SizedWriter s
  = SizedWriter' ( (##) -> (# U#, W.Writer s #) )

-- | The number of bytes that will be written by the writer
sizedWriterSize :: SizedWriter s -> U#
sizedWriterSize (SizedWriter' f) = case f (##) of (# u, _ #) -> u

-- | The Writer associated with this SizedWriter
sizedWriter :: SizedWriter s -> W.Writer s
sizedWriter (SizedWriter' f) = case f (##) of (# _, w #) -> w

unpackSizedWriter :: SizedWriter s -> (# U#, W.Writer s #)
unpackSizedWriter (SizedWriter' f) = f (##)

{-# COMPLETE SizedWriter #-}
pattern SizedWriter :: U# -> W.Writer s -> SizedWriter s
pattern SizedWriter u w <- (unpackSizedWriter -> (# u, w #))
  where
    SizedWriter u w = SizedWriter' \_ -> (# u, w #)
#endif

instance Semigroup (SizedWriter s) where
  SizedWriter n f <> SizedWriter m g
    = SizedWriter (n `plusWord#` m) (f <> g)

instance Monoid (SizedWriter s) where
  {-# INLINE mempty #-}
  mempty = SizedWriter 0## mempty


-- | Write a SizedWriter into a freshly allocated array
fromSizedWriter :: SizedWriter s -> State# s -> (# State# s, ByteArray# #)
fromSizedWriter (SizedWriter sz w) s0 =
  let !(# s1, marr #) = newPinnedByteArray# (iFromU# sz) s0
      !(# s2, arr  #) = unsafeFreezeByteArray# marr s1
      !addr = byteArrayContents# arr
#if MIN_VERSION_base(4,19,0)
  in keepAlive# arr s2 \s ->
      case W.runWriter# w addr s of
        (# s3, _ #) -> (# s3, arr #)
#else
  -- Before GHC 9.8, keepAlive# type isn't generalized.
  -- It should be safe to unsafeCoerce# the tokens though.
  in keepAlive# arr (unsafeCoerce# s2) \s ->
      case W.runWriter# w addr (unsafeCoerce# s) of
        (# s4, _ #) -> (# s4, arr #)
#endif

-- | Equality test for SizedWriter
--
-- Not very efficient. Use it only for tests!
sizedWriterEq :: SizedWriter RealWorld -> SizedWriter RealWorld -> Bool
sizedWriterEq w1 w2
  | isTrue# (sizedWriterSize w1 `neWord#` sizedWriterSize w2) = False
  | otherwise = runRW# \s0 ->
      let !(# s1, b1 #) = fromSizedWriter w1 s0
          !(# _, b2 #) = fromSizedWriter w2 s1
          !n = iFromU# (sizedWriterSize w1)
          !b = compareByteArrays# b1 0# b2 0# n
      in isTrue# (b ==# 0#)


writeU8# :: U8# -> SizedWriter s
writeU8# w = SizedWriter 1## (W.writeU8# w)

writeU16# :: U16# -> SizedWriter s
writeU16# w = SizedWriter 2## (W.writeU16# w)

writeU16BE# :: U16# -> SizedWriter s
writeU16BE# w = SizedWriter 2## (W.writeU16BE# w)

writeU16LE# :: U16# -> SizedWriter s
writeU16LE# w = SizedWriter 2## (W.writeU16LE# w)

writeU32# :: U32# -> SizedWriter s
writeU32# w = SizedWriter 4## (W.writeU32# w)

writeU32BE# :: U32# -> SizedWriter s
writeU32BE# w = SizedWriter 4## (W.writeU32BE# w)

writeU32LE# :: U32# -> SizedWriter s
writeU32LE# w = SizedWriter 4## (W.writeU32LE# w)

writeU64# :: U64# -> SizedWriter s
writeU64# w = SizedWriter 8## (W.writeU64# w)

writeU64BE# :: U64# -> SizedWriter s
writeU64BE# w = SizedWriter 8## (W.writeU64BE# w)

writeU64LE# :: U64# -> SizedWriter s
writeU64LE# w = SizedWriter 8## (W.writeU64LE# w)

writeI8# :: I8# -> SizedWriter s
writeI8# w = SizedWriter 1## (W.writeI8# w)

writeI16# :: I16# -> SizedWriter s
writeI16# w = SizedWriter 2## (W.writeI16# w)

writeI16BE# :: I16# -> SizedWriter s
writeI16BE# w = SizedWriter 2## (W.writeI16BE# w)

writeI16LE# :: I16# -> SizedWriter s
writeI16LE# w = SizedWriter 2## (W.writeI16LE# w)

writeI32# :: I32# -> SizedWriter s
writeI32# w = SizedWriter 4## (W.writeI32# w)

writeI32BE# :: I32# -> SizedWriter s
writeI32BE# w = SizedWriter 4## (W.writeI32BE# w)

writeI32LE# :: I32# -> SizedWriter s
writeI32LE# w = SizedWriter 4## (W.writeI32LE# w)

writeI64# :: I64# -> SizedWriter s
writeI64# w = SizedWriter 8## (W.writeI64# w)

writeI64BE# :: I64# -> SizedWriter s
writeI64BE# w = SizedWriter 8## (W.writeI64BE# w)

writeI64LE# :: I64# -> SizedWriter s
writeI64LE# w = SizedWriter 8## (W.writeI64LE# w)



writeU8 :: U8 -> SizedWriter s
writeU8 (U8 w) = writeU8# w

writeU16 :: U16 -> SizedWriter s
writeU16 (U16 w) =  writeU16# w

writeU16BE :: U16 -> SizedWriter s
writeU16BE (U16 w) = writeU16BE# w

writeU16LE :: U16 -> SizedWriter s
writeU16LE (U16 w) = writeU16LE# w

writeU32 :: U32 -> SizedWriter s
writeU32 (U32 w) = writeU32# w

writeU32BE :: U32 -> SizedWriter s
writeU32BE (U32 w) = writeU32BE# w

writeU32LE :: U32 -> SizedWriter s
writeU32LE (U32 w) = writeU32LE# w

writeU64 :: U64 -> SizedWriter s
writeU64 (U64 w) = writeU64# w

writeU64BE :: U64 -> SizedWriter s
writeU64BE (U64 w) = writeU64BE# w

writeU64LE :: U64 -> SizedWriter s
writeU64LE (U64 w) = writeU64LE# w

writeI8 :: I8 -> SizedWriter s
writeI8 (I8 w) = writeI8# w

writeI16 :: I16 -> SizedWriter s
writeI16 (I16 w) = writeI16# w

writeI16BE :: I16 -> SizedWriter s
writeI16BE (I16 w) = writeI16BE# w

writeI16LE :: I16 -> SizedWriter s
writeI16LE (I16 w) = writeI16LE# w

writeI32 :: I32 -> SizedWriter s
writeI32 (I32 w) = writeI32# w

writeI32BE :: I32 -> SizedWriter s
writeI32BE (I32 w) = writeI32BE# w

writeI32LE :: I32 -> SizedWriter s
writeI32LE (I32 w) = writeI32LE# w

writeI64 :: I64 -> SizedWriter s
writeI64 (I64 w) = writeI64# w

writeI64BE :: I64 -> SizedWriter s
writeI64BE (I64 w) = writeI64BE# w

writeI64LE :: I64 -> SizedWriter s
writeI64LE (I64 w) = writeI64LE# w
