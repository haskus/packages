{-# LANGUAGE CPP #-}

-- | Writer
--
-- A Writer is a function that writes something into memory at a given address
-- and returns the new address. As such, Writers can be composed.
--
-- Note: in Haskell-verse, a Writer may be called a "Builder". Not all Builders
-- use this function-based implementation. Many of them are ADTs with
-- constructors for Words, ByteString, etc. that can then be interpreted to be
-- written in memory. The function-based approach is usually more efficient (no
-- need to allocate intermediate ADT values).
module Haskus.Memory.Writer
  ( -- * Writer
    Writer (..)
  , WriterIO
  , runWriter#
  , runWriterIO
  , runWriterST
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
  -- * Sized writer
  , SizedWriter (..)
  , fromSizedWriter
  , sizedWriterEq
  , swriteU8#
  , swriteU16#
  , swriteU16BE#
  , swriteU16LE#
  , swriteU32#
  , swriteU32BE#
  , swriteU32LE#
  , swriteU64#
  , swriteU64BE#
  , swriteU64LE#
  , swriteI8#
  , swriteI16#
  , swriteI16BE#
  , swriteI16LE#
  , swriteI32#
  , swriteI32BE#
  , swriteI32LE#
  , swriteI64#
  , swriteI64BE#
  , swriteI64LE#
  -- ** Lifted
  , swriteU8
  , swriteU16
  , swriteU16BE
  , swriteU16LE
  , swriteU32
  , swriteU32BE
  , swriteU32LE
  , swriteU64
  , swriteU64BE
  , swriteU64LE
  , swriteI8
  , swriteI16
  , swriteI16BE
  , swriteI16LE
  , swriteI32
  , swriteI32BE
  , swriteI32LE
  , swriteI64
  , swriteI64BE
  , swriteI64LE
  )
where

import qualified GHC.Exts as E
import GHC.Exts
import GHC.IO
import GHC.ST

import Haskus.Binary.Endianness (hostEndianness, Endianness(..))
import Haskus.Binary.ByteSwap
import Haskus.Binary.Word
import Haskus.Binary.Int
import Haskus.Binary.Cast

-- | A writer writes a value at the given address and return the address after
-- the written value. The value to write is captured by the closure.
newtype Writer s
  = Writer (Addr# -> State# s -> (# State# s, Addr# #))

instance Semigroup (Writer s) where
  Writer f <> Writer g = Writer \addr s ->
    case f addr s of
      (# s', addr' #) -> g addr' s'

instance Monoid (Writer s) where
  {-# INLINE mempty #-}
  mempty = Writer \addr s -> (# s, addr #)

runWriter# :: Writer s -> Addr# -> State# s -> (# State# s, Addr# #)
runWriter# (Writer f) = f

type WriterIO = Writer RealWorld

runWriterIO :: Ptr a -> WriterIO -> IO (Ptr a)
runWriterIO (E.Ptr addr) w = IO \s -> case runWriter# w addr s of
  (# s', addr' #) -> (# s', Ptr addr' #)

runWriterST :: Ptr a -> Writer s -> ST s (Ptr a)
runWriterST (E.Ptr addr) w = ST \s -> case runWriter# w addr s of
  (# s', addr' #) -> (# s', Ptr addr' #)


-- | A writer that carries the size of the written value in bytes.
data SizedWriter s = SizedWriter
  { sizedWriterSize :: !U#          -- ^ Number of bytes that will be written by the writer
  , sizedWriter     :: !(Writer s)  -- ^ Writer
  }

instance Semigroup (SizedWriter s) where
  SizedWriter n f <> SizedWriter m g
    = SizedWriter (n `plusWord#` m) (f <> g)

instance Monoid (SizedWriter s) where
  {-# INLINE mempty #-}
  mempty = SizedWriter 0## mempty
  {-# INLINE mconcat #-}
  mconcat sws = SizedWriter (sum_size 0## sws) (sum_writer mempty sws)
    where
      sum_size n = \case
        [] -> n
        SizedWriter s _ : sw -> sum_size (n `plusWord#` s) sw
      sum_writer f = \case
        [] -> f
        SizedWriter _ g : sw -> sum_writer (f <> g) sw


-- | Write a SizedWriter into a freshly allocated array
fromSizedWriter :: SizedWriter s -> State# s -> (# State# s, ByteArray# #)
fromSizedWriter (SizedWriter sz w) s0 =
  let !(# s1, marr #) = newPinnedByteArray# (iFromU# sz) s0
      !(# s2, arr  #) = unsafeFreezeByteArray# marr s1
      !addr = byteArrayContents# arr
#if MIN_VERSION_base(4,19,0)
  in keepAlive# arr s2 \s ->
      case runWriter# w addr s of
        (# s3, _ #) -> (# s3, arr #)
#else
  -- Before GHC 9.8, keepAlive# type isn't generalized.
  -- It should be safe to unsafeCoerce# the tokens though.
  in keepAlive# arr (unsafeCoerce# s2) \s ->
      case runWriter# w addr (unsafeCoerce# s) of
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

----------------------------
-- Writers
----------------------------

writeU8# :: U8# -> Writer s
writeU8# w = Writer \addr s ->
  case E.writeWord8OffAddr# addr 0# w s of
    s' -> (# s', plusAddr# addr 1# #)

writeU16# :: U16# -> Writer s
writeU16# w = Writer \addr s ->
  case E.writeWord16OffAddr# addr 0# w s of
    s' -> (# s', plusAddr# addr 2# #)

writeU16BE# :: U16# -> Writer s
writeU16BE# w = case hostEndianness of
  BigEndian -> writeU16# w
  LittleEndian -> writeU16# (byteSwapU16# w)

writeU16LE# :: U16# -> Writer s
writeU16LE# w = case hostEndianness of
  LittleEndian -> writeU16# w
  BigEndian -> writeU16# (byteSwapU16# w)

writeU32# :: U32# -> Writer s
writeU32# w = Writer \addr s ->
  case E.writeWord32OffAddr# addr 0# w s of
    s' -> (# s', plusAddr# addr 4# #)

writeU32BE# :: U32# -> Writer s
writeU32BE# w = case hostEndianness of
  BigEndian -> writeU32# w
  LittleEndian -> writeU32# (byteSwapU32# w)

writeU32LE# :: U32# -> Writer s
writeU32LE# w = case hostEndianness of
  LittleEndian -> writeU32# w
  BigEndian -> writeU32# (byteSwapU32# w)

writeU64# :: U64# -> Writer s
writeU64# w = Writer \addr s ->
  case E.writeWord64OffAddr# addr 0# w s of
    s' -> (# s', plusAddr# addr 8# #)

writeU64BE# :: U64# -> Writer s
writeU64BE# w = case hostEndianness of
  BigEndian -> writeU64# w
  LittleEndian -> writeU64# (byteSwapU64# w)

writeU64LE# :: U64# -> Writer s
writeU64LE# w = case hostEndianness of
  LittleEndian -> writeU64# w
  BigEndian -> writeU64# (byteSwapU64# w)

writeI8# :: I8# -> Writer s
writeI8# w = Writer \addr s ->
  case E.writeInt8OffAddr# addr 0# w s of
    s' -> (# s', plusAddr# addr 1# #)

writeI16# :: I16# -> Writer s
writeI16# w = Writer \addr s ->
  case E.writeInt16OffAddr# addr 0# w s of
    s' -> (# s', plusAddr# addr 2# #)

writeI16BE# :: I16# -> Writer s
writeI16BE# w = case hostEndianness of
  BigEndian -> writeI16# w
  LittleEndian -> writeI16# (byteSwapI16# w)

writeI16LE# :: I16# -> Writer s
writeI16LE# w = case hostEndianness of
  LittleEndian -> writeI16# w
  BigEndian -> writeI16# (byteSwapI16# w)

writeI32# :: I32# -> Writer s
writeI32# w = Writer \addr s ->
  case E.writeInt32OffAddr# addr 0# w s of
    s' -> (# s', plusAddr# addr 4# #)

writeI32BE# :: I32# -> Writer s
writeI32BE# w = case hostEndianness of
  BigEndian -> writeI32# w
  LittleEndian -> writeI32# (byteSwapI32# w)

writeI32LE# :: I32# -> Writer s
writeI32LE# w = case hostEndianness of
  LittleEndian -> writeI32# w
  BigEndian -> writeI32# (byteSwapI32# w)

writeI64# :: I64# -> Writer s
writeI64# w = Writer \addr s ->
  case E.writeInt64OffAddr# addr 0# w s of
    s' -> (# s', plusAddr# addr 8# #)

writeI64BE# :: I64# -> Writer s
writeI64BE# w = case hostEndianness of
  BigEndian -> writeI64# w
  LittleEndian -> writeI64# (byteSwapI64# w)

writeI64LE# :: I64# -> Writer s
writeI64LE# w = case hostEndianness of
  LittleEndian -> writeI64# w
  BigEndian -> writeI64# (byteSwapI64# w)

----------------------------
-- Sized Writers
----------------------------

swriteU8# :: U8# -> SizedWriter s
swriteU8# w = SizedWriter 1## (writeU8# w)

swriteU16# :: U16# -> SizedWriter s
swriteU16# w = SizedWriter 2## (writeU16# w)

swriteU16BE# :: U16# -> SizedWriter s
swriteU16BE# w = SizedWriter 2## (writeU16BE# w)

swriteU16LE# :: U16# -> SizedWriter s
swriteU16LE# w = SizedWriter 2## (writeU16LE# w)

swriteU32# :: U32# -> SizedWriter s
swriteU32# w = SizedWriter 4## (writeU32# w)

swriteU32BE# :: U32# -> SizedWriter s
swriteU32BE# w = SizedWriter 4## (writeU32BE# w)

swriteU32LE# :: U32# -> SizedWriter s
swriteU32LE# w = SizedWriter 4## (writeU32LE# w)

swriteU64# :: U64# -> SizedWriter s
swriteU64# w = SizedWriter 8## (writeU64# w)

swriteU64BE# :: U64# -> SizedWriter s
swriteU64BE# w = SizedWriter 8## (writeU64BE# w)

swriteU64LE# :: U64# -> SizedWriter s
swriteU64LE# w = SizedWriter 8## (writeU64LE# w)

swriteI8# :: I8# -> SizedWriter s
swriteI8# w = SizedWriter 1## (writeI8# w)

swriteI16# :: I16# -> SizedWriter s
swriteI16# w = SizedWriter 2## (writeI16# w)

swriteI16BE# :: I16# -> SizedWriter s
swriteI16BE# w = SizedWriter 2## (writeI16BE# w)

swriteI16LE# :: I16# -> SizedWriter s
swriteI16LE# w = SizedWriter 2## (writeI16LE# w)

swriteI32# :: I32# -> SizedWriter s
swriteI32# w = SizedWriter 4## (writeI32# w)

swriteI32BE# :: I32# -> SizedWriter s
swriteI32BE# w = SizedWriter 4## (writeI32BE# w)

swriteI32LE# :: I32# -> SizedWriter s
swriteI32LE# w = SizedWriter 4## (writeI32LE# w)

swriteI64# :: I64# -> SizedWriter s
swriteI64# w = SizedWriter 8## (writeI64# w)

swriteI64BE# :: I64# -> SizedWriter s
swriteI64BE# w = SizedWriter 8## (writeI64BE# w)

swriteI64LE# :: I64# -> SizedWriter s
swriteI64LE# w = SizedWriter 8## (writeI64LE# w)



swriteU8 :: U8 -> SizedWriter s
swriteU8 (U8 w) = swriteU8# w

swriteU16 :: U16 -> SizedWriter s
swriteU16 (U16 w) =  swriteU16# w

swriteU16BE :: U16 -> SizedWriter s
swriteU16BE (U16 w) = swriteU16BE# w

swriteU16LE :: U16 -> SizedWriter s
swriteU16LE (U16 w) = swriteU16LE# w

swriteU32 :: U32 -> SizedWriter s
swriteU32 (U32 w) = swriteU32# w

swriteU32BE :: U32 -> SizedWriter s
swriteU32BE (U32 w) = swriteU32BE# w

swriteU32LE :: U32 -> SizedWriter s
swriteU32LE (U32 w) = swriteU32LE# w

swriteU64 :: U64 -> SizedWriter s
swriteU64 (U64 w) = swriteU64# w

swriteU64BE :: U64 -> SizedWriter s
swriteU64BE (U64 w) = swriteU64BE# w

swriteU64LE :: U64 -> SizedWriter s
swriteU64LE (U64 w) = swriteU64LE# w

swriteI8 :: I8 -> SizedWriter s
swriteI8 (I8 w) = swriteI8# w

swriteI16 :: I16 -> SizedWriter s
swriteI16 (I16 w) = swriteI16# w

swriteI16BE :: I16 -> SizedWriter s
swriteI16BE (I16 w) = swriteI16BE# w

swriteI16LE :: I16 -> SizedWriter s
swriteI16LE (I16 w) = swriteI16LE# w

swriteI32 :: I32 -> SizedWriter s
swriteI32 (I32 w) = swriteI32# w

swriteI32BE :: I32 -> SizedWriter s
swriteI32BE (I32 w) = swriteI32BE# w

swriteI32LE :: I32 -> SizedWriter s
swriteI32LE (I32 w) = swriteI32LE# w

swriteI64 :: I64 -> SizedWriter s
swriteI64 (I64 w) = swriteI64# w

swriteI64BE :: I64 -> SizedWriter s
swriteI64BE (I64 w) = swriteI64BE# w

swriteI64LE :: I64 -> SizedWriter s
swriteI64LE (I64 w) = swriteI64LE# w
