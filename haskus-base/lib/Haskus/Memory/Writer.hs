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

import qualified GHC.Exts as E
import GHC.Exts
import GHC.IO
import GHC.ST

import Haskus.Binary.Endianness (hostEndianness, Endianness(..))
import Haskus.Binary.ByteSwap
import Haskus.Binary.Word
import Haskus.Binary.Int

-- | A writer writes a value at the given address and return the address after
-- the written value. The value to write is captured by the closure.
newtype Writer s
  = Writer (Addr# -> State# s -> (# State# s, Addr# #))

instance Semigroup (Writer s) where
  Writer f <> Writer g = Writer \addr0 s0 ->
    let !(# s1, addr1 #) = f addr0 s0
    in g addr1 s1

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

writeU8 :: U8 -> Writer s
writeU8 (U8 w) = writeU8# w

writeU16 :: U16 -> Writer s
writeU16 (U16 w) =  writeU16# w

writeU16BE :: U16 -> Writer s
writeU16BE (U16 w) = writeU16BE# w

writeU16LE :: U16 -> Writer s
writeU16LE (U16 w) = writeU16LE# w

writeU32 :: U32 -> Writer s
writeU32 (U32 w) = writeU32# w

writeU32BE :: U32 -> Writer s
writeU32BE (U32 w) = writeU32BE# w

writeU32LE :: U32 -> Writer s
writeU32LE (U32 w) = writeU32LE# w

writeU64 :: U64 -> Writer s
writeU64 (U64 w) = writeU64# w

writeU64BE :: U64 -> Writer s
writeU64BE (U64 w) = writeU64BE# w

writeU64LE :: U64 -> Writer s
writeU64LE (U64 w) = writeU64LE# w

writeI8 :: I8 -> Writer s
writeI8 (I8 w) = writeI8# w

writeI16 :: I16 -> Writer s
writeI16 (I16 w) = writeI16# w

writeI16BE :: I16 -> Writer s
writeI16BE (I16 w) = writeI16BE# w

writeI16LE :: I16 -> Writer s
writeI16LE (I16 w) = writeI16LE# w

writeI32 :: I32 -> Writer s
writeI32 (I32 w) = writeI32# w

writeI32BE :: I32 -> Writer s
writeI32BE (I32 w) = writeI32BE# w

writeI32LE :: I32 -> Writer s
writeI32LE (I32 w) = writeI32LE# w

writeI64 :: I64 -> Writer s
writeI64 (I64 w) = writeI64# w

writeI64BE :: I64 -> Writer s
writeI64BE (I64 w) = writeI64BE# w

writeI64LE :: I64 -> Writer s
writeI64LE (I64 w) = writeI64LE# w
