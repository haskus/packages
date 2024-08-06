module Haskus.Memory.Writer
  ( Writer (..)
  , writeWord8#
  , writeWord16#
  , writeWord16BE#
  , writeWord16LE#
  , writeWord32#
  , writeWord32BE#
  , writeWord32LE#
  , writeWord64#
  , writeWord64BE#
  , writeWord64LE#
  , writeInt8#
  , writeInt16#
  , writeInt16BE#
  , writeInt16LE#
  , writeInt32#
  , writeInt32BE#
  , writeInt32LE#
  , writeInt64#
  , writeInt64BE#
  , writeInt64LE#
  )
where

import qualified GHC.Exts as E
import GHC.Exts hiding (byteSwap16#, byteSwap32#)
import Haskus.Binary.Endianness (hostEndianness, Endianness(..))

-- FIXME: move these to another module
-- TODO: fix GHC's byteSwap16/32 to take Word16#/Word32#
byteSwapWord16# :: Word16# -> Word16#
byteSwapWord16# w = E.wordToWord16# (E.byteSwap16# (E.word16ToWord# w))

byteSwapWord32# :: Word32# -> Word32#
byteSwapWord32# w = E.wordToWord32# (E.byteSwap32# (E.word32ToWord# w))

byteSwapWord64# :: Word64# -> Word64#
byteSwapWord64# = E.byteSwap64#

byteSwapInt16# :: Int16# -> Int16#
byteSwapInt16# w = E.word16ToInt16# (byteSwapWord16# (E.int16ToWord16# w))

byteSwapInt32# :: Int32# -> Int32#
byteSwapInt32# w = E.word32ToInt32# (byteSwapWord32# (E.int32ToWord32# w))

byteSwapInt64# :: Int64# -> Int64#
byteSwapInt64# w = E.word64ToInt64# (byteSwapWord64# (E.int64ToWord64# w))

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

writeWord8# :: Word8# -> Writer s
writeWord8# w = Writer \addr s ->
  case writeWord8OffAddr# addr 0# w s of
    s' -> (# s', plusAddr# addr 1# #)

writeWord16# :: Word16# -> Writer s
writeWord16# w = Writer \addr s ->
  case writeWord8OffAddrAsWord16# addr 0# w s of
    s' -> (# s', plusAddr# addr 2# #)

writeWord16BE# :: Word16# -> Writer s
writeWord16BE# w = case hostEndianness of
  BigEndian -> writeWord16# w
  LittleEndian -> writeWord16# (byteSwapWord16# w)

writeWord16LE# :: Word16# -> Writer s
writeWord16LE# w = case hostEndianness of
  LittleEndian -> writeWord16# w
  BigEndian -> writeWord16# (byteSwapWord16# w)

writeWord32# :: Word32# -> Writer s
writeWord32# w = Writer \addr s ->
  case writeWord8OffAddrAsWord32# addr 0# w s of
    s' -> (# s', plusAddr# addr 4# #)

writeWord32BE# :: Word32# -> Writer s
writeWord32BE# w = case hostEndianness of
  BigEndian -> writeWord32# w
  LittleEndian -> writeWord32# (byteSwapWord32# w)

writeWord32LE# :: Word32# -> Writer s
writeWord32LE# w = case hostEndianness of
  LittleEndian -> writeWord32# w
  BigEndian -> writeWord32# (byteSwapWord32# w)

writeWord64# :: Word64# -> Writer s
writeWord64# w = Writer \addr s ->
  case writeWord8OffAddrAsWord64# addr 0# w s of
    s' -> (# s', plusAddr# addr 8# #)

writeWord64BE# :: Word64# -> Writer s
writeWord64BE# w = case hostEndianness of
  BigEndian -> writeWord64# w
  LittleEndian -> writeWord64# (byteSwapWord64# w)

writeWord64LE# :: Word64# -> Writer s
writeWord64LE# w = case hostEndianness of
  LittleEndian -> writeWord64# w
  BigEndian -> writeWord64# (byteSwapWord64# w)

writeInt8# :: Int8# -> Writer s
writeInt8# w = Writer \addr s ->
  case writeInt8OffAddr# addr 0# w s of
    s' -> (# s', plusAddr# addr 1# #)

writeInt16# :: Int16# -> Writer s
writeInt16# w = Writer \addr s ->
  case writeWord8OffAddrAsInt16# addr 0# w s of
    s' -> (# s', plusAddr# addr 2# #)

writeInt16BE# :: Int16# -> Writer s
writeInt16BE# w = case hostEndianness of
  BigEndian -> writeInt16# w
  LittleEndian -> writeInt16# (byteSwapInt16# w)

writeInt16LE# :: Int16# -> Writer s
writeInt16LE# w = case hostEndianness of
  LittleEndian -> writeInt16# w
  BigEndian -> writeInt16# (byteSwapInt16# w)

writeInt32# :: Int32# -> Writer s
writeInt32# w = Writer \addr s ->
  case writeWord8OffAddrAsInt32# addr 0# w s of
    s' -> (# s', plusAddr# addr 4# #)

writeInt32BE# :: Int32# -> Writer s
writeInt32BE# w = case hostEndianness of
  BigEndian -> writeInt32# w
  LittleEndian -> writeInt32# (byteSwapInt32# w)

writeInt32LE# :: Int32# -> Writer s
writeInt32LE# w = case hostEndianness of
  LittleEndian -> writeInt32# w
  BigEndian -> writeInt32# (byteSwapInt32# w)

writeInt64# :: Int64# -> Writer s
writeInt64# w = Writer \addr s ->
  case writeWord8OffAddrAsInt64# addr 0# w s of
    s' -> (# s', plusAddr# addr 8# #)

writeInt64BE# :: Int64# -> Writer s
writeInt64BE# w = case hostEndianness of
  BigEndian -> writeInt64# w
  LittleEndian -> writeInt64# (byteSwapInt64# w)

writeInt64LE# :: Int64# -> Writer s
writeInt64LE# w = case hostEndianness of
  LittleEndian -> writeInt64# w
  BigEndian -> writeInt64# (byteSwapInt64# w)
