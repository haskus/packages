module Haskus.Memory.Writer
  ( Writer (..)
  , WriterIO
  , SizedWriter (..)
  , runWriter#
  , runWriterIO
  -- * writers
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
  -- * Sized writers
  , swriteWord8#
  , swriteWord16#
  , swriteWord16BE#
  , swriteWord16LE#
  , swriteWord32#
  , swriteWord32BE#
  , swriteWord32LE#
  , swriteWord64#
  , swriteWord64BE#
  , swriteWord64LE#
  , swriteInt8#
  , swriteInt16#
  , swriteInt16BE#
  , swriteInt16LE#
  , swriteInt32#
  , swriteInt32BE#
  , swriteInt32LE#
  , swriteInt64#
  , swriteInt64BE#
  , swriteInt64LE#
  )
where

import GHC.Exts
import GHC.IO
import Haskus.Binary.Endianness (hostEndianness, Endianness(..))
import Haskus.Binary.ByteSwap

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
runWriterIO (Ptr addr) w = IO \s -> case runWriter# w addr s of
  (# s', addr' #) -> (# s', Ptr addr' #)


-- | A writer that carries the size of the written value in bytes.
data SizedWriter s = SizedWriter
  { sizedWriterSize :: !Word#
  , sizedWriter     :: !(Writer s)
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


----------------------------
-- Writers
----------------------------

writeWord8# :: Word8# -> Writer s
writeWord8# w = Writer \addr s ->
  case writeWord8OffAddr# addr 0# w s of
    s' -> (# s', plusAddr# addr 1# #)

writeWord16# :: Word16# -> Writer s
writeWord16# w = Writer \addr s ->
  case writeWord16OffAddr# addr 0# w s of
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
  case writeWord32OffAddr# addr 0# w s of
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
  case writeWord64OffAddr# addr 0# w s of
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
  case writeInt16OffAddr# addr 0# w s of
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
  case writeInt32OffAddr# addr 0# w s of
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
  case writeInt64OffAddr# addr 0# w s of
    s' -> (# s', plusAddr# addr 8# #)

writeInt64BE# :: Int64# -> Writer s
writeInt64BE# w = case hostEndianness of
  BigEndian -> writeInt64# w
  LittleEndian -> writeInt64# (byteSwapInt64# w)

writeInt64LE# :: Int64# -> Writer s
writeInt64LE# w = case hostEndianness of
  LittleEndian -> writeInt64# w
  BigEndian -> writeInt64# (byteSwapInt64# w)

----------------------------
-- Sized Writers
----------------------------

swriteWord8# :: Word8# -> SizedWriter s
swriteWord8# w = SizedWriter 1## (writeWord8# w)

swriteWord16# :: Word16# -> SizedWriter s
swriteWord16# w = SizedWriter 2## (writeWord16# w)

swriteWord16BE# :: Word16# -> SizedWriter s
swriteWord16BE# w = SizedWriter 2## (writeWord16BE# w)

swriteWord16LE# :: Word16# -> SizedWriter s
swriteWord16LE# w = SizedWriter 2## (writeWord16LE# w)

swriteWord32# :: Word32# -> SizedWriter s
swriteWord32# w = SizedWriter 4## (writeWord32# w)

swriteWord32BE# :: Word32# -> SizedWriter s
swriteWord32BE# w = SizedWriter 4## (writeWord32BE# w)

swriteWord32LE# :: Word32# -> SizedWriter s
swriteWord32LE# w = SizedWriter 4## (writeWord32LE# w)

swriteWord64# :: Word64# -> SizedWriter s
swriteWord64# w = SizedWriter 8## (writeWord64# w)

swriteWord64BE# :: Word64# -> SizedWriter s
swriteWord64BE# w = SizedWriter 8## (writeWord64BE# w)

swriteWord64LE# :: Word64# -> SizedWriter s
swriteWord64LE# w = SizedWriter 8## (writeWord64LE# w)

swriteInt8# :: Int8# -> SizedWriter s
swriteInt8# w = SizedWriter 1## (writeInt8# w)

swriteInt16# :: Int16# -> SizedWriter s
swriteInt16# w = SizedWriter 2## (writeInt16# w)

swriteInt16BE# :: Int16# -> SizedWriter s
swriteInt16BE# w = SizedWriter 2## (writeInt16BE# w)

swriteInt16LE# :: Int16# -> SizedWriter s
swriteInt16LE# w = SizedWriter 2## (writeInt16LE# w)

swriteInt32# :: Int32# -> SizedWriter s
swriteInt32# w = SizedWriter 4## (writeInt32# w)

swriteInt32BE# :: Int32# -> SizedWriter s
swriteInt32BE# w = SizedWriter 4## (writeInt32BE# w)

swriteInt32LE# :: Int32# -> SizedWriter s
swriteInt32LE# w = SizedWriter 4## (writeInt32LE# w)

swriteInt64# :: Int64# -> SizedWriter s
swriteInt64# w = SizedWriter 8## (writeInt64# w)

swriteInt64BE# :: Int64# -> SizedWriter s
swriteInt64BE# w = SizedWriter 8## (writeInt64BE# w)

swriteInt64LE# :: Int64# -> SizedWriter s
swriteInt64LE# w = SizedWriter 8## (writeInt64LE# w)
