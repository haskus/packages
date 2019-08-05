{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Binary serialization of Haskell values
module Haskus.Binary.Serialize
   ( PutMonad (..)
   , GetMonad (..)
   , Serializable (..)
   , Size (..)
   -- * Floating point
   , putFloat32
   , putFloat32LE
   , putFloat32BE
   , putFloat64
   , putFloat64LE
   , putFloat64BE
   , getFloat32
   , getFloat32LE
   , getFloat32BE
   , getFloat64
   , getFloat64LE
   , getFloat64BE
   -- * Endianness helpers
   , putWord16BE
   , putWord32BE
   , putWord64BE
   , putWord16LE
   , putWord32LE
   , putWord64LE
   , putWord16BEs
   , putWord32BEs
   , putWord64BEs
   , putWord16LEs
   , putWord32LEs
   , putWord64LEs
   , getWord16BE
   , getWord32BE
   , getWord64BE
   , getWord16LE
   , getWord32LE
   , getWord64LE
   , getWord16BEs
   , getWord32BEs
   , getWord64BEs
   , getWord16LEs
   , getWord32LEs
   , getWord64LEs
   )
where

import Haskus.Memory.Buffer
import Haskus.Number.Word
import Haskus.Number.Int
import Haskus.Binary.Endianness
import Haskus.Number.Float
import Haskus.Utils.Types
import Haskus.Utils.Flow

import GHC.Exts (IsList(..))

-- | Monad which can build a sequence of bytes
class Monad m => PutMonad m where
   -- | Write a Word8
   putWord8 :: Word8 -> m ()
   -- | Write a Word16
   putWord16 :: Word16 -> m ()
   -- | Write a Word32
   putWord32 :: Word32 -> m ()
   -- | Write a Word64
   putWord64 :: Word64 -> m ()

   -- | Write some Word8
   putWord8s   :: [Word8]  -> m ()
   putWord8s xs = forM_ xs putWord8

   -- | Write some Word16
   putWord16s  :: [Word16] -> m ()
   putWord16s xs = forM_ xs putWord16

   -- | Write some Word32
   putWord32s  :: [Word32] -> m ()
   putWord32s xs = forM_ xs putWord32

   -- | Write some Word64
   putWord64s  :: [Word64] -> m ()
   putWord64s xs = forM_ xs putWord64

   -- | Write the contents of a buffer
   putBuffer   :: BufferSize (Buffer Immutable pin gc heap) => Buffer Immutable pin gc heap -> m ()

   -- | Pre-allocate at least the given amount of bytes
   --
   -- This is a hint for the putter to speed up the allocation of memory
   preAllocateAtLeast :: Word -> m ()
   preAllocateAtLeast _ = return ()

-- | Monad which can read a sequence of bytes
class Monad m => GetMonad m where
   -- | Read a Word8
   getWord8    :: m Word8
   -- | Read a Word16 with host endianness
   getWord16   :: m Word16
   -- | Read a Word32 with host endianness
   getWord32   :: m Word32
   -- | Read a Word64 with host endianness
   getWord64   :: m Word64

   -- | Read some Word8
   getWord8s     :: Word -> m [Word8]
   getWord8s n = replicateM (fromIntegral n) getWord8
   -- | Read some Word16 with host endianness
   getWord16s    :: Word -> m [Word16]
   getWord16s n = replicateM (fromIntegral n) getWord16
   -- | Read some Word32 with host endianness
   getWord32s    :: Word -> m [Word32]
   getWord32s n = replicateM (fromIntegral n) getWord32
   -- | Read some Word64 with host endianness
   getWord64s    :: Word -> m [Word64]
   getWord64s n = replicateM (fromIntegral n) getWord64

   -- | Read the given amount of bytes into a new buffer
   getBuffer     :: Word -> m BufferI
   getBuffer n = do
      xs <- replicateM (fromIntegral n) getWord8
      return (fromListN (fromIntegral n) xs)

   -- | Read the given amount of bytes into the specified buffer
   getBufferInto :: Word -> Buffer 'Mutable pin gc heap -> m ()

-- | Size in bytes
data Size
   = Exactly Nat  -- ^ Exactly the given size
   | AtLeast Nat  -- ^ At least the given size
   | Dynamic      -- ^ Dynamically known size

-- | Binary serializable data
class Serializable a where

   -- | Size of the data in bytes
   type SizeOf a :: Size

   -- | Sensible to endianness
   type Endian a :: Bool

   -- | Dynamic size of the data in bytes
   sizeOf :: a -> Word

   -- | Serialize a value
   put :: PutMonad m => Endianness -> a -> m ()

   -- | Deserialize a value
   get :: GetMonad m => Endianness -> Word -> m a

--------------------------------------------
-- Floating point
--------------------------------------------

-- | Write a Float64 with host order
putFloat64 :: PutMonad m => Float64 -> m ()
putFloat64 d = putWord64 (float64ToWord64 d)

-- | Write a Float64 with little-endian order
putFloat64LE :: PutMonad m => Float64 -> m ()
putFloat64LE d = putWord64LE (float64ToWord64 d)

-- | Write a Float64 with big-endian order
putFloat64BE :: PutMonad m => Float64 -> m ()
putFloat64BE d = putWord64BE (float64ToWord64 d)

-- | Write a Float32 with host order
putFloat32 :: PutMonad m => Float32 -> m ()
putFloat32 d = putWord32 (float32ToWord32 d)

-- | Write a Float32 with little-endian order
putFloat32LE :: PutMonad m => Float32 -> m ()
putFloat32LE d = putWord32LE (float32ToWord32 d)

-- | Write a Float32 with big-endian order
putFloat32BE :: PutMonad m => Float32 -> m ()
putFloat32BE d = putWord32BE (float32ToWord32 d)


-- | Get a Float64 with host order
getFloat64 :: GetMonad m => m Float64
getFloat64 = getWord64 ||> word64ToFloat64

-- | Get a Float64 with little-endian order
getFloat64LE :: GetMonad m => m Float64
getFloat64LE = getWord64LE ||> word64ToFloat64

-- | Get a Float64 with big-endian order
getFloat64BE :: GetMonad m => m Float64
getFloat64BE = getWord64BE ||> word64ToFloat64

-- | Get a Float32 with host order
getFloat32 :: GetMonad m => m Float32
getFloat32 = getWord32 ||> word32ToFloat32

-- | Get a Float32 with little-endian order
getFloat32LE :: GetMonad m => m Float32
getFloat32LE = getWord32LE ||> word32ToFloat32

-- | Get a Float32 with big-endian order
getFloat32BE :: GetMonad m => m Float32
getFloat32BE = getWord32BE ||> word32ToFloat32

--------------------------------------------
-- Helpers for endianness
--------------------------------------------

-- | Write a Word16 with little-endian order
putWord16LE :: PutMonad m => Word16 -> m ()
putWord16LE x = putWord16 (hostToLittleEndian x)
-- | Write a Word32 with little-endian order
putWord32LE :: PutMonad m => Word32 -> m ()
putWord32LE x = putWord32 (hostToLittleEndian x)
-- | Write a Word64 with little-endian order
putWord64LE :: PutMonad m => Word64 -> m ()
putWord64LE x = putWord64 (hostToLittleEndian x)

-- | Write a Word16 with big-endian order
putWord16BE :: PutMonad m => Word16 -> m ()
putWord16BE x = putWord16 (hostToBigEndian x)
-- | Write a Word32 with big-endian order
putWord32BE :: PutMonad m => Word32 -> m ()
putWord32BE x = putWord32 (hostToBigEndian x)
-- | Write a Word64 with big-endian order
putWord64BE :: PutMonad m => Word64 -> m ()
putWord64BE x = putWord64 (hostToBigEndian x)

-- | Write some Word16 with little-endian order
putWord16LEs  :: PutMonad m => [Word16] -> m ()
putWord16LEs xs = putWord16s (fmap hostToLittleEndian xs)
-- | Write some Word32 with little-endian order
putWord32LEs  :: PutMonad m => [Word32] -> m ()
putWord32LEs xs = putWord32s (fmap hostToLittleEndian xs)
-- | Write some Word64 with little-endian order
putWord64LEs :: PutMonad m => [Word64] -> m ()
putWord64LEs xs = putWord64s (fmap hostToLittleEndian xs)
-- | Write some Word16 with big-endian order
putWord16BEs  :: PutMonad m => [Word16] -> m ()
putWord16BEs xs = putWord16s (fmap hostToBigEndian xs)
-- | Write some Word32 with big-endian order
putWord32BEs  :: PutMonad m => [Word32] -> m ()
putWord32BEs xs = putWord32s (fmap hostToBigEndian xs)
-- | Write some Word64 with big-endian order
putWord64BEs :: PutMonad m => [Word64] -> m ()
putWord64BEs xs = putWord64s (fmap hostToBigEndian xs)

-- | Read a Word16 with little-endian order
getWord16LE   :: GetMonad m => m Word16
getWord16LE = littleEndianToHost <$> getWord16
-- | Read a Word32 with little-endian order
getWord32LE   :: GetMonad m => m Word32
getWord32LE = littleEndianToHost <$> getWord32
-- | Read a Word64 with little-endian order
getWord64LE   :: GetMonad m => m Word64
getWord64LE = littleEndianToHost <$> getWord64
-- | Read a Word16 with big-endian order
getWord16BE   :: GetMonad m => m Word16
getWord16BE = bigEndianToHost <$> getWord16
-- | Read a Word32 with big-endian order
getWord32BE   :: GetMonad m => m Word32
getWord32BE = bigEndianToHost <$> getWord32
-- | Read a Word64 with big-endian order
getWord64BE   :: GetMonad m => m Word64
getWord64BE = bigEndianToHost <$> getWord64


-- | Read some Word16 with little-endian order
getWord16LEs    :: GetMonad m => Word -> m [Word16]
getWord16LEs n = fmap littleEndianToHost <$> getWord16s n
-- | Read some Word32 with little-endian order
getWord32LEs    :: GetMonad m => Word -> m [Word32]
getWord32LEs n =  fmap littleEndianToHost <$> getWord32s n
-- | Read some Word64 with little-endian order
getWord64LEs    :: GetMonad m => Word -> m [Word64]
getWord64LEs n =  fmap littleEndianToHost <$> getWord64s n

-- | Read some Word16 with big-endian order
getWord16BEs    :: GetMonad m => Word -> m [Word16]
getWord16BEs n = fmap bigEndianToHost <$> getWord16s n
-- | Read some Word32 with big-endian order
getWord32BEs    :: GetMonad m => Word -> m [Word32]
getWord32BEs n = fmap bigEndianToHost <$> getWord32s n
-- | Read some Word64 with big-endian order
getWord64BEs    :: GetMonad m => Word -> m [Word64]
getWord64BEs n = fmap bigEndianToHost <$> getWord64s n


--------------------------------------------
-- Instances
--------------------------------------------

instance Serializable Word8 where
   type SizeOf Word8  = 'Exactly 1
   type Endian Word8  = 'False
   sizeOf _           = 1
   put _ x            = putWord8 x
   get _ _            = getWord8

instance Serializable Word16 where
   type SizeOf Word16 = 'Exactly 2
   type Endian Word16 = 'True
   sizeOf _           = 2
   put LittleEndian x = putWord16LE x
   put BigEndian    x = putWord16BE x
   get LittleEndian _ = getWord16LE
   get BigEndian    _ = getWord16BE

instance Serializable Word32 where
   type SizeOf Word32 = 'Exactly 4
   type Endian Word32 = 'True
   sizeOf _           = 4
   put LittleEndian x = putWord32LE x
   put BigEndian    x = putWord32BE x
   get LittleEndian _ = getWord32LE
   get BigEndian    _ = getWord32BE

instance Serializable Word64 where
   type SizeOf Word64 = 'Exactly 8
   type Endian Word64 = 'True
   sizeOf _           = 8
   put LittleEndian x = putWord64LE x
   put BigEndian    x = putWord64BE x
   get LittleEndian _ = getWord64LE
   get BigEndian    _ = getWord64BE

instance Serializable Int8 where
   type SizeOf Int8   = 'Exactly 1
   type Endian Int8   = 'False
   sizeOf _           = 1
   put _ x            = putWord8 (fromIntegral x)
   get _ _            = fromIntegral <$> getWord8

instance Serializable Int16 where
   type SizeOf Int16  = 'Exactly 2
   type Endian Int16  = 'True
   sizeOf _           = 2
   put LittleEndian x = putWord16LE (fromIntegral x)
   put BigEndian    x = putWord16BE (fromIntegral x)
   get LittleEndian _ = fromIntegral <$> getWord16LE
   get BigEndian    _ = fromIntegral <$> getWord16BE

instance Serializable Int32 where
   type SizeOf Int32  = 'Exactly 4
   type Endian Int32  = 'True
   sizeOf _           = 4
   put LittleEndian x = putWord32LE (fromIntegral x)
   put BigEndian    x = putWord32BE (fromIntegral x)
   get LittleEndian _ = fromIntegral <$> getWord32LE
   get BigEndian    _ = fromIntegral <$> getWord32BE

instance Serializable Int64 where
   type SizeOf Int64  = 'Exactly 8
   type Endian Int64  = 'True
   sizeOf _           = 8
   put LittleEndian x = putWord64LE (fromIntegral x)
   put BigEndian    x = putWord64BE (fromIntegral x)
   get LittleEndian _ = fromIntegral <$> getWord64LE
   get BigEndian    _ = fromIntegral <$> getWord64BE

instance Serializable BufferI where
   type SizeOf BufferI = 'Dynamic
   type Endian BufferI = 'False
   sizeOf b            = bufferSize b
   put _ x             = putBuffer x
   get _ sz            = getBuffer sz

instance Serializable a => Serializable (AsBigEndian a) where
   type SizeOf (AsBigEndian a) = SizeOf a
   type Endian (AsBigEndian a) = 'False
   sizeOf (AsBigEndian b)      = sizeOf b
   put _ (AsBigEndian x)       = put BigEndian x
   get _ sz                    = AsBigEndian <$> get BigEndian sz

instance Serializable a => Serializable (AsLittleEndian a) where
   type SizeOf (AsLittleEndian a) = SizeOf a
   type Endian (AsLittleEndian a) = 'False
   sizeOf (AsLittleEndian b)      = sizeOf b
   put _ (AsLittleEndian x)       = put LittleEndian x
   get _ sz                       = AsLittleEndian <$> get LittleEndian sz
