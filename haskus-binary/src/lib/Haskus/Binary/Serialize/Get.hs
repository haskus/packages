{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Binary deserialization of Haskell values
module Haskus.Binary.Serialize.Get
   ( GetMonad (..)
   , getFloat32
   , getFloat32LE
   , getFloat32BE
   , getFloat64
   , getFloat64LE
   , getFloat64BE
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
import Haskus.Binary.Endianness
import Haskus.Number.Float
import Haskus.Utils.Flow

import GHC.Exts (IsList(..))


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
