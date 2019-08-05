{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Binary serialization of Haskell values
module Haskus.Binary.Serialize.Put
   ( PutMonad (..)
   , putFloat32
   , putFloat32LE
   , putFloat32BE
   , putFloat64
   , putFloat64LE
   , putFloat64BE
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
   )
where

import Haskus.Memory.Buffer
import Haskus.Number.Word
import Haskus.Binary.Endianness
import Haskus.Number.Float
import Haskus.Utils.Flow

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

