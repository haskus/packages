{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Binary serialization of Haskell values
module Haskus.Format.Binary.Serialize
   ( PutMonad (..)
   , GetMonad (..)
   , Serializable (..)
   , Size (..)
   )
where

import Haskus.Data.Buffer
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Endianness
import Haskus.Utils.Types

-- | Monad which can build a sequence of bytes
class Monad m => PutMonad m where
   -- | Write some Word8
   putWord8s   :: [Word8]  -> m ()
   -- | Write some Word16
   putWord16s  :: [Word16] -> m ()
   -- | Write some Word32
   putWord32s  :: [Word32] -> m ()
   -- | Write some Word64
   putWord64s  :: [Word64] -> m ()
   -- | Write some Word16 with little-endian order
   putWord16LEs  :: [Word16] -> m ()
   -- | Write some Word32 with little-endian order
   putWord32LEs  :: [Word32] -> m ()
   -- | Write some Word64 with little-endian order
   putWord64LEs :: [Word64] -> m ()
   -- | Write some Word16 with big-endian order
   putWord16BEs  :: [Word16] -> m ()
   -- | Write some Word32 with big-endian order
   putWord32BEs  :: [Word32] -> m ()
   -- | Write some Word64 with big-endian order
   putWord64BEs :: [Word64] -> m ()
   -- | Write the contents of a buffer
   putBuffer   :: Buffer mut pin gc heap -> m ()

   -- | Pre-allocate at least the given amount of bytes
   --
   -- This is a hint for the putter to speed up the allocation of memory
   preAllocateAtLeast :: Word -> m ()

-- | Monad which can read a sequence of bytes
class Monad m => GetMonad m where
   -- | Read some Word8
   getWord8s     :: Word -> m [Word8]
   -- | Read some Word16 with host endianness
   getWord16s    :: Word -> m [Word16]
   -- | Read some Word32 with host endianness
   getWord32s    :: Word -> m [Word32]
   -- | Read some Word64 with host endianness
   getWord64s    :: Word -> m [Word64]
   -- | Read some Word16 with little-endian order
   getWord16LEs    :: Word -> m [Word16]
   -- | Read some Word32 with little-endian order
   getWord32LEs    :: Word -> m [Word32]
   -- | Read some Word64 with little-endian order
   getWord64LEs    :: Word -> m [Word64]
   -- | Read some Word16 with big-endian order
   getWord16BEs    :: Word -> m [Word16]
   -- | Read some Word32 with big-endian order
   getWord32BEs    :: Word -> m [Word32]
   -- | Read some Word64 with big-endian order
   getWord64BEs    :: Word -> m [Word64]
   -- | Read the given amount of bytes into a new buffer
   getBuffer     :: Word -> m BufferI
   -- | Read the given amount of bytes into the specified buffer
   getBufferInto :: Word -> Buffer 'Mutable pin gc heap -> m ()

-- | Size in bytes
data Size
   = Exactly Nat  -- ^ Exactly the given size
   | AtLeast Nat  -- ^ At least the given size
   | Dynamic      -- ^ Dynamically known size

-- | Binary serializable data
class Serializable a where

   -- | Size of the data in bytes (Dynamic or Static Nat)
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
-- Instances
--------------------------------------------

instance Serializable Word8 where
   type SizeOf Word8  = 'Exactly 1
   type Endian Word8  = 'False
   sizeOf _           = 1
   put _ x            = putWord8s [x]
   get _ _            = head <$> getWord8s 1

instance Serializable Word16 where
   type SizeOf Word16 = 'Exactly 2
   type Endian Word16 = 'True
   sizeOf _           = 2
   put LittleEndian x = putWord16LEs [x]
   put BigEndian    x = putWord16BEs [x]
   get LittleEndian _ = head <$> getWord16LEs 1
   get BigEndian    _ = head <$> getWord16BEs 1

instance Serializable Word32 where
   type SizeOf Word32 = 'Exactly 4
   type Endian Word32 = 'True
   sizeOf _           = 4
   put LittleEndian x = putWord32LEs [x]
   put BigEndian    x = putWord32BEs [x]
   get LittleEndian _ = head <$> getWord32LEs 1
   get BigEndian    _ = head <$> getWord32BEs 1

instance Serializable Word64 where
   type SizeOf Word64 = 'Exactly 8
   type Endian Word64 = 'True
   sizeOf _           = 8
   put LittleEndian x = putWord64LEs [x]
   put BigEndian    x = putWord64BEs [x]
   get LittleEndian _ = head <$> getWord64LEs 1
   get BigEndian    _ = head <$> getWord64BEs 1

instance Serializable Int8 where
   type SizeOf Int8   = 'Exactly 1
   type Endian Int8   = 'False
   sizeOf _           = 1
   put _ x            = putWord8s [fromIntegral x]
   get _ _            = fromIntegral . head <$> getWord8s 1

instance Serializable Int16 where
   type SizeOf Int16  = 'Exactly 2
   type Endian Int16  = 'True
   sizeOf _           = 2
   put LittleEndian x = putWord16LEs [fromIntegral x]
   put BigEndian    x = putWord16BEs [fromIntegral x]
   get LittleEndian _ = fromIntegral . head <$> getWord16LEs 1
   get BigEndian    _ = fromIntegral . head <$> getWord16BEs 1

instance Serializable Int32 where
   type SizeOf Int32  = 'Exactly 4
   type Endian Int32  = 'True
   sizeOf _           = 4
   put LittleEndian x = putWord32LEs [fromIntegral x]
   put BigEndian    x = putWord32BEs [fromIntegral x]
   get LittleEndian _ = fromIntegral . head <$> getWord32LEs 1
   get BigEndian    _ = fromIntegral . head <$> getWord32BEs 1

instance Serializable Int64 where
   type SizeOf Int64  = 'Exactly 8
   type Endian Int64  = 'True
   sizeOf _           = 8
   put LittleEndian x = putWord64LEs [fromIntegral x]
   put BigEndian    x = putWord64BEs [fromIntegral x]
   get LittleEndian _ = fromIntegral . head <$> getWord64LEs 1
   get BigEndian    _ = fromIntegral . head <$> getWord64BEs 1

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
