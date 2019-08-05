{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Binary serialization of Haskell values
module Haskus.Binary.Serialize
   ( Serializable (..)
   , Size (..)
   )
where

import Haskus.Binary.Serialize.Put
import Haskus.Binary.Serialize.Size
import Haskus.Binary.Serialize.Get
import Haskus.Memory.Buffer
import Haskus.Number.Word
import Haskus.Number.Int
import Haskus.Binary.Endianness
import Haskus.Utils.Types

-- | Size in bytes
data Size
   = Exactly Nat   -- ^ Exactly the given size
   | DynamicStored -- ^ Dynamically known size (the size is stored with the object)
   | DynamicGiven  -- ^ Dynamically known size (the size isn't stored with the object and must be given)

-- | Binary serializable data
class Serializable a where

   -- | Size of the data in bytes
   type SizeOf a :: Size

   -- | Sensible to endianness
   type Endian a :: Bool

   -- | Dynamic size of the data in bytes
   --
   -- The default implementation execute the put method with a PutMonad that
   -- only stores the size in bytes. Overload this function if possible!
   sizeOf :: a -> Word
   sizeOf a = runGetSize (put LittleEndian a)

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
   type SizeOf BufferI = 'DynamicGiven
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
