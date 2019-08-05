{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Bit rotations
module Haskus.Binary.Bits.Rotate
   ( RotatableBits (..)
   )
where

import Haskus.Binary.Bits.Finite
import Haskus.Binary.Bits.Shift
import Haskus.Binary.Bits.Bitwise
import Haskus.Number.Word
import Haskus.Number.Int
import Haskus.Utils.Types

-- | Types whose bits can be rotated
class RotatableBits a where

   -- | Rotate left if positive, right if negative
   rotate :: a -> Int -> a
   default rotate ::
      ( FiniteBits a
      , KnownNat (BitSize a)
      ) => a -> Int -> a
   rotate a i
      | i' > 0     = rotateL a (fromIntegral i')
      | i' < 0     = rotateR a (fromIntegral (negate i'))
      | otherwise = a
      where
         i' = i `mod` bitSize a

   -- | Checked left bit rotation
   rotateL :: a -> Word -> a
   default rotateL ::
      ( FiniteBits a
      , KnownNat (BitSize a)
      ) => a -> Word -> a
   rotateL a n = uncheckedRotateL a (n `mod` bitSize a)

   -- | Checked right bit rotation
   rotateR :: a -> Word -> a
   default rotateR ::
      ( FiniteBits a
      , KnownNat (BitSize a)
      ) => a -> Word -> a
   rotateR a n = uncheckedRotateR a (n `mod` bitSize a)

   -- | Unchecked rotate left if positive, right if negative
   uncheckedRotate :: a -> Int -> a
   uncheckedRotate a i
      | i > 0     = uncheckedRotateL a (fromIntegral i)
      | i < 0     = uncheckedRotateR a (fromIntegral (negate i))
      | otherwise = a

   -- | Unchecked left bit rotation
   uncheckedRotateL :: a -> Word -> a
   default uncheckedRotateL ::
      ( ShiftableBits a
      , FiniteBits a
      , KnownNat (BitSize a)
      , Bitwise a
      ) => a -> Word -> a
   uncheckedRotateL a i = (a `uncheckedShiftL` i) .|. (a `uncheckedShiftR` (n-i))
      where n = bitSize a
      

   -- | Unchecked right bit rotation
   uncheckedRotateR :: a -> Word -> a
   default uncheckedRotateR ::
      ( ShiftableBits a
      , FiniteBits a
      , KnownNat (BitSize a)
      , Bitwise a
      ) => a -> Word -> a
   uncheckedRotateR a i = (a `uncheckedShiftL` (n-i)) .|. (a `uncheckedShiftR` i)
      where n = bitSize a


instance RotatableBits Word
instance RotatableBits Word8
instance RotatableBits Word16
instance RotatableBits Word32
instance RotatableBits Word64

instance RotatableBits Int
instance RotatableBits Int8
instance RotatableBits Int16
instance RotatableBits Int32
instance RotatableBits Int64
