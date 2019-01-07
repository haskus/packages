{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Operations on bits
module Haskus.Format.Binary.Bits
   ( Bits
   , FiniteBits (..)
   , IndexableBits (..)
   , ShiftableBits (..)
   , SignedShiftableBits (..)
   , RotatableBits (..)
   , Bitwise (..)
   -- * Bit reversal
   , ReversableBits (..)
   , reverseBitsGeneric
   , reverseLeastBits
   -- * Mask
   , makeMask
   , maskLeastBits
   -- * String conversion
   , bitsToString
   , bitsToStringN
   , bitsFromString
   -- * Shift
   , getBitRange
   -- * Various
   , bitOffset
   , byteOffset
   )
where

import Haskus.Utils.List (foldl')
import Haskus.Utils.Types
import Haskus.Format.Binary.Bits.Finite
import Haskus.Format.Binary.Bits.Index
import Haskus.Format.Binary.Bits.Reverse
import Haskus.Format.Binary.Bits.Rotate
import Haskus.Format.Binary.Bits.Shift
import Haskus.Format.Binary.Bits.Bitwise
import Haskus.Format.Binary.Bits.Order
import Haskus.Format.Binary.Word

type Bits a =
   ( Eq a
   , FiniteBits a
   , IndexableBits a
   , ShiftableBits a
   , Bitwise a
   , RotatableBits a
   , KnownNat (BitSize a)
   )

-- | makeMask 3 = 00000111
makeMask :: forall a.
   ( ShiftableBits a
   , FiniteBits a
   , KnownNat (BitSize a)
   , Bitwise a
   ) => Word -> a
makeMask n = complement zeroBits `shiftR` off
   where
      off = natValue' @(BitSize a) - n

{-# SPECIALIZE makeMask :: Word -> Int #-}
{-# SPECIALIZE makeMask :: Word -> Int8 #-}
{-# SPECIALIZE makeMask :: Word -> Int16 #-}
{-# SPECIALIZE makeMask :: Word -> Int32 #-}
{-# SPECIALIZE makeMask :: Word -> Int64 #-}
{-# SPECIALIZE makeMask :: Word -> Word #-}
{-# SPECIALIZE makeMask :: Word -> Word8 #-}
{-# SPECIALIZE makeMask :: Word -> Word16 #-}
{-# SPECIALIZE makeMask :: Word -> Word32 #-}
{-# SPECIALIZE makeMask :: Word -> Word64 #-}

-- | Keep only the n least-significant bits of the given value
maskLeastBits :: forall a.
   ( ShiftableBits a
   , FiniteBits a
   , Bitwise a
   , KnownNat (BitSize a)
   ) => Word -> a -> a
{-# INLINE maskLeastBits #-}
maskLeastBits n v = v .&. makeMask n

-- | Compute bit offset (equivalent to x `mod` 8 but faster)
bitOffset :: Word -> Word
{-# INLINE bitOffset #-}
bitOffset n = makeMask 3 .&. n

-- | Compute byte offset (equivalent to x `div` 8 but faster)
byteOffset :: Word -> Word
{-# INLINE byteOffset #-}
byteOffset n = n `uncheckedShiftR` 3

-- | Reverse the @n@ least important bits of the given value. The higher bits
-- are set to 0.
reverseLeastBits ::
   ( ShiftableBits a
   , FiniteBits a
   , ReversableBits a
   , KnownNat (BitSize a)
   ) => Word -> a -> a
reverseLeastBits n value = reverseBits value `uncheckedShiftR` ((bitSize value) - n)

-- | Convert bits into a string composed of '0' and '1' chars
bitsToString :: forall a.
   ( FiniteBits a
   , IndexableBits a
   , KnownNat (BitSize a)
   ) => a -> String
bitsToString = bitsToStringN (natValue @(BitSize a))

-- | Convert a specified amount of bits into a string composed of '0' and '1' chars
bitsToStringN :: forall a.
   ( IndexableBits a
   ) => Word -> a -> String
bitsToStringN n x = fmap b [n-1, n-2 .. 0]
   where
      b v = if testBit x v then '1' else '0'

-- | Convert a string of '0' and '1' chars into a word
bitsFromString :: Bits a => String -> a
bitsFromString xs = foldl' b zeroBits (reverse xs `zip` [0..])
   where
      b x ('0',i) = clearBit x i
      b x ('1',i) = setBit x i
      b _ (c,_)   = error $ "Invalid character in the string: " ++ [c]


-- | `getBitRange bo offset n c` takes n bits at offset in c and put them in the
-- least-significant bits of the result
getBitRange :: forall b.
   ( ShiftableBits b
   , ReversableBits b
   , FiniteBits b
   , KnownNat (BitSize b)
   , Bitwise b
   ) => BitOrder -> Word -> Word -> b -> b
{-# INLINE getBitRange #-}
getBitRange bo o n c = case bo of
      BB -> maskLeastBits n $ c             `uncheckedShiftR` d
      BL -> maskLeastBits n $ reverseBits c `uncheckedShiftR` o
      LB -> maskLeastBits n $ reverseBits c `uncheckedShiftR` d
      LL -> maskLeastBits n $ c             `uncheckedShiftR` o
   where 
      d  = bitSize c - n - o

