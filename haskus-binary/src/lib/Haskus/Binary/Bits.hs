{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

-- | Operations on bits
module Haskus.Binary.Bits
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
   , MaskBits (..)
   , Maskable
   , maskDyn
   , mask
   -- * String conversion
   , bitsToString
   , bitsToStringN
   , bitsFromString
   -- * Shift
   , getBitRange
   -- * Various
   , bitOffset
   , byteOffset
   , isPowerOfTwo
   , isPowerOfFour
   , getPowerOfTwo
   , getPowerOfFour
   )
where

import Haskus.Utils.List (foldl')
import Haskus.Utils.Types
import Haskus.Utils.Maybe
import Haskus.Binary.Bits.Finite
import Haskus.Binary.Bits.Index
import Haskus.Binary.Bits.Reverse
import Haskus.Binary.Bits.Rotate
import Haskus.Binary.Bits.Shift
import Haskus.Binary.Bits.Bitwise
import Haskus.Binary.Bits.Order
import Haskus.Binary.Bits.Mask
import Haskus.Binary.Bits.Helper

-- $setup
-- >>> import Haskus.Binary.Bits


type Bits a =
   ( Eq a
   , FiniteBits a
   , IndexableBits a
   , ShiftableBits a
   , Bitwise a
   , RotatableBits a
   , KnownNat (BitSize a)
   , MaskBits a
   )

-- | Check if a number is a power of two (2^n)
--
-- >>> isPowerOfTwo (10 :: Word)
-- False
-- >>> isPowerOfTwo (16 :: Word)
-- True
isPowerOfTwo :: IndexableBits a => a -> Bool
isPowerOfTwo x = popCount x == 1

-- | Check if a number is a power of two (2^n) and return `n`
--
-- >>> getPowerOfTwo (10 :: Word)
-- Nothing
-- >>> getPowerOfTwo (16 :: Word)
-- Just 4
getPowerOfTwo :: (IndexableBits a, FiniteBits a) => a -> Maybe Word
getPowerOfTwo x
   | isPowerOfTwo x = Just (countTrailingZeros x)
   | otherwise      = Nothing

-- | Check if a number is a power of four (4^n)
--
-- >>> isPowerOfFour (10 :: Word)
-- False
-- >>> isPowerOfFour (16 :: Word)
-- True
isPowerOfFour :: (IndexableBits a, FiniteBits a) => a -> Bool
isPowerOfFour x = isJust (getPowerOfFour x)

-- | Check if a number is a power of four (4^n) and return `n`
--
-- >>> getPowerOfFour (10 :: Word)
-- Nothing
-- >>> getPowerOfFour (16 :: Word)
-- Just 2
getPowerOfFour :: (IndexableBits a, FiniteBits a) => a -> Maybe Word
getPowerOfFour x
   | popCount x == 1                -- test that a single bit is set to 1
   , let c = countTrailingZeros x   -- and that it is followed by an even
   , testBit c 0 == False           -- number of zeros
   = Just (c `shiftR` 1)
   | otherwise       = Nothing

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
   , MaskBits b
   ) => BitOrder -> Word -> Word -> b -> b
{-# INLINABLE getBitRange #-}
getBitRange bo o n c = case bo of
      BB -> maskDyn n $ c             `uncheckedShiftR` d
      BL -> maskDyn n $ reverseBits c `uncheckedShiftR` o
      LB -> maskDyn n $ reverseBits c `uncheckedShiftR` d
      LL -> maskDyn n $ c             `uncheckedShiftR` o
   where 
      d  = bitSize c - n - o

