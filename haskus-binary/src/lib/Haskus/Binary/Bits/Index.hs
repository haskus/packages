{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

-- | Bit indexable types
module Haskus.Binary.Bits.Index
   ( IndexableBits (..)
   )
where

import Haskus.Binary.Bits.Shift
import Haskus.Binary.Bits.Bitwise
import Haskus.Binary.Bits.Finite
import Haskus.Number.Word
import Haskus.Number.Int

import GHC.Exts
import qualified Data.Bits as BaseBits
import Numeric.Natural

-- | Type whose bits are indexable
class IndexableBits a where
   -- | @bit /i/@ is a value with the @/i/@th bit set and all other bits clear.
   bit :: Word -> a
   default bit :: (Num a, ShiftableBits a) => Word -> a
   bit i = 1 `shiftL` i

   -- | @x \`setBit\` i@ is the same as @x .|. bit i@
   setBit :: a -> Word -> a
   default setBit :: (Bitwise a) => a -> Word -> a
   setBit a i = a .|. bit i

   -- | @x \`clearBit\` i@ is the same as @x .&. complement (bit i)@
   clearBit :: a -> Word -> a
   default clearBit :: (FiniteBits a,Bitwise a) => a -> Word -> a
   clearBit a i = a .&. complement (bit i)

   -- | @x \`complementBit\` i@ is the same as @x \`xor\` bit i@
   complementBit :: a -> Word -> a
   default complementBit :: (Bitwise a) => a -> Word -> a
   complementBit a i = a `xor` bit i

   -- | Return 'True' if the @n@th bit of the argument is 1
   testBit :: a -> Word -> Bool
   default testBit :: (Bitwise a, Num a, Eq a) => a -> Word -> Bool
   testBit a i = (a .&. bit i) /= 0

   -- | Return the number of set bits
   popCount :: a -> Word
   default popCount :: (Bitwise a, Num a, Eq a) => a -> Word
   popCount = go 0
      where
         go !c 0 = c
         go c  w = go (c+1) (w .&. (w-1))


instance IndexableBits Word where
   popCount (W# x#) = W# (popCnt# x#)

instance IndexableBits Word8 where
   popCount (W8# x#) = W# (popCnt8# x#)

instance IndexableBits Word16 where
   popCount (W16# x#) = W# (popCnt16# x#)

instance IndexableBits Word32 where
   popCount (W32# x#) = W# (popCnt32# x#)

instance IndexableBits Word64 where
   popCount (W64# x#) = W# (popCnt64# x#)

instance IndexableBits Int where
   popCount (I# x#) = W# (popCnt# (int2Word# x#))

instance IndexableBits Int8 where
   popCount (I8# x#) = W# (popCnt8# (int2Word# x#))

instance IndexableBits Int16 where
   popCount (I16# x#) = W# (popCnt16# (int2Word# x#))

instance IndexableBits Int32 where
   popCount (I32# x#) = W# (popCnt32# (int2Word# x#))

instance IndexableBits Int64 where
   popCount (I64# x#) = W# (popCnt64# (int2Word# x#))

instance IndexableBits Integer where
   -- we don't have access to Integer primitive (we would have to conditionally
   -- import integer-gmp or integer-simple like `base` does) so we use Data.Bits
   -- from `base` instead.
   testBit x i  = BaseBits.testBit x (fromIntegral i)
   bit i        = BaseBits.bit (fromIntegral i)
   popCount x   = fromIntegral (BaseBits.popCount x)
   clearBit x i = BaseBits.clearBit x (fromIntegral i)

instance IndexableBits Natural where
   testBit x i  = BaseBits.testBit x (fromIntegral i)
   bit i        = BaseBits.bit (fromIntegral i)
   popCount x   = fromIntegral (BaseBits.popCount x)
   clearBit x i = BaseBits.clearBit x (fromIntegral i)
