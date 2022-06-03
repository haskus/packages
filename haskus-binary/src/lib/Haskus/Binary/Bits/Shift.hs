{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}

-- | Bit shifts
module Haskus.Binary.Bits.Shift
   ( ShiftableBits (..)
   , SignedShiftableBits (..)
   )
where

import Haskus.Number.Word
import Haskus.Number.Int
import GHC.Exts
import GHC.Num

#include "MachDeps.h"

#if !MIN_VERSION_GLASGOW_HASKELL (9,0,0,0)
wordToInt# :: Word -> Int#
wordToInt# (W# w) = word2Int# w

integerShiftL :: Integer -> Word -> Integer
integerShiftL x w = shiftLInteger x (wordToInt# w)

integerShiftR :: Integer -> Word -> Integer
integerShiftR x w = shiftRInteger x (wordToInt# w)

naturalShiftL :: Natural -> Word -> Natural
naturalShiftL x w = shiftLNatural x (fromIntegral w)

naturalShiftR :: Natural -> Word -> Natural
naturalShiftR x w = shiftRNatural x (fromIntegral w)
#endif

-- | Bit shifts
--
-- "Checked" means that there is an additional test to ensure that the shift
-- offset is valid (less than the bit count). If you are sure that the offset is
-- valid, use the "unchecked" version which should be faster.
--
-- To shift signed numbers, see `SignedShiftableBits` class methods.
class ShiftableBits a where
   -- | Checked right shift
   shiftR :: a -> Word -> a

   -- | Checked left shift
   shiftL :: a -> Word -> a

   -- | Unchecked right shift
   uncheckedShiftR :: a -> Word -> a

   -- | Unchecked left shift
   uncheckedShiftL :: a -> Word -> a

   -- | Checked shift to the left if positive, to the right if negative
   shift :: a -> Int -> a
   shift a i
      | i > 0     = shiftL a (fromIntegral i)
      | i < 0     = shiftR a (fromIntegral (negate i))
      | otherwise = a

   -- | Unchecked shift to the left if positive, to the right if negative
   uncheckedShift :: a -> Int -> a
   uncheckedShift a i
      | i > 0     = uncheckedShiftL a (fromIntegral i)
      | i < 0     = uncheckedShiftR a (fromIntegral (negate i))
      | otherwise = a

-- | Signed bit shifts
--
-- "Signed" means that the sign bit (the higher order bit):
--    - propagates to the right during right shifts and 
--    - keeps its value during left shifts (except when all other bits are 0)
--
-- "Checked" means that there is an additional test to ensure that the shift
-- offset is valid (less than the bit count). If you are sure that the offset is
-- valid, use the "unchecked" version which should be faster.
class SignedShiftableBits a where
   -- | Checked signed right shift
   signedShiftR :: a -> Word -> a

   -- | Checked signed left shift
   signedShiftL :: a -> Word -> a

   -- | Unchecked signed right shift
   uncheckedSignedShiftR :: a -> Word -> a

   -- | Unchecked signed left shift
   uncheckedSignedShiftL :: a -> Word -> a

   -- | Checked signed shift to the left if positive, to the right if negative
   signedShift :: a -> Int -> a
   signedShift a i
      | i > 0     = signedShiftL a (fromIntegral i)
      | i < 0     = signedShiftR a (fromIntegral (negate i))
      | otherwise = a

   -- | Unchecked signed shift to the left if positive, to the right if negative
   uncheckedSignedShift :: a -> Int -> a
   uncheckedSignedShift a i
      | i > 0     = uncheckedSignedShiftL a (fromIntegral i)
      | i < 0     = uncheckedSignedShiftR a (fromIntegral (negate i))
      | otherwise = a


instance ShiftableBits Word where
   {-# INLINABLE shiftR #-}
   {-# INLINABLE shiftL #-}
   {-# INLINABLE uncheckedShiftL #-}
   {-# INLINABLE uncheckedShiftR #-}

   (W# x#) `shiftL`          (W# i#)
      | isTrue# (i# `geWord#` WORD_SIZE_IN_BITS##) = W# 0##
      | otherwise                                  = W# (x# `uncheckedShiftL#` word2Int# i#)
   (W# x#) `shiftR`          (W# i#)
      | isTrue# (i# `geWord#` WORD_SIZE_IN_BITS##) = W# 0##
      | otherwise                                  = W# (x# `uncheckedShiftRL#` word2Int# i#)
   (W# x#) `uncheckedShiftL` (W# i#) = W# (x# `uncheckedShiftL#` word2Int# i#)
   (W# x#) `uncheckedShiftR` (W# i#) = W# (x# `uncheckedShiftRL#` word2Int# i#)

instance ShiftableBits Word8 where
   {-# INLINABLE shiftR #-}
   {-# INLINABLE shiftL #-}
   {-# INLINABLE uncheckedShiftL #-}
   {-# INLINABLE uncheckedShiftR #-}

   (W8# x#) `shiftL` (W# i#)
      | isTrue# (i# `geWord#` 8##)    = W8# 0##
      | otherwise                     = W8# (narrow8Word# (x# `uncheckedShiftL#` word2Int# i#))

   (W8# x#) `uncheckedShiftL` (W# i#) = W8# (narrow8Word# (x# `uncheckedShiftL#` word2Int# i#))
   
   (W8# x#) `shiftR` (W# i#)
      | isTrue# (i# `geWord#` 8##)    = W8# 0##
      | otherwise                     = W8# (x# `uncheckedShiftRL#` word2Int# i#)
   
   (W8# x#) `uncheckedShiftR` (W# i#) = W8# (x# `uncheckedShiftRL#` word2Int# i#)

instance ShiftableBits Word16 where
   {-# INLINABLE shiftR #-}
   {-# INLINABLE shiftL #-}
   {-# INLINABLE uncheckedShiftL #-}
   {-# INLINABLE uncheckedShiftR #-}

   (W16# x#) `shiftL` (W# i#)
      | isTrue# (i# `geWord#` 16##)    = W16# 0##
      | otherwise                      = W16# (narrow16Word# (x# `uncheckedShiftL#` word2Int# i#))

   (W16# x#) `uncheckedShiftL` (W# i#) = W16# (narrow16Word# (x# `uncheckedShiftL#` word2Int# i#))
   
   (W16# x#) `shiftR` (W# i#)
      | isTrue# (i# `geWord#` 16##)    = W16# 0##
      | otherwise                      = W16# (x# `uncheckedShiftRL#` word2Int# i#)
   
   (W16# x#) `uncheckedShiftR` (W# i#) = W16# (x# `uncheckedShiftRL#` word2Int# i#)

instance ShiftableBits Word32 where
   {-# INLINABLE shiftR #-}
   {-# INLINABLE shiftL #-}
   {-# INLINABLE uncheckedShiftL #-}
   {-# INLINABLE uncheckedShiftR #-}

   (W32# x#) `shiftL` (W# i#)
      | isTrue# (i# `geWord#` 32##)    = W32# 0##
      | otherwise                      = W32# (narrow32Word# (x# `uncheckedShiftL#` word2Int# i#))

   (W32# x#) `uncheckedShiftL` (W# i#) = W32# (narrow32Word# (x# `uncheckedShiftL#` word2Int# i#))
   
   (W32# x#) `shiftR` (W# i#)
      | isTrue# (i# `geWord#` 32##)    = W32# 0##
      | otherwise                      = W32# (x# `uncheckedShiftRL#` word2Int# i#)
   
   (W32# x#) `uncheckedShiftR` (W# i#) = W32# (x# `uncheckedShiftRL#` word2Int# i#)

instance ShiftableBits Word64 where
   {-# INLINABLE shiftR #-}
   {-# INLINABLE shiftL #-}
   {-# INLINABLE uncheckedShiftL #-}
   {-# INLINABLE uncheckedShiftR #-}

   (W64# x#) `shiftL` (W# i#)
      | isTrue# (i# `geWord#` 64##)    = W64# 0##
      | otherwise                      = W64# (x# `uncheckedShiftL#` word2Int# i#)

   (W64# x#) `uncheckedShiftL` (W# i#) = W64# (x# `uncheckedShiftL#` word2Int# i#)
   
   (W64# x#) `shiftR` (W# i#)
      | isTrue# (i# `geWord#` 64##)    = W64# 0##
      | otherwise                      = W64# (x# `uncheckedShiftRL#` word2Int# i#)
   
   (W64# x#) `uncheckedShiftR` (W# i#) = W64# (x# `uncheckedShiftRL#` word2Int# i#)


instance ShiftableBits Int where
   {-# INLINABLE shiftR #-}
   {-# INLINABLE shiftL #-}
   {-# INLINABLE uncheckedShiftL #-}
   {-# INLINABLE uncheckedShiftR #-}

   (I# x#) `shiftL`          (W# i#)
      | isTrue# (i# `geWord#` WORD_SIZE_IN_BITS##) = I# 0#
      | otherwise                                  = I# (x# `uncheckedIShiftL#` word2Int# i#)

   (I# x#) `uncheckedShiftL` (W# i#)               = I# (x# `uncheckedIShiftL#` word2Int# i#)
   
   (I# x#) `shiftR`          (W# i#)
      | isTrue# (i# `geWord#` WORD_SIZE_IN_BITS##) = I# 0#
      | otherwise                                  = I# (x# `uncheckedIShiftRL#` word2Int# i#)
   
   (I# x#) `uncheckedShiftR` (W# i#)               = I# (x# `uncheckedIShiftRL#` word2Int# i#)

instance ShiftableBits Int8 where
   {-# INLINABLE shiftR #-}
   {-# INLINABLE shiftL #-}
   {-# INLINABLE uncheckedShiftL #-}
   {-# INLINABLE uncheckedShiftR #-}

   (I8# x#) `shiftL`          (W# i#)
      | isTrue# (i# `geWord#` 8##)    = I8# 0#
      | otherwise                     = I8# (narrow8Int# (x# `uncheckedIShiftL#` word2Int# i#))

   (I8# x#) `uncheckedShiftL` (W# i#) = I8# (narrow8Int# (x# `uncheckedIShiftL#` word2Int# i#))
   
   (I8# x#) `shiftR`          (W# i#)
      | isTrue# (i# `geWord#` 8##)    = I8# 0#
      | otherwise                     = I8# (word2Int# (narrow8Word# (int2Word# x#) `uncheckedShiftRL#` word2Int# i#))

   (I8# x#) `uncheckedShiftR` (W# i#) = I8# (word2Int# (narrow8Word# (int2Word# x#) `uncheckedShiftRL#` word2Int# i#))
   

instance ShiftableBits Int16 where
   {-# INLINABLE shiftR #-}
   {-# INLINABLE shiftL #-}
   {-# INLINABLE uncheckedShiftL #-}
   {-# INLINABLE uncheckedShiftR #-}

   (I16# x#) `shiftL`          (W# i#)
      | isTrue# (i# `geWord#` 16##)    = I16# 0#
      | otherwise                      = I16# (narrow16Int# (x# `uncheckedIShiftL#` word2Int# i#))

   (I16# x#) `uncheckedShiftL` (W# i#) = I16# (narrow16Int# (x# `uncheckedIShiftL#` word2Int# i#))
   
   (I16# x#) `shiftR`          (W# i#)
      | isTrue# (i# `geWord#` 16##)    = I16# 0#
      | otherwise                      = I16# (word2Int# (narrow16Word# (int2Word# x#) `uncheckedShiftRL#` word2Int# i#))

   (I16# x#) `uncheckedShiftR` (W# i#) = I16# (word2Int# (narrow16Word# (int2Word# x#) `uncheckedShiftRL#` word2Int# i#))


instance ShiftableBits Int32 where
   {-# INLINABLE shiftR #-}
   {-# INLINABLE shiftL #-}
   {-# INLINABLE uncheckedShiftL #-}
   {-# INLINABLE uncheckedShiftR #-}

   (I32# x#) `shiftL`          (W# i#)
      | isTrue# (i# `geWord#` 32##)    = I32# 0#
      | otherwise                      = I32# (narrow32Int# (x# `uncheckedIShiftL#` word2Int# i#))

   (I32# x#) `uncheckedShiftL` (W# i#) = I32# (narrow32Int# (x# `uncheckedIShiftL#` word2Int# i#))
   
   (I32# x#) `shiftR`          (W# i#)
      | isTrue# (i# `geWord#` 32##)    = I32# 0#
      | otherwise                      = I32# (word2Int# (narrow32Word# (int2Word# x#) `uncheckedShiftRL#` word2Int# i#))

   (I32# x#) `uncheckedShiftR` (W# i#) = I32# (word2Int# (narrow32Word# (int2Word# x#) `uncheckedShiftRL#` word2Int# i#))

instance ShiftableBits Int64 where
   {-# INLINABLE shiftR #-}
   {-# INLINABLE shiftL #-}
   {-# INLINABLE uncheckedShiftL #-}
   {-# INLINABLE uncheckedShiftR #-}

   (I64# x#) `shiftL`          (W# i#)
      | isTrue# (i# `geWord#` 64##)    = I64# 0#
      | otherwise                      = I64# (x# `uncheckedIShiftL#` word2Int# i#)

   (I64# x#) `uncheckedShiftL` (W# i#) = I64# (x# `uncheckedIShiftL#` word2Int# i#)
   
   (I64# x#) `shiftR`          (W# i#)
      | isTrue# (i# `geWord#` 64##)    = I64# 0#
      | otherwise                      = I64# (word2Int# (int2Word# x# `uncheckedShiftRL#` word2Int# i#))

   (I64# x#) `uncheckedShiftR` (W# i#) = I64# (word2Int# (int2Word# x# `uncheckedShiftRL#` word2Int# i#))


instance SignedShiftableBits Int where
   (I# x#) `signedShiftL`          (W# i#) = I# (x# `iShiftL#` word2Int# i#)
   (I# x#) `signedShiftR`          (W# i#) = I# (x# `iShiftRA#` word2Int# i#)
   (I# x#) `uncheckedSignedShiftL` (W# i#) = I# (x# `uncheckedIShiftL#` word2Int# i#)
   (I# x#) `uncheckedSignedShiftR` (W# i#) = I# (x# `uncheckedIShiftRA#` word2Int# i#)

instance SignedShiftableBits Int8 where
   (I8# x#) `signedShiftL`          (W# i#) = I8# (narrow8Int# (x# `iShiftL#` word2Int# i#))
   (I8# x#) `signedShiftR`          (W# i#) = I8# (x# `iShiftRA#` word2Int# i#)
   (I8# x#) `uncheckedSignedShiftL` (W# i#) = I8# (narrow8Int# (x# `uncheckedIShiftL#` word2Int# i#))
   (I8# x#) `uncheckedSignedShiftR` (W# i#) = I8# (x# `uncheckedIShiftRA#` word2Int# i#)

instance SignedShiftableBits Int16 where
   (I16# x#) `signedShiftL`          (W# i#) = I16# (narrow16Int# (x# `iShiftL#` word2Int# i#))
   (I16# x#) `signedShiftR`          (W# i#) = I16# (x# `iShiftRA#` word2Int# i#)
   (I16# x#) `uncheckedSignedShiftL` (W# i#) = I16# (narrow16Int# (x# `uncheckedIShiftL#` word2Int# i#))
   (I16# x#) `uncheckedSignedShiftR` (W# i#) = I16# (x# `uncheckedIShiftRA#` word2Int# i#)

instance SignedShiftableBits Int32 where
   (I32# x#) `signedShiftL`          (W# i#) = I32# (narrow32Int# (x# `iShiftL#` word2Int# i#))
   (I32# x#) `signedShiftR`          (W# i#) = I32# (x# `iShiftRA#` word2Int# i#)
   (I32# x#) `uncheckedSignedShiftL` (W# i#) = I32# (narrow32Int# (x# `uncheckedIShiftL#` word2Int# i#))
   (I32# x#) `uncheckedSignedShiftR` (W# i#) = I32# (x# `uncheckedIShiftRA#` word2Int# i#)

instance SignedShiftableBits Int64 where
   (I64# x#) `signedShiftL`          (W# i#) = I64# (x# `iShiftL#` word2Int# i#)
   (I64# x#) `signedShiftR`          (W# i#) = I64# (x# `iShiftRA#` word2Int# i#)
   (I64# x#) `uncheckedSignedShiftL` (W# i#) = I64# (x# `uncheckedIShiftL#` word2Int# i#)
   (I64# x#) `uncheckedSignedShiftR` (W# i#) = I64# (x# `uncheckedIShiftRA#` word2Int# i#)



instance ShiftableBits Integer where
   {-# INLINABLE shiftR #-}
   {-# INLINABLE shiftL #-}
   {-# INLINABLE uncheckedShiftL #-}
   {-# INLINABLE uncheckedShiftR #-}

   x `shiftL` w = integerShiftL x w
   x `shiftR` w = integerShiftR x w

   uncheckedShiftL = shiftL
   uncheckedShiftR = shiftR

instance ShiftableBits Natural where
   {-# INLINABLE shiftR #-}
   {-# INLINABLE shiftL #-}
   {-# INLINABLE uncheckedShiftL #-}
   {-# INLINABLE uncheckedShiftR #-}

   x `shiftL` w = naturalShiftL x w
   x `shiftR` w = naturalShiftR x w

   uncheckedShiftL = shiftL
   uncheckedShiftR = shiftR
