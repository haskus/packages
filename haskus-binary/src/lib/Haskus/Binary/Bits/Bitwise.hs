{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

-- | Bitwise bit operations
module Haskus.Binary.Bits.Bitwise
   ( Bitwise (..)
   )
where

import Haskus.Number.Word
import Haskus.Number.Int
import GHC.Exts
import GHC.Num

#if MIN_VERSION_GLASGOW_HASKELL (9,0,0,0)
import GHC.Natural
import GHC.Integer
#endif

-- | Bitwise bit operations
class Bitwise a where
   -- | Bitwise "and"
   (.&.) :: a -> a -> a

   -- | Bitwise "or"
   (.|.) :: a -> a -> a

   -- | Bitwise "xor"
   xor :: a -> a -> a


instance Bitwise Word where
   (W# x#) .&.   (W# y#) = W# (x# `and#` y#)
   (W# x#) .|.   (W# y#) = W# (x# `or#` y#)
   (W# x#) `xor` (W# y#) = W# (x# `xor#` y#)

instance Bitwise Word8 where
#if MIN_VERSION_GLASGOW_HASKELL (9,2,0,0)
   (W8# x#) .&.   (W8# y#) = W8# (x# `andWord8#` y#)
   (W8# x#) .|.   (W8# y#) = W8# (x# `orWord8#` y#)
   (W8# x#) `xor` (W8# y#) = W8# (x# `xorWord8#` y#)
#else
   (W8# x#) .&.   (W8# y#) = W8# (x# `and#` y#)
   (W8# x#) .|.   (W8# y#) = W8# (x# `or#` y#)
   (W8# x#) `xor` (W8# y#) = W8# (x# `xor#` y#)
#endif

instance Bitwise Word16 where
#if MIN_VERSION_GLASGOW_HASKELL (9,2,0,0)
   (W16# x#) .&.   (W16# y#) = W16# (x# `andWord16#` y#)
   (W16# x#) .|.   (W16# y#) = W16# (x# `orWord16#` y#)
   (W16# x#) `xor` (W16# y#) = W16# (x# `xorWord16#` y#)
#else
   (W16# x#) .&.   (W16# y#) = W16# (x# `and#` y#)
   (W16# x#) .|.   (W16# y#) = W16# (x# `or#` y#)
   (W16# x#) `xor` (W16# y#) = W16# (x# `xor#` y#)
#endif

instance Bitwise Word32 where
#if MIN_VERSION_GLASGOW_HASKELL (9,2,0,0)
   (W32# x#) .&.   (W32# y#) = W32# (x# `andWord32#` y#)
   (W32# x#) .|.   (W32# y#) = W32# (x# `orWord32#` y#)
   (W32# x#) `xor` (W32# y#) = W32# (x# `xorWord32#` y#)
#else
   (W32# x#) .&.   (W32# y#) = W32# (x# `and#` y#)
   (W32# x#) .|.   (W32# y#) = W32# (x# `or#` y#)
   (W32# x#) `xor` (W32# y#) = W32# (x# `xor#` y#)
#endif

instance Bitwise Word64 where
#if MIN_VERSION_GLASGOW_HASKELL (9,4,0,0)
   (W64# x#) .&.   (W64# y#) = W64# (x# `and64#` y#)
   (W64# x#) .|.   (W64# y#) = W64# (x# `or64#` y#)
   (W64# x#) `xor` (W64# y#) = W64# (x# `xor64#` y#)
#else
   (W64# x#) .&.   (W64# y#) = W64# (x# `and#` y#)
   (W64# x#) .|.   (W64# y#) = W64# (x# `or#` y#)
   (W64# x#) `xor` (W64# y#) = W64# (x# `xor#` y#)
#endif

instance Bitwise Int where
   (I# x#) .&.   (I# y#) = I# (x# `andI#` y#)
   (I# x#) .|.   (I# y#) = I# (x# `orI#` y#)
   (I# x#) `xor` (I# y#) = I# (x# `xorI#` y#)

instance Bitwise Int8 where
#if MIN_VERSION_GLASGOW_HASKELL (9,2,0,0)
   (I8# x#) .&.   (I8# y#) = I8# (intToInt8# (int8ToInt# x# `andI#` int8ToInt# y#))
   (I8# x#) .|.   (I8# y#) = I8# (intToInt8# (int8ToInt# x# `orI#`  int8ToInt# y#))
   (I8# x#) `xor` (I8# y#) = I8# (intToInt8# (int8ToInt# x# `xorI#` int8ToInt# y#))
#else
   (I8# x#) .&.   (I8# y#) = I8# (word2Int# (int2Word# x# `and#` int2Word# y#))
   (I8# x#) .|.   (I8# y#) = I8# (word2Int# (int2Word# x# `or#`  int2Word# y#))
   (I8# x#) `xor` (I8# y#) = I8# (word2Int# (int2Word# x# `xor#` int2Word# y#))
#endif

instance Bitwise Int16 where
#if MIN_VERSION_GLASGOW_HASKELL (9,2,0,0)
   (I16# x#) .&.   (I16# y#) = I16# (intToInt16# (int16ToInt# x# `andI#` int16ToInt# y#))
   (I16# x#) .|.   (I16# y#) = I16# (intToInt16# (int16ToInt# x# `orI#`  int16ToInt# y#))
   (I16# x#) `xor` (I16# y#) = I16# (intToInt16# (int16ToInt# x# `xorI#` int16ToInt# y#))
#else
   (I16# x#) .&.   (I16# y#) = I16# (word2Int# (int2Word# x# `and#` int2Word# y#))
   (I16# x#) .|.   (I16# y#) = I16# (word2Int# (int2Word# x# `or#`  int2Word# y#))
   (I16# x#) `xor` (I16# y#) = I16# (word2Int# (int2Word# x# `xor#` int2Word# y#))
#endif

instance Bitwise Int32 where
#if MIN_VERSION_GLASGOW_HASKELL (9,2,0,0)
   (I32# x#) .&.   (I32# y#) = I32# (intToInt32# (int32ToInt# x# `andI#` int32ToInt# y#))
   (I32# x#) .|.   (I32# y#) = I32# (intToInt32# (int32ToInt# x# `orI#`  int32ToInt# y#))
   (I32# x#) `xor` (I32# y#) = I32# (intToInt32# (int32ToInt# x# `xorI#` int32ToInt# y#))
#else
   (I32# x#) .&.   (I32# y#) = I32# (word2Int# (int2Word# x# `and#` int2Word# y#))
   (I32# x#) .|.   (I32# y#) = I32# (word2Int# (int2Word# x# `or#`  int2Word# y#))
   (I32# x#) `xor` (I32# y#) = I32# (word2Int# (int2Word# x# `xor#` int2Word# y#))
#endif

instance Bitwise Int64 where
#if MIN_VERSION_GLASGOW_HASKELL (9,4,0,0)
   (I64# x#) .&.   (I64# y#) = I64# (word64ToInt64# (int64ToWord64# x# `and64#` int64ToWord64# y#))
   (I64# x#) .|.   (I64# y#) = I64# (word64ToInt64# (int64ToWord64# x# `or64#`  int64ToWord64# y#))
   (I64# x#) `xor` (I64# y#) = I64# (word64ToInt64# (int64ToWord64# x# `xor64#` int64ToWord64# y#))
#else
   (I64# x#) .&.   (I64# y#) = I64# (word2Int# (int2Word# x# `and#` int2Word# y#))
   (I64# x#) .|.   (I64# y#) = I64# (word2Int# (int2Word# x# `or#`  int2Word# y#))
   (I64# x#) `xor` (I64# y#) = I64# (word2Int# (int2Word# x# `xor#` int2Word# y#))
#endif

instance Bitwise Integer where
   (.&.)      = andInteger
   (.|.)      = orInteger
   xor        = xorInteger

instance Bitwise Natural where
   (.&.)      = andNatural
   (.|.)      = orNatural
   xor        = xorNatural
