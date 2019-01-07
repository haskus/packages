{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

-- | Bitwise bit operations
module Haskus.Format.Binary.Bits.Bitwise
   ( Bitwise (..)
   )
where

import Haskus.Format.Binary.Word
import GHC.Exts
import GHC.Num

-- | Bitwise bit operations
class Bitwise a where
   -- | Bitwise "and"
   (.&.) :: a -> a -> a

   -- | Bitwise "or"
   (.|.) :: a -> a -> a

   -- | Bitwise "xor"
   xor :: a -> a -> a

   -- | Complement
   complement :: a -> a


instance Bitwise Word where
   (W# x#) .&.   (W# y#) = W# (x# `and#` y#)
   (W# x#) .|.   (W# y#) = W# (x# `or#` y#)
   (W# x#) `xor` (W# y#) = W# (x# `xor#` y#)
   complement (W# x#)    = W# (x# `xor#` mb#)
      where !(W# mb#) = maxBound

instance Bitwise Word8 where
   (W8# x#) .&.   (W8# y#) = W8# (x# `and#` y#)
   (W8# x#) .|.   (W8# y#) = W8# (x# `or#` y#)
   (W8# x#) `xor` (W8# y#) = W8# (x# `xor#` y#)
   complement (W8# x#)     = W8# (x# `xor#` mb#)
      where !(W8# mb#) = maxBound

instance Bitwise Word16 where
   (W16# x#) .&.   (W16# y#) = W16# (x# `and#` y#)
   (W16# x#) .|.   (W16# y#) = W16# (x# `or#` y#)
   (W16# x#) `xor` (W16# y#) = W16# (x# `xor#` y#)
   complement (W16# x#)      = W16# (x# `xor#` mb#)
      where !(W16# mb#) = maxBound

instance Bitwise Word32 where
   (W32# x#) .&.   (W32# y#) = W32# (x# `and#` y#)
   (W32# x#) .|.   (W32# y#) = W32# (x# `or#` y#)
   (W32# x#) `xor` (W32# y#) = W32# (x# `xor#` y#)
   complement (W32# x#)      = W32# (x# `xor#` mb#)
      where !(W32# mb#) = maxBound

instance Bitwise Word64 where
   (W64# x#) .&.   (W64# y#) = W64# (x# `and#` y#)
   (W64# x#) .|.   (W64# y#) = W64# (x# `or#` y#)
   (W64# x#) `xor` (W64# y#) = W64# (x# `xor#` y#)
   complement (W64# x#)      = W64# (x# `xor#` mb#)
      where !(W64# mb#) = maxBound

instance Bitwise Int where
   (I# x#) .&.   (I# y#) = I# (x# `andI#` y#)
   (I# x#) .|.   (I# y#) = I# (x# `orI#` y#)
   (I# x#) `xor` (I# y#) = I# (x# `xorI#` y#)
   complement (I# x#)    = I# (notI# x#)

instance Bitwise Int8 where
   (I8# x#) .&.   (I8# y#) = I8# (word2Int# (int2Word# x# `and#` int2Word# y#))
   (I8# x#) .|.   (I8# y#) = I8# (word2Int# (int2Word# x# `or#`  int2Word# y#))
   (I8# x#) `xor` (I8# y#) = I8# (word2Int# (int2Word# x# `xor#` int2Word# y#))
   complement (I8# x#)     = I8# (word2Int# (not# (int2Word# x#)))

instance Bitwise Int16 where
   (I16# x#) .&.   (I16# y#) = I16# (word2Int# (int2Word# x# `and#` int2Word# y#))
   (I16# x#) .|.   (I16# y#) = I16# (word2Int# (int2Word# x# `or#`  int2Word# y#))
   (I16# x#) `xor` (I16# y#) = I16# (word2Int# (int2Word# x# `xor#` int2Word# y#))
   complement (I16# x#)      = I16# (word2Int# (not# (int2Word# x#)))

instance Bitwise Int32 where
   (I32# x#) .&.   (I32# y#) = I32# (word2Int# (int2Word# x# `and#` int2Word# y#))
   (I32# x#) .|.   (I32# y#) = I32# (word2Int# (int2Word# x# `or#`  int2Word# y#))
   (I32# x#) `xor` (I32# y#) = I32# (word2Int# (int2Word# x# `xor#` int2Word# y#))
   complement (I32# x#)      = I32# (word2Int# (not# (int2Word# x#)))

instance Bitwise Int64 where
   (I64# x#) .&.   (I64# y#) = I64# (word2Int# (int2Word# x# `and#` int2Word# y#))
   (I64# x#) .|.   (I64# y#) = I64# (word2Int# (int2Word# x# `or#`  int2Word# y#))
   (I64# x#) `xor` (I64# y#) = I64# (word2Int# (int2Word# x# `xor#` int2Word# y#))
   complement (I64# x#)      = I64# (word2Int# (int2Word# x# `xor#` int2Word# (-1#)))

instance Bitwise Integer where
   (.&.)      = andInteger
   (.|.)      = orInteger
   xor        = xorInteger
   complement = complementInteger
