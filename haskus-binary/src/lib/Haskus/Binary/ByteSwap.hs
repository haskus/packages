-- | Byte swapping
module Haskus.Binary.ByteSwap
  ( byteSwapWord16#
  , byteSwapWord32#
  , byteSwapWord64#
  , byteSwapInt16#
  , byteSwapInt32#
  , byteSwapInt64#
  )
where

import qualified GHC.Exts as E
import GHC.Exts hiding (byteSwap16#, byteSwap32#)

-- TODO: fix GHC's byteSwap16/32 to take Word16#/Word32#
byteSwapWord16# :: Word16# -> Word16#
byteSwapWord16# w = E.wordToWord16# (E.byteSwap16# (E.word16ToWord# w))

byteSwapWord32# :: Word32# -> Word32#
byteSwapWord32# w = E.wordToWord32# (E.byteSwap32# (E.word32ToWord# w))

byteSwapWord64# :: Word64# -> Word64#
byteSwapWord64# = E.byteSwap64#

byteSwapInt16# :: Int16# -> Int16#
byteSwapInt16# w = E.word16ToInt16# (byteSwapWord16# (E.int16ToWord16# w))

byteSwapInt32# :: Int32# -> Int32#
byteSwapInt32# w = E.word32ToInt32# (byteSwapWord32# (E.int32ToWord32# w))

byteSwapInt64# :: Int64# -> Int64#
byteSwapInt64# w = E.word64ToInt64# (byteSwapWord64# (E.int64ToWord64# w))
