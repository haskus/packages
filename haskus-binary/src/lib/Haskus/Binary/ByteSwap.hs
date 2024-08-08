-- | Byte swapping
module Haskus.Binary.ByteSwap
  ( byteSwapU#
  , byteSwapU16#
  , byteSwapU32#
  , byteSwapU64#
  , byteSwapI#
  , byteSwapI16#
  , byteSwapI32#
  , byteSwapI64#
  )
where

import qualified GHC.Exts as E

import Haskus.Binary.Word
import Haskus.Binary.Int
import Haskus.Binary.Cast

byteSwapU# :: U# -> U#
byteSwapU# = E.byteSwap#

-- TODO: fix GHC's byteSwap16/32 to take Word16#/Word32#
byteSwapU16# :: U16# -> U16#
byteSwapU16# w = u16NarrowFromU# (E.byteSwap16# (uFromU16# w))

byteSwapU32# :: U32# -> U32#
byteSwapU32# w = u32NarrowFromU# (E.byteSwap32# (uFromU32# w))

byteSwapU64# :: U64# -> U64#
byteSwapU64# = E.byteSwap64#

byteSwapI# :: I# -> I#
byteSwapI# w = iFromU# (byteSwapU# (uFromI# w))

byteSwapI16# :: I16# -> I16#
byteSwapI16# w = i16FromU16# (byteSwapU16# (u16FromI16# w))

byteSwapI32# :: I32# -> I32#
byteSwapI32# w = i32FromU32# (byteSwapU32# (u32FromI32# w))

byteSwapI64# :: I64# -> I64#
byteSwapI64# w = i64FromU64# (byteSwapU64# (u64FromI64# w))
