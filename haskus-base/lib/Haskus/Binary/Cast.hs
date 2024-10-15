module Haskus.Binary.Cast
  ( -- * Bitcasts
    uFromI#
  , u8FromI8#
  , u16FromI16#
  , u16FromI16
  , u32FromI32#
  , u64FromI64#
  , iFromU#
  , i8FromU8#
  , i8FromU8
  , i16FromU16#
  , i32FromU32#
  , i64FromU64#
  -- * Extensions
  , uFromU8#
  , uFromU16#
  , uFromU32#
  , u64FromU#
  , u64FromU8#
  , u64FromU16#
  , u64FromU32#
  , iFromI8#
  , iFromI16#
  , iFromI32#
  , i16FromI8#
  , i16FromI8
  , i32FromI8#
  , i32FromI8
  , i32FromI16#
  , i32FromI16
  , i64FromI#
  , i64FromI8#
  , i64FromI16#
  , i64FromI32#
  -- * Narrowing
  , u8NarrowFromU#
  , u16NarrowFromU#
  , u32NarrowFromU#
  , i8NarrowFromI#
  , i16NarrowFromI#
  , i32NarrowFromI#
  , narrowU64ToU32#
  , narrowU64ToU32
  )
where

import Haskus.Binary.Int
import Haskus.Binary.Word

import qualified GHC.Exts as E

---------------------------
-- Bitcasts
---------------------------

uFromI# :: I# -> U#
uFromI# = E.int2Word#

u8FromI8# :: I8# -> U8#
u8FromI8# = E.int8ToWord8#

u16FromI16# :: I16# -> U16#
u16FromI16# = E.int16ToWord16#

u16FromI16 :: I16 -> U16
u16FromI16 (I16 x) = U16 (u16FromI16# x)

u32FromI32# :: I32# -> U32#
u32FromI32# = E.int32ToWord32#

u64FromI64# :: I64# -> U64#
u64FromI64# = E.int64ToWord64#

iFromU# :: U# -> I#
iFromU# = E.word2Int#

i8FromU8# :: U8# -> I8#
i8FromU8# = E.word8ToInt8#

i8FromU8 :: U8 -> I8
i8FromU8 (U8 x) = I8 (i8FromU8# x)

i16FromU16# :: U16# -> I16#
i16FromU16# = E.word16ToInt16#

i32FromU32# :: U32# -> I32#
i32FromU32# = E.word32ToInt32#

i64FromU64# :: U64# -> I64#
i64FromU64# = E.word64ToInt64#

---------------------------
-- Extensions
---------------------------

uFromU8# :: U8# -> U#
uFromU8# = E.word8ToWord#

uFromU16# :: U16# -> U#
uFromU16# = E.word16ToWord#

uFromU32# :: U32# -> U#
uFromU32# = E.word32ToWord#

u64FromU# :: U# -> U64#
u64FromU# = E.wordToWord64#

u64FromU8# :: U8# -> U64#
u64FromU8# w = u64FromU# (uFromU8# w)

u64FromU16# :: U16# -> U64#
u64FromU16# w = u64FromU# (uFromU16# w)

u64FromU32# :: U32# -> U64#
u64FromU32# w = u64FromU# (uFromU32# w)


iFromI8# :: I8# -> I#
iFromI8# = E.int8ToInt#

iFromI16# :: I16# -> I#
iFromI16# = E.int16ToInt#

iFromI32# :: I32# -> I#
iFromI32# = E.int32ToInt#

i16FromI8# :: I8# -> I16#
i16FromI8# x = i16NarrowFromI# (iFromI8# x)

i16FromI8 :: I8 -> I16
i16FromI8 (I8 x) = I16 (i16FromI8# x)

i32FromI8# :: I8# -> I32#
i32FromI8# x = i32NarrowFromI# (iFromI8# x)

i32FromI8 :: I8 -> I32
i32FromI8 (I8 x) = I32 (i32FromI8# x)

i32FromI16# :: I16# -> I32#
i32FromI16# x = i32NarrowFromI# (iFromI16# x)

i32FromI16 :: I16 -> I32
i32FromI16 (I16 x) = I32 (i32FromI16# x)

i64FromI# :: I# -> I64#
i64FromI# = E.intToInt64#

i64FromI8# :: I8# -> I64#
i64FromI8# w = i64FromI# (iFromI8# w)

i64FromI16# :: I16# -> I64#
i64FromI16# w = i64FromI# (iFromI16# w)

i64FromI32# :: I32# -> I64#
i64FromI32# w = i64FromI# (iFromI32# w)

--------------------------
-- Truncations
--------------------------

u8NarrowFromU# :: U# -> U8#
u8NarrowFromU# = E.wordToWord8#

u16NarrowFromU# :: U# -> U16#
u16NarrowFromU# = E.wordToWord16#

u32NarrowFromU# :: U# -> U32#
u32NarrowFromU# = E.wordToWord32#

i8NarrowFromI# :: I# -> I8#
i8NarrowFromI# = E.intToInt8#

i16NarrowFromI# :: I# -> I16#
i16NarrowFromI# = E.intToInt16#

i32NarrowFromI# :: I# -> I32#
i32NarrowFromI# = E.intToInt32#

narrowU64ToU32# :: U64# -> U32#
narrowU64ToU32# w = E.wordToWord32# (E.word64ToWord# w)

narrowU64ToU32 :: U64 -> U32
narrowU64ToU32 (U64 w) = U32 (narrowU64ToU32# w)
