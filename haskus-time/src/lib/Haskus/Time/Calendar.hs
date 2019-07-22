{-# LANGUAGE BangPatterns #-}

module Haskus.Time.Calendar
   ( Year
   , Month
   , DayOfMonth
   , YearMonthDay (..)
   , isLeapYear
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Bits

type Year       = Word
type Month      = Word8
type DayOfMonth = Word8

-- | Gregorian calendar day
data YearMonthDay = YearMonthDay
   { ymdYear  :: {-# UNPACK #-} !Year
   , ymdMonth :: {-# UNPACK #-} !Month
   , ymdDay   :: {-# UNPACK #-} !DayOfMonth
   }
   deriving (Show,Eq,Ord)

-- | In the Gregorian calendar, a year has an additional leap day if:
--    1) it is divisible by 4 and not by 100
--    2) or if it is divisible by 400
--
-- >>> isLeapYear 2019
-- False
-- >>> isLeapYear 2016
-- True
-- >>> isLeapYear 1900
-- False
-- >>> isLeapYear 2000
-- True
isLeapYear :: Year -> Bool
isLeapYear y = y .&. 3 == 0 && (r100 /= 0 || q100 .&. 3 == 0) where
   (q100, r100) = y `quotRem` 100
