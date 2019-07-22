{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Haskus.Time.Calendar
   ( Year
   , Month
   , DayOfMonth
   , YearMonthDay (..)
   , showYearMonthDay
   , isLeapYear
   , monthLength
   , nextDay
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Bits
import Haskus.Memory.Embed
import Haskus.Memory.Buffer

type Year       = Word

-- | Month index [1..12]
type Month      = Word8

-- | Day of month index [1..31]
type DayOfMonth = Word8

-- | Gregorian calendar day
data YearMonthDay = YearMonthDay
   { ymdYear  :: {-# UNPACK #-} !Year
   , ymdMonth :: {-# UNPACK #-} !Month
   , ymdDay   :: {-# UNPACK #-} !DayOfMonth
   }
   deriving (Show,Eq,Ord)

-- | Show a date as YEAR-MONTH-DAY
--
-- >>> showYearMonthDay (YearMonthDay 2017 02 27)
-- "2017-2-27"
showYearMonthDay :: YearMonthDay -> String
showYearMonthDay YearMonthDay{..} = mconcat
   [ show ymdYear , "-"
   , show ymdMonth, "-"
   , show ymdDay
   ]

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

-- | Table of number of days in each month (as Word8)
monthDaysTable :: BufferE
monthDaysTable = $$(embedBytes [31,28,31,30,31,30,31,31,30,31,30,31])

-- | Table of number of days in each month for a leap year (as Word8)
monthDaysLeapTable :: BufferE
monthDaysLeapTable = $$(embedBytes [31,29,31,30,31,30,31,31,30,31,30,31])

-- | Get month length
--
-- >>> monthLength 2019 2
-- 28
-- >>> monthLength 2016 2
-- 29
monthLength :: Year -> Month -> Word8
monthLength y m = bufferReadWord8 table (fromIntegral m - 1)
   where
      table
         | isLeapYear y = monthDaysLeapTable
         | otherwise    = monthDaysTable

-- | Get the next day
--
-- >>> showYearMonthDay (nextDay (YearMonthDay 2019 12 31))
-- "2020-1-1"
-- >>> showYearMonthDay (nextDay (YearMonthDay 2019 2  28))
-- "2019-3-1"
-- >>> showYearMonthDay (nextDay (YearMonthDay 2016 2  28))
-- "2016-2-29"
nextDay :: YearMonthDay -> YearMonthDay
nextDay YearMonthDay{..}
   | ymdDay < monthLength ymdYear ymdMonth = YearMonthDay ymdYear ymdMonth (ymdDay+1)
   | ymdMonth == 12                        = YearMonthDay (ymdYear+1) 1 1
   | otherwise                             = YearMonthDay ymdYear (ymdMonth+1) 1
