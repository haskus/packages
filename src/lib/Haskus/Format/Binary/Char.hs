module Haskus.Format.Binary.Char
   ( Char8 (..)
   )
where

import Haskus.Format.Binary.Word

-- | 8-bit character (ASCII, etc.)
newtype Char8 = Char8 Word8
