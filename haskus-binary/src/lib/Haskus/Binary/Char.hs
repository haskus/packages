{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Character
module Haskus.Binary.Char
   ( Char8 (..)
   )
where

import Haskus.Number.Word
import Haskus.Binary.Storable

-- | 8-bit character (ASCII, etc.)
newtype Char8
   = Char8 Word8
   deriving (Show,Eq,Ord,Storable)
