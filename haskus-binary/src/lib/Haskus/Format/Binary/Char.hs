{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Character
module Haskus.Format.Binary.Char
   ( Char8 (..)
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Storable

-- | 8-bit character (ASCII, etc.)
newtype Char8
   = Char8 Word8
   deriving (Show,Eq,Ord,Storable)
