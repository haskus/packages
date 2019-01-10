module Haskus.Format.Text.Unicode
   ( CodePoint (..)
   , UTF8 (..)
   , UTF16_BOM (..)
   , UTF16_BE (..)
   , UTF16_LE (..)
   , UTF32_BOM (..)
   , UTF32_BE (..)
   , UTF32_LE (..)
   )
where

import Haskus.Format.Binary.Word
import Numeric

data UTF8      = UTF8
data UTF16_BOM = UTF16_BOM
data UTF16_BE  = UTF16_BE
data UTF16_LE  = UTF16_LE
data UTF32_BOM = UTF32_BOM
data UTF32_BE  = UTF32_BE
data UTF32_LE  = UTF32_LE

-- | Code point
--
-- Number from 0 to 0x10FFFF
newtype CodePoint = CodePoint Word32 deriving (Eq)

-- | Show instance for CodePoint
--
-- >>> CodePoint 0x1234
-- U+1234
--
-- >>> CodePoint 0x12
-- U+0012
--
-- >>> CodePoint 0x1234A
-- U+1234A
--
instance Show CodePoint where
   show (CodePoint v) = "U+" ++ f (fmap toUpper (showHex v ""))
      where
         f xs@[_,_,_] = '0':xs
         f xs@[_,_]   = "00"   <> xs
         f xs@[_]     = "000"  <> xs
         f xs@[]      = "0000" <> xs
         f xs         = xs
