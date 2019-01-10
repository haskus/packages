{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- | Unicode
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
import Haskus.Utils.Types
import Numeric
import Data.Kind
import Data.Char (toUpper)

-- Unicode encodings
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

-- | Unicode encoding scheme
class Encoding a where
   -- | Code unit type
   type CodeUnit a :: Type

   -- | Maximum number of code unit per code point
   type MaxCodeUnit a :: Nat

   -- | Binary sorting is equivalent to code point sorting
   type BinarySorting a :: Bool

   -- | BOM (byte-order mask) allowed
   type AllowedBOM a :: Bool


instance Encoding UTF8 where
   type CodeUnit        UTF8 = Word8
   type MaxCodeUnit     UTF8 = 4
   type BinarySorting   UTF8 = 'True
   type AllowedBOM      UTF8 = 'True

instance Encoding UTF16_BOM where
   type CodeUnit        UTF16_BOM = Word16
   type MaxCodeUnit     UTF16_BOM = 2
   type BinarySorting   UTF16_BOM = 'False
   type AllowedBOM      UTF16_BOM = 'True

instance Encoding UTF16_BE where
   type CodeUnit        UTF16_BE = Word16
   type MaxCodeUnit     UTF16_BE = 2
   type BinarySorting   UTF16_BE = 'False
   type AllowedBOM      UTF16_BE = 'False

instance Encoding UTF16_LE where
   type CodeUnit        UTF16_LE = Word16
   type MaxCodeUnit     UTF16_LE = 2
   type BinarySorting   UTF16_LE = 'False
   type AllowedBOM      UTF16_LE = 'False

instance Encoding UTF32_BOM where
   type CodeUnit        UTF32_BOM = Word32
   type MaxCodeUnit     UTF32_BOM = 1
   type BinarySorting   UTF32_BOM = 'True
   type AllowedBOM      UTF32_BOM = 'True

instance Encoding UTF32_BE where
   type CodeUnit        UTF32_BE = Word32
   type MaxCodeUnit     UTF32_BE = 1
   type BinarySorting   UTF32_BE = 'True
   type AllowedBOM      UTF32_BE = 'False

instance Encoding UTF32_LE where
   type CodeUnit        UTF32_LE = Word32
   type MaxCodeUnit     UTF32_LE = 1
   type BinarySorting   UTF32_LE = 'True
   type AllowedBOM      UTF32_LE = 'False
