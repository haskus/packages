{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}

-- | Unicode
module Haskus.Format.Text.Unicode
   ( CodePoint (..)
   -- * Planes
   , Plane (..)
   , codePointPlane
   , planeBMP
   , planeSMP
   , planeSIP
   , planeSSP
   , planePrivates
   -- * Encodings
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
import Haskus.Format.Binary.Bits
import Haskus.Utils.Types
import Numeric
import Data.Kind
import Data.Char (toUpper)
import Language.Haskell.TH.Syntax (Lift)

-- | Code point
--
-- Number from 0 to 0x10FFFF
newtype CodePoint = CodePoint Word32 deriving (Eq,Lift)

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


--------------------------------------------------
-- Planes
--------------------------------------------------

newtype Plane = Plane Word deriving (Show,Eq)

-- | Get the plan the code-point belongs to
--
-- >>> codePointPlane (CodePoint 0x21234)
-- Plane 2
codePointPlane :: CodePoint -> Plane
codePointPlane (CodePoint x) = Plane (fromIntegral (x `uncheckedShiftR` 16))

-- | Basic Multilingual Plane (BMP)
--
--  The Basic Multilingual Plane (BMP, or Plane 0) contains the common-use
--  characters for all the modern scripts of the world as well as many
--  historical and rare characters. By far the majority of all Unicode
--  characters for almost all textual data can be found in the BMP.
planeBMP :: Plane
planeBMP = Plane 0

-- | Supplementary Multilingual Plane (SMP)
--
-- Supplementary Multilingual Plane. The Supplementary Multilingual Plane (SMP,
-- or Plane 1) is dedicated to the encoding of characters for scripts or symbols
-- which either could not be fit into the BMP or see very infrequent usage. This
-- includes many historic scripts, a number of lesser-used contemporary scripts,
-- special-purpose invented scripts, notational systems or large pictographic
-- symbol sets, and occasionally historic extensions of scripts whose core sets
-- are encoded on the BMP.
--
-- Examples include Gothic (historic), Shavian (special-purpose invented),
-- Musical Symbols (notational system), Domino Tiles (pictographic), and Ancient
-- Greek Numbers (historic extension for Greek). A number of scripts, whether of
-- historic and contemporary use, do not yet have their characters encoded in
-- the Unicode Standard. The majority of scripts currently identified for
-- encoding will eventually be allocated in the SMP. As a result, some areas of
-- the SMP will experience common, frequent usage.
planeSMP :: Plane
planeSMP = Plane 1

-- | Supplementary Ideographic Plane (SIP)
--
-- The Supplementary Ideographic Plane (SIP, or Plane 2) is intended as an
-- additional allocation area for those CJK characters that could not be fit in
-- the blocks set aside for more common CJK characters in the BMP. While there
-- are a small number of common-use CJK characters in the SIP (for example, for
-- Cantonese usage), the vast majority of Plane 2 characters are extremely rare
-- or of historical interest only.
planeSIP :: Plane
planeSIP = Plane 2

-- | Supplementary Special-purpose Plane (SSP)
--
-- The Supplementary Special-purpose Plane (SSP, or Plane 14) is the spillover
-- allocation area for format control characters that do not fit into the small
-- allocation areas for format control characters in the BMP.
planeSSP :: Plane
planeSSP = Plane 14

-- | Private Use Planes
--
-- The two Private Use Planes (Planes 15 and 16) are allocated, in their
-- entirety, for private use. Those two planes contain a total of 131,068
-- characters to supple- ment the 6,400 private-use characters located in the
-- BMP.
planePrivates :: [Plane]
planePrivates = [Plane 15, Plane 16]

--------------------------------------------------
-- Encoding
--------------------------------------------------

-- Unicode encodings
data UTF8      = UTF8
data UTF16_BOM = UTF16_BOM
data UTF16_BE  = UTF16_BE
data UTF16_LE  = UTF16_LE
data UTF32_BOM = UTF32_BOM
data UTF32_BE  = UTF32_BE
data UTF32_LE  = UTF32_LE

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
