{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Unicode code-point
module Haskus.Format.Text.Unicode.CodePoint
   ( CodePoint (..)
   , CodePointRange
   , pattern CodePointRange
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Bits
import Numeric
import Data.Char (toUpper)
import Language.Haskell.TH.Syntax (Lift)

--------------------------------------------------
-- Code-point
--------------------------------------------------


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
-- Code-point range
--------------------------------------------------

-- | Code point range
newtype CodePointRange = Range Word64 deriving (Eq,Lift)

fromRange :: CodePointRange -> (CodePoint,CodePoint)
fromRange (Range w) = ( CodePoint $ fromIntegral (w .&. 0xFFFFFFFF)
                      , CodePoint $ fromIntegral (w `uncheckedShiftR` 32)
                      )

toRange :: (CodePoint,CodePoint) -> CodePointRange
toRange (CodePoint x, CodePoint y) =
   Range (fromIntegral x .|. (fromIntegral y `uncheckedShiftL` 32))

-- | Code-point range
{-# COMPLETE CodePointRange #-}
pattern CodePointRange :: CodePoint -> CodePoint -> CodePointRange
pattern CodePointRange x y <- (fromRange -> (x,y))
   where
      CodePointRange x y = toRange (x,y)

instance Show CodePointRange where
   show (CodePointRange x y) = show x ++ ".." ++ show y
