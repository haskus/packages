{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BinaryLiterals #-}

-- | Unicode code-point
module Haskus.Format.Text.Unicode.CodePoint
   ( CodePoint (..)
   , CodePointRange
   , pattern CodePointRange
   , toUtf8
   , fromUtf8
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Bits
import Haskus.Utils.Flow
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

-- | Encode a code-point into UTF8.
-- Extended to support any Word32 (not just 21 bits) to make the function total
-- and useful in other contexts
--
-- >>> :set -XBinaryLiterals
-- >>> import Control.Monad.Trans.State
-- >>> toNext x = modify (x:)
-- >>> let f x = reverse (execState (toUtf8 toNext x ) [])
-- >>> f 0x24    == [0b00100100]
-- True
-- >>> f 0xA2    == [0b11000010,0b10100010]
-- True
-- >>> f 0x939   == [0b11100000,0b10100100,0b10111001]
-- True
-- >>> f 0x20AC  == [0b11100010,0b10000010,0b10101100]
-- True
-- >>> f 0x10348 == [0b11110000,0b10010000,0b10001101,0b10001000]
-- True
toUtf8 :: Monad m => (Word8 -> m ()) -> Word32 -> m ()
toUtf8 putW8 w
   | w .&. 0xFFFFFF80 == 0 = putW8 (fromIntegral w)
   | w .&. 0xFFFFF800 == 0 = do
      putW8 <| fromIntegral <| (0b11000000 .|. (w `shiftR` 6))
      putW8 <| fromIntegral <| (0b10000000 .|. (w .&. 0b00111111))
   | w .&. 0xFFFF0000 == 0 = do
      putW8 <| fromIntegral <| (0b11100000 .|. (w `shiftR` 12))
      putW8 <| fromIntegral <| (0b10000000 .|. ((w `shiftR` 6) .&. 0b00111111))
      putW8 <| fromIntegral <| (0b10000000 .|. (w .&. 0b00111111))
   | w .&. 0xFFE00000 == 0 = do
      putW8 <| fromIntegral <| (0b11110000 .|. (w `shiftR` 18))
      putW8 <| fromIntegral <| (0b10000000 .|. ((w `shiftR` 12) .&. 0b00111111))
      putW8 <| fromIntegral <| (0b10000000 .|. ((w `shiftR` 6 ) .&. 0b00111111))
      putW8 <| fromIntegral <| (0b10000000 .|. (w .&. 0b00111111))
   | w .&. 0xFC000000 == 0 = do
      putW8 <| fromIntegral <| (0b11111000 .|. (w `shiftR` 24))
      putW8 <| fromIntegral <| (0b10000000 .|. ((w `shiftR` 18) .&. 0b00111111))
      putW8 <| fromIntegral <| (0b10000000 .|. ((w `shiftR` 12) .&. 0b00111111))
      putW8 <| fromIntegral <| (0b10000000 .|. ((w `shiftR` 6 ) .&. 0b00111111))
      putW8 <| fromIntegral <| (0b10000000 .|. (w .&. 0b00111111))
   | otherwise = do
      putW8 <| fromIntegral <| (0b11111100 .|. (w `shiftR` 30))
      putW8 <| fromIntegral <| (0b10000000 .|. ((w `shiftR` 24) .&. 0b00111111))
      putW8 <| fromIntegral <| (0b10000000 .|. ((w `shiftR` 18) .&. 0b00111111))
      putW8 <| fromIntegral <| (0b10000000 .|. ((w `shiftR` 12) .&. 0b00111111))
      putW8 <| fromIntegral <| (0b10000000 .|. ((w `shiftR` 6 ) .&. 0b00111111))
      putW8 <| fromIntegral <| (0b10000000 .|. (w .&. 0b00111111))

-- | Decode a code-point in UTF8.
--
-- Extended to support any Word32 (not just 21 bits).
--
-- We don't check that following bytes are valid.
--
-- >>> :set -XBinaryLiterals
-- >>> import Control.Monad.Trans.State
-- >>> getNext = do { ~(x:xs) <- get; put xs; pure x }
-- >>> let x = evalState (fromUtf8 getNext) [0b11110000,0b10010000,0b10001101,0b10001000]
-- >>> x == Just 0x10348
-- True
fromUtf8 :: Monad m => m Word8 -> m (Maybe Word32)
fromUtf8 getW8 = do
   w <- getW8
   let
      n  = countLeadingZeros (complement w)
      b1 = fromIntegral w
   case n of
      1 -> pure Nothing -- not a first byte
      0 -> pure (Just b1)
      2 -> do
        b2 <- fromIntegral <|| getW8
        pure <| Just <| ((b1 .&. 0b00011111) `shiftL` 6)
                        .|. (b2 .&. 0b00111111)

      3 -> do
        b2 <- fromIntegral <|| getW8
        b3 <- fromIntegral <|| getW8
        pure <| Just <| ((b1 .&. 0b00001111) `shiftL` 12)
                        .|. ((b2 .&. 0b00111111) `shiftL` 6)
                        .|. (b3 .&. 0b00111111)

      4 -> do
        b2 <- fromIntegral <|| getW8
        b3 <- fromIntegral <|| getW8
        b4 <- fromIntegral <|| getW8
        pure <| Just <| ((b1 .&. 0b00000111) `shiftL` 18)
                        .|. ((b2 .&. 0b00111111) `shiftL` 12)
                        .|. ((b3 .&. 0b00111111) `shiftL` 6)
                        .|. (b4 .&. 0b00111111)

      5 -> do
        b2 <- fromIntegral <|| getW8
        b3 <- fromIntegral <|| getW8
        b4 <- fromIntegral <|| getW8
        b5 <- fromIntegral <|| getW8
        pure <| Just <| ((b1 .&. 0b00000111) `shiftL` 24)
                        .|. ((b2 .&. 0b00111111) `shiftL` 18)
                        .|. ((b3 .&. 0b00111111) `shiftL` 12)
                        .|. ((b4 .&. 0b00111111) `shiftL` 6)
                        .|. (b5 .&. 0b00111111)

      _ -> do
        b2 <- fromIntegral <|| getW8
        b3 <- fromIntegral <|| getW8
        b4 <- fromIntegral <|| getW8
        b5 <- fromIntegral <|| getW8
        b6 <- fromIntegral <|| getW8
        pure <| Just <| ((b1 .&. 0b00000011) `shiftL` 30)
                        .|. ((b2 .&. 0b00111111) `shiftL` 24)
                        .|. ((b3 .&. 0b00111111) `shiftL` 18)
                        .|. ((b4 .&. 0b00111111) `shiftL` 12)
                        .|. ((b5 .&. 0b00111111) `shiftL` 6)
                        .|. (b6 .&. 0b00111111)
