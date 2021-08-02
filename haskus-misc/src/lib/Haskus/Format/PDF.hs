{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Haskus.Format.PDF where

import ByteString.StrictBuilder
import qualified Data.ByteString as BS
import Data.Word
import Data.Char
import Data.Bits
import GHC.Natural
import Foreign.Storable
import qualified Data.List as List
import Numeric

type PDF = Builder

renderPDF :: FilePath -> PDF -> IO ()
renderPDF fp pdf = BS.writeFile fp (builderBytes pdf)

ascii :: String -> PDF
ascii s = mconcat (fmap asciiChar s)

hexa_word8 :: Word8 -> PDF
hexa_word8 x = word8 (asc (x `shiftR` 4)) <> word8 (asc (x .&. 0xF))
  where
    asc v
      | v <= 9    = 0x30 + v
      | otherwise = 0x41 + v - 10

encodeUtf8 :: Char -> [Word8]
encodeUtf8 (ord -> x)
  | x <= 0x007f = [ fromIntegral x]
  | x <= 0x07ff = [ fromIntegral (0xC0 .|. ((x `shiftR` 6) .&. 0x1F))
                  , fromIntegral (0x80 .|. (x .&. 0x3F))
                  ]
  | x <= 0xffff = [ fromIntegral (0xE0 .|. (x `shiftR` 12) .&. 0x0F)
                  , fromIntegral (0x80 .|. (x `shiftR` 6) .&. 0x3F)
                  , fromIntegral (0x80 .|. (x .&. 0x3F))
                  ]
  | otherwise   = [ fromIntegral (0xF0 .|. (x `shiftR` 18))
                  , fromIntegral (0x80 .|. ((x `shiftR` 12) .&. 0x3F))
                  , fromIntegral (0x80 .|. ((x `shiftR` 6) .&. 0x3F))
                  , fromIntegral (0x80 .|. (x .&. 0x3F))
                  ]


------------------------------------
-- Character set
------------------------------------

-- There are 3 character classes: regular, delimiter, white-space

-- | Is the character a white-space
--
-- 1) White-space are equivalent except in comments, strings, and streams.
-- 2) In all other contexts, a sequence of consecutive white-space characters is
-- considered as one character.
isWhiteSpace :: Word8 -> Bool
isWhiteSpace = \case
  0x00 -> True -- null (NUL)
  0x09 -> True -- horizontal tab (HT)
  0x0A -> True -- line feed (LF)
  0x0C -> True -- form feed (FF)
  0x0D -> True -- carriage return (CR)
  0x20 -> True -- space (SP)
  _    -> False

space :: PDF
space = word8 0x20

-- | End-of-line can be CR, LF or CR-LF.
eol :: PDF
eol = word8 0x0A -- LF

isDelimiter :: Word8 -> Bool
isDelimiter = \case
  0x28 -> True -- left parenthesis
  0x29 -> True -- right parenthesis
  0x3C -> True -- less-than sign
  0x3E -> True -- greater-than sign
  0x5B -> True -- left square bracket
  0x5D -> True -- right square bracket
  0x7B -> True -- left curly brace
  0x7D -> True -- right curly brace
  0x2F -> True -- solidus ("slash")
  0x25 -> True -- percent sign
  _    -> False

leftParen, rightParen, leftAngle, rightAngle,
  leftSquare, rightSquare, leftCurly, rightCurly,
  solidus, percent :: PDF
leftParen   = word8 0x28
rightParen  = word8 0x29
leftAngle   = word8 0x3C
rightAngle  = word8 0x3E
leftSquare  = word8 0x5B
rightSquare = word8 0x5D
leftCurly   = word8 0x7B
rightCurly  = word8 0x7D
solidus     = word8 0x2F
percent     = word8 0x25

isRegular :: Word8 -> Bool
isRegular = \case
  -- white-spaces
  0x00 -> False -- null (NUL)
  0x09 -> False -- horizontal tab (HT)
  0x0A -> False -- line feed (LF)
  0x0C -> False -- form feed (FF)
  0x0D -> False -- carriage return (CR)
  0x20 -> False -- space (SP)
  -- delimiters
  0x28 -> False -- left parenthesis
  0x29 -> False -- right parenthesis
  0x3C -> False -- less-than sign
  0x3E -> False -- greater-than sign
  0x5B -> False -- left square bracket
  0x5D -> False -- right square bracket
  0x7B -> False -- left curly brace
  0x7D -> False -- right curly brace
  0x2F -> False -- solidus ("slash")
  0x25 -> False -- percent sign
  _    -> True

------------------------------------
-- Comment
------------------------------------

-- | Insert a comment into a PDF.
--
-- Note that characters are truncated to ASCII
comment :: String -> PDF
comment s = case filter is_invalid s of
  [] -> percent <> mconcat (fmap asciiChar s) <> eol
  xs -> error $ "Invalid character in comment: " ++ show (fmap ord xs)
  where
    is_invalid c = case fromIntegral (ord c) of
      0x20 -> False
      0x09 -> False
      x | isDelimiter x -> False
      x | isRegular x   -> False
      _ -> True

------------------------------------
-- Objects
------------------------------------

-- Booleans: true and false

bTrue :: PDF
bTrue = ascii "true"

bFalse :: PDF
bFalse = ascii "false"

-- Numeric: integer and real

int :: Integral a => a -> PDF
int a = mconcat (fmap asciiChar (show (toInteger a)))

float :: RealFloat a => a -> PDF
float a = mconcat (fmap asciiChar (showFFloat Nothing a ""))

-- Strings: literal or hexadecimal

litString :: [Word8] -> PDF
litString s = leftParen <> mconcat (fmap go s) <> rightParen
  where
    -- any character except (unbalanced) parentheses and reverse solidus.
    -- We escape every parentheses to avoid checking balancing.
    go = \case
      0x5C -> word16BE 0x5C5C -- reverse solidus:  \\
      0x28 -> word16BE 0x5C28 -- left parenthese:  \(
      0x29 -> word16BE 0x5C29 -- right parenthese: \)
      x    -> word8 x

litStringUtf8 :: String -> PDF
litStringUtf8 s = litString (concatMap encodeUtf8 s)

hexaString :: [Word8] -> PDF
hexaString s = leftAngle <> mconcat (fmap hexa_word8 s) <> rightAngle

-- Name

name :: [Word8] -> PDF
name s = solidus <> mconcat (fmap go s)
  where
    go = \case
      0x00 -> error "NULL character not valid in a PDF object name"
      0x23 -> ascii "#23"
      x | isRegular x -> word8 x
      x    -> word8 0x23 <> hexa_word8 x

nameUtf8 :: String -> PDF
nameUtf8 s = name (concatMap encodeUtf8 s)

-- Array

array :: [PDF] -> PDF
array xs = leftSquare <> mconcat (List.intersperse space xs) <> rightSquare

-- Dictionary

dict :: [(PDF,PDF)] -> PDF
dict xs = leftAngle <> leftAngle <> mconcat (List.intersperse space (fmap go xs)) <> rightAngle <> rightAngle
  where
    go (key,val) = key <> space <> val


-- Stream

stream :: PDF -> [Word8] -> PDF
stream d xs = d <> ascii "stream" <> eol <> mconcat (fmap word8 xs) <> eol <> ascii "endstream"

streamStorable :: Storable a => PDF -> a -> PDF
streamStorable d xs = d <> ascii "stream" <> eol <> storable xs <> eol <> ascii "endstream"

-- Null

null :: PDF
null = ascii "null"

-- Indirect object

obj :: Natural -> Natural -> PDF -> PDF
obj i gen contents = int i <> space <> int gen <> space
                           <> ascii "obj" <> space <> contents
                           <> ascii "endobj"

ref :: Natural -> Natural -> PDF
ref i gen = int i <> space <> int gen <> space <> ascii "R"

------------------------------------
-- Example
------------------------------------

toAscii :: String -> [Word8]
toAscii = fmap (fromIntegral . ord)

example :: IO ()
example = do
  let hdr = ascii "%PDF-1.7\n"
  renderPDF "test.pdf" $ mconcat
    [ hdr
    , litString (toAscii "this is a test")
    , hexaString (toAscii "this is a test")
    , name (toAscii "il était une fois 133###") <> eol
    , nameUtf8 "il était une fois 133###"
    , array
        [ nameUtf8 "il était une fois 133###"
        , int (18 :: Int)
        , float (1.2 :: Double)
        ]
    , dict [ (nameUtf8 "Type", nameUtf8 "Example")
           , (nameUtf8 "SubType", nameUtf8 "DictionaryExample")
           , (nameUtf8 "Version", float (0.01 :: Float))
           , (nameUtf8 "IntegerItem", int (12 :: Int))
           , (nameUtf8 "StringItem", litStringUtf8 "a string")
           , (nameUtf8 "Subdictionary", dict
                [ (nameUtf8 "Item1", float (0.4 :: Double))
                , (nameUtf8 "Item2", bTrue)
                ])
           ]
    ]
