-- | REX prefix
module Haskus.Arch.X86_64.ISA.Encoding.Rex
  ( Rex (..)
  , testRexW
  , testRexR
  , testRexX
  , testRexB
  , setRexW
  , setRexR
  , setRexX
  , setRexB
  , unsetRexW
  , unsetRexR
  , unsetRexX
  , unsetRexB
  , emptyRex
  , isRexPrefix
  , rexU8
  , rexW
  , rexR
  , rexX
  , rexB
  , rexWB
  )
where

import Haskus.Binary.Word
import Haskus.Binary.Bits

-- | Rex prefix
newtype Rex
  = Rex U8
  deriving (Show,Eq,Ord)

instance Semigroup Rex where
  Rex a <> Rex b = Rex (a .|. b)

-- | Test W bit of REX prefix
testRexW :: Rex -> Bool
testRexW (Rex v) = testBit v 3

-- | Test R bit of REX prefix
testRexR :: Rex -> Bool
testRexR (Rex v) = testBit v 2

-- | Test X bit of REX prefix
testRexX :: Rex -> Bool
testRexX (Rex v) = testBit v 1

-- | Test B bit of REX prefix
testRexB :: Rex -> Bool
testRexB (Rex v) = testBit v 0

-- | Test for a REX prefix
isRexPrefix :: U8 -> Bool
isRexPrefix w = w .&. 0xF0 == 0x40

-- | Get Rex byte
rexU8 :: Rex -> U8
rexU8 (Rex v) = v

emptyRex :: Rex
emptyRex = Rex 0b0100_0000

setRexW :: Rex -> Rex
setRexW (Rex w) = Rex (w .|. 0b1000)

setRexR :: Rex -> Rex
setRexR (Rex w) = Rex (w .|. 0b0100)

setRexX :: Rex -> Rex
setRexX (Rex w) = Rex (w .|. 0b0010)

setRexB :: Rex -> Rex
setRexB (Rex w) = Rex (w .|. 0b0010)

unsetRexW :: Rex -> Rex
unsetRexW (Rex w) = Rex (w .&. 0b1111_0111)

unsetRexR :: Rex -> Rex
unsetRexR (Rex w) = Rex (w .&. 0b1111_1011)

unsetRexX :: Rex -> Rex
unsetRexX (Rex w) = Rex (w .&. 0b1111_1101)

unsetRexB :: Rex -> Rex
unsetRexB (Rex w) = Rex (w .&. 0b1111_1110)

rexW :: Rex
rexW = setRexW emptyRex

rexB :: Rex
rexB = setRexB emptyRex

rexR :: Rex
rexR = setRexR emptyRex

rexX :: Rex
rexX = setRexX emptyRex

rexWB :: Rex
rexWB = setRexB rexW
