-- | REX prefix
module Haskus.Arch.X86_64.ISA.Encoding.Rex
  ( Rex (..)
  , rexW
  , rexR
  , rexX
  , rexB
  , isRexPrefix
  , rexU8
  )
where

import Haskus.Number.Word
import Haskus.Binary.Bits

-- | Rex prefix
newtype Rex = Rex Word8 deriving (Show,Eq,Ord)

-- | Test W bit of REX prefix
rexW :: Rex -> Bool
rexW (Rex v) = testBit v 3

-- | Test R bit of REX prefix
rexR :: Rex -> Word8
rexR (Rex v) = if testBit v 2 then 1 else 0

-- | Test X bit of REX prefix
rexX :: Rex -> Word8
rexX (Rex v) = if testBit v 1 then 1 else 0

-- | Test B bit of REX prefix
rexB :: Rex -> Word8
rexB (Rex v) = if testBit v 0 then 1 else 0

-- | Test for a REX prefix
isRexPrefix :: Word8 -> Bool
isRexPrefix w = w .&. 0xF0 == 0x40

-- | Get Rex byte
rexU8 :: Rex -> Word8
rexU8 (Rex v) = v
