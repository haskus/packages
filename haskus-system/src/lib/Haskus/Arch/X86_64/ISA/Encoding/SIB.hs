-- | SIB byte
module Haskus.Arch.X86_64.ISA.Encoding.SIB
  ( SIB
  , sibU8
  , mkSIB
  , writeSIB
  , Scale(..)
  , scaleU8
  )
where

import Haskus.Binary.Word
import Haskus.Binary.Bits
import qualified Haskus.Memory.Writer as W

-- | SIB byte: ss_iii_bbb
newtype SIB
  = SIB U8
  deriving (Show,Eq,Ord)

data Scale
  = Scale1
  | Scale2
  | Scale4
  | Scale8
  deriving (Show,Eq,Ord)

scaleU8 :: Scale -> U8
scaleU8 = \case
  Scale1 -> 0b00
  Scale2 -> 0b01
  Scale4 -> 0b10
  Scale8 -> 0b11

sibU8 :: SIB -> U8
sibU8 (SIB w) = w

mkSIB :: Scale -> U8 -> U8 -> SIB
mkSIB s i b = SIB ((scaleU8 s `shiftL` 6) .|. (i `shiftL` 3) .|. b)

-- | Write a SIB byte
writeSIB :: SIB -> W.Writer s
writeSIB (SIB u) = W.writeU8 u
