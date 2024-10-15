module Haskus.Arch.X86_64.ISA.Encoding.Vex
  ( Vex(..)
  , vexSize
  , writeVex
  )
where

import Haskus.Binary.Word
import qualified Haskus.Memory.Writer as W

data Vex
  = Vex2 !U8
  | Vex3 !U8 !U8
  deriving (Show,Eq,Ord)

-- | VEX prefix size in bytes
vexSize :: Vex -> U
vexSize = \case
  Vex2 {} -> 2
  Vex3 {} -> 3

-- | Write a VEX prefix
writeVex :: Vex -> W.Writer s
writeVex = \case
  Vex2 v1    -> W.writeU8 0xC5 <> W.writeU8 v1
  Vex3 v1 v2 -> W.writeU8 0xC4 <> W.writeU8 v1 <> W.writeU8 v2
