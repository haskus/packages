module Haskus.Arch.X86_64.ISA.Encoding.Xop
  ( Xop(..)
  , writeXop
  )
where

import Haskus.Binary.Word
import qualified Haskus.Memory.Writer as W

data Xop
  = Xop !U8 !U8
  deriving (Show,Eq,Ord)

-- | Write a XOP prefix
writeXop :: Xop -> W.Writer s
writeXop (Xop v1 v2) = W.writeU8 0x8F <> W.writeU8 v1 <> W.writeU8 v2
