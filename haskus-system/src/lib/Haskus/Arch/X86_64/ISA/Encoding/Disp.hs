module Haskus.Arch.X86_64.ISA.Encoding.Disp
  ( Disp(..)
  , dispSizeInBytes
  , writeDisp
  , hasDisp
  , showDisp
  , showDispMaybe
  )
where

import Haskus.Binary.Word
import Haskus.Binary.Int
import qualified Haskus.Memory.Writer as W

-- | Displacement
data Disp
  = NoDisp
  | Disp8  !I8
  | Disp16 !I16
  | Disp32 !I32
  deriving (Show,Eq,Ord)

showDisp :: Disp -> String
showDisp = \case
  NoDisp  -> ""
  Disp8  i -> "imm8(" ++ show i ++ ")"
  Disp16 i -> "imm16(" ++ show i ++ ")"
  Disp32 i -> "imm32(" ++ show i ++ ")"

showDispMaybe :: Disp -> Maybe String
showDispMaybe = \case
  NoDisp -> Nothing
  o      -> Just (showDisp o)

hasDisp :: Disp -> Bool
hasDisp d = d /= NoDisp

dispSizeInBytes :: Disp -> U
dispSizeInBytes = \case
  NoDisp    -> 0
  Disp8  {} -> 1
  Disp16 {} -> 2
  Disp32 {} -> 4

-- | Write a Disp (little-endian)
writeDisp :: Disp -> W.Writer s
writeDisp = \case
   NoDisp   -> mempty
   Disp8  v -> W.writeI8 v
   Disp16 v -> W.writeI16LE v
   Disp32 v -> W.writeI32LE v
