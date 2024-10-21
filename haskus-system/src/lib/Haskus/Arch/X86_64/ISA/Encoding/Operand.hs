{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Haskus.Arch.X86_64.ISA.Encoding.Operand
  ( Operand (..)
  , Operands (..)
  , pattern I8
  , pattern I16
  , pattern I32
  , pattern I64
  , pattern R8
  , pattern R16
  , pattern R32
  , pattern R64
  , pattern M8
  , pattern M16
  , pattern M32
  , pattern M64
  , pattern M128
  , pattern M256
  , pattern V64
  , pattern V128
  , pattern V256
  , pattern V512
  )
where

import GHC.IsList

import Haskus.Arch.X86_64.ISA.Size
import Haskus.Arch.X86_64.ISA.Encoding.Reg
import Haskus.Arch.X86_64.ISA.Encoding.Mem
import Haskus.Arch.X86_64.ISA.Encoding.Vec
import Haskus.Arch.X86_64.ISA.Encoding.Segment

data Operand
  = OpImm !SizedValue
  | OpReg !Reg
  | OpVec !Vec
  | OpMem !Mem
  | OpSeg !Segment
  deriving (Eq,Ord)

instance Show Operand where
  show = \case
    OpReg r -> show r
    OpVec v -> show v
    OpMem m -> show m
    OpSeg s -> show s
    OpImm i -> case i of
      SizedValue8  v -> "imm8(" ++ show v ++ ")"
      SizedValue16 v -> "imm16(" ++ show v ++ ")"
      SizedValue32 v -> "imm32(" ++ show v ++ ")"
      SizedValue64 v -> "imm64(" ++ show v ++ ")"


pattern I8   x = OpImm (SizedValue8  x)
pattern I16  x = OpImm (SizedValue16 x)
pattern I32  x = OpImm (SizedValue32 x)
pattern I64  x = OpImm (SizedValue64 x)
pattern R8   x <- OpReg (isReg8   -> Just x)
pattern R16  x <- OpReg (isReg16  -> Just x)
pattern R32  x <- OpReg (isReg32  -> Just x)
pattern R64  x <- OpReg (isReg64  -> Just x)
pattern M8   x <- OpMem (isMem8   -> Just x)
pattern M16  x <- OpMem (isMem16  -> Just x)
pattern M32  x <- OpMem (isMem32  -> Just x)
pattern M64  x <- OpMem (isMem64  -> Just x)
pattern M128 x <- OpMem (isMem128 -> Just x)
pattern M256 x <- OpMem (isMem256 -> Just x)
pattern V64  x <- OpVec (isVec64  -> Just x)
pattern V128 x <- OpVec (isVec128 -> Just x)
pattern V256 x <- OpVec (isVec256 -> Just x)
pattern V512 x <- OpVec (isVec512 -> Just x)

-- | Only a limited number of operands is supported. Hence we use this datatype
-- instead of a generic sequence like list.
data Operands
  = Ops0
  | Ops1 !Operand
  | Ops2 !Operand !Operand
  | Ops3 !Operand !Operand !Operand
  | Ops4 !Operand !Operand !Operand !Operand
  deriving (Eq,Ord)

instance Show Operands where
  show = \case
    Ops0 -> ""
    Ops1 a -> show a
    Ops2 a b -> show a ++","++ show b
    Ops3 a b c -> show a ++","++ show b++","++show c
    Ops4 a b c d -> show a ++","++ show b++","++show c++","++show d


instance IsList Operands where
  type Item Operands = Operand

  {-# INLINE fromListN #-}
  fromListN _ = fromList

  {-# INLINE fromList #-}
  fromList = \case
    []        -> Ops0
    [a]       -> Ops1 a
    [a,b]     -> Ops2 a b
    [a,b,c]   -> Ops3 a b c
    [a,b,c,d] -> Ops4 a b c d
    xs      -> error $ "Invalid list of operands (too long): " ++ show xs
  {-# INLINE toList #-}
  toList = \case
    Ops0         -> []
    Ops1 a       -> [a]
    Ops2 a b     -> [a,b]
    Ops3 a b c   -> [a,b,c]
    Ops4 a b c d -> [a,b,c,d]

