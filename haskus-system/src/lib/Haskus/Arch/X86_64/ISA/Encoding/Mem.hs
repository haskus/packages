module Haskus.Arch.X86_64.ISA.Encoding.Mem
  ( Disp (..)
  , Scale (..)
  , Mem (..)
  , encodeMem16
  )
where

import Haskus.Arch.X86_64.ISA.Encoding.Reg
import Haskus.Arch.X86_64.ISA.Encoding.SIB
import Haskus.Binary.Word

-- | Displacement
data Disp
  = NoDisp
  | Disp8  !U8
  | Disp16 !U16
  | Disp32 !U32
  deriving (Show,Eq,Ord)

-- | Memory addressing
data Mem
  = M_16        !(Maybe Reg) !(Maybe Reg) !Disp -- ^ 16-bit index-base-disp
  | M_32 !Scale !(Maybe Reg) !(Maybe Reg) !Disp -- ^ 32-bit scaled-index-base-disp
  | M_64 !Scale !(Maybe Reg) !(Maybe Reg) !Disp -- ^ 64-bit scaled-index-base-disp
  | M_Rel !Disp                                 -- ^ RIP-relative displacement
  deriving (Show,Eq,Ord)


-- | Return (mod,rm,disp) for the given 16-bit addressing form base-index-disp
encodeMem16 :: Maybe Reg -> Maybe Reg -> Disp -> Maybe (U8,U8,Disp)
encodeMem16 = \cases
  JR_BX JR_SI NoDisp -> r 0b00 0b000 NoDisp
  JR_SI JR_BX NoDisp -> r 0b00 0b000 NoDisp
  JR_BX JR_DI NoDisp -> r 0b00 0b001 NoDisp
  JR_DI JR_BX NoDisp -> r 0b00 0b001 NoDisp
  -- TODO: continue
  _     _     _      -> Nothing
  where
    r a b c = Just (a,b,c)
