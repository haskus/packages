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
import Haskus.Binary.Int
import Haskus.Binary.Cast

-- | Displacement
data Disp
  = NoDisp
  | Disp8  !I8
  | Disp16 !I16
  | Disp32 !I32
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
  JR_BX   JR_SI   NoDisp      -> r 0b00 0b000 NoDisp
  JR_SI   JR_BX   NoDisp      -> r 0b00 0b000 NoDisp
  JR_BX   JR_DI   NoDisp      -> r 0b00 0b001 NoDisp
  JR_DI   JR_BX   NoDisp      -> r 0b00 0b001 NoDisp
  JR_BP   JR_SI   NoDisp      -> r 0b00 0b010 NoDisp
  JR_SI   JR_BP   NoDisp      -> r 0b00 0b010 NoDisp
  JR_BP   JR_DI   NoDisp      -> r 0b00 0b011 NoDisp
  JR_DI   JR_BP   NoDisp      -> r 0b00 0b011 NoDisp
  JR_SI   Nothing NoDisp      -> r 0b00 0b100 NoDisp
  Nothing JR_SI   NoDisp      -> r 0b00 0b100 NoDisp
  JR_DI   Nothing NoDisp      -> r 0b00 0b101 NoDisp
  Nothing JR_DI   NoDisp      -> r 0b00 0b101 NoDisp
  Nothing Nothing (Disp16 d)  -> r 0b00 0b110 (Disp16 d)
  JR_BX   Nothing NoDisp      -> r 0b00 0b111 NoDisp
  Nothing JR_BX   NoDisp      -> r 0b00 0b111 NoDisp

  JR_BX   JR_SI   (Disp8 d)   -> r 0b01 0b000 (Disp8 d)
  JR_SI   JR_BX   (Disp8 d)   -> r 0b01 0b000 (Disp8 d)
  JR_BX   JR_DI   (Disp8 d)   -> r 0b01 0b001 (Disp8 d)
  JR_DI   JR_BX   (Disp8 d)   -> r 0b01 0b001 (Disp8 d)
  JR_BP   JR_SI   (Disp8 d)   -> r 0b01 0b010 (Disp8 d)
  JR_SI   JR_BP   (Disp8 d)   -> r 0b01 0b010 (Disp8 d)
  JR_BP   JR_DI   (Disp8 d)   -> r 0b01 0b011 (Disp8 d)
  JR_DI   JR_BP   (Disp8 d)   -> r 0b01 0b011 (Disp8 d)
  JR_SI   Nothing (Disp8 d)   -> r 0b01 0b100 (Disp8 d)
  Nothing JR_SI   (Disp8 d)   -> r 0b01 0b100 (Disp8 d)
  JR_DI   Nothing (Disp8 d)   -> r 0b01 0b101 (Disp8 d)
  Nothing JR_DI   (Disp8 d)   -> r 0b01 0b101 (Disp8 d)
  JR_BP   Nothing (Disp8 d)   -> r 0b01 0b110 (Disp8 d)
  Nothing JR_BP   (Disp8 d)   -> r 0b01 0b110 (Disp8 d)
  JR_BX   Nothing (Disp8 d)   -> r 0b01 0b111 (Disp8 d)
  Nothing JR_BX   (Disp8 d)   -> r 0b01 0b111 (Disp8 d)

  JR_BX   JR_SI   (Disp16 d)  -> r 0b10 0b000 (Disp16 d)
  JR_SI   JR_BX   (Disp16 d)  -> r 0b10 0b000 (Disp16 d)
  JR_BX   JR_DI   (Disp16 d)  -> r 0b10 0b001 (Disp16 d)
  JR_DI   JR_BX   (Disp16 d)  -> r 0b10 0b001 (Disp16 d)
  JR_BP   JR_SI   (Disp16 d)  -> r 0b10 0b010 (Disp16 d)
  JR_SI   JR_BP   (Disp16 d)  -> r 0b10 0b010 (Disp16 d)
  JR_BP   JR_DI   (Disp16 d)  -> r 0b10 0b011 (Disp16 d)
  JR_DI   JR_BP   (Disp16 d)  -> r 0b10 0b011 (Disp16 d)
  JR_SI   Nothing (Disp16 d)  -> r 0b10 0b100 (Disp16 d)
  Nothing JR_SI   (Disp16 d)  -> r 0b10 0b100 (Disp16 d)
  JR_DI   Nothing (Disp16 d)  -> r 0b10 0b101 (Disp16 d)
  Nothing JR_DI   (Disp16 d)  -> r 0b10 0b101 (Disp16 d)
  JR_BP   Nothing (Disp16 d)  -> r 0b10 0b110 (Disp16 d)
  Nothing JR_BP   (Disp16 d)  -> r 0b10 0b110 (Disp16 d)
  JR_BX   Nothing (Disp16 d)  -> r 0b10 0b111 (Disp16 d)
  Nothing JR_BX   (Disp16 d)  -> r 0b10 0b111 (Disp16 d)
  
  -- bonus forms that can be encoded with other forms
  Nothing Nothing (Disp8 d)   -> r 0b00 0b100 (Disp16 (i16FromI8 d))
  _       _       _           -> Nothing
  where
    r a b c = Just (a,b,c)
