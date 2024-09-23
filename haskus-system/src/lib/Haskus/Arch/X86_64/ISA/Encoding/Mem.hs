module Haskus.Arch.X86_64.ISA.Encoding.Mem
  ( Disp (..)
  , Scale (..)
  , Mem (..)
  , encodeMem16
  , encodeMem32
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

-- | Return (mod,rm,disp) for the given 32-bit addressing form scale-index-base-disp
encodeMem32 :: Scale -> Maybe Reg -> Maybe Reg -> Disp -> Maybe (U8,U8,Maybe SIB,Disp)
encodeMem32 = \cases
  -- single unscaled register, no displacement
  Scale1  JR_EAX  Nothing NoDisp      -> r 0b00 0b000 Nothing NoDisp
  _       Nothing JR_EAX  NoDisp      -> r 0b00 0b000 Nothing NoDisp
  Scale1  JR_ECX  Nothing NoDisp      -> r 0b00 0b001 Nothing NoDisp
  _       Nothing JR_ECX  NoDisp      -> r 0b00 0b001 Nothing NoDisp
  Scale1  JR_EDX  Nothing NoDisp      -> r 0b00 0b010 Nothing NoDisp
  _       Nothing JR_EDX  NoDisp      -> r 0b00 0b010 Nothing NoDisp
  Scale1  JR_EBX  Nothing NoDisp      -> r 0b00 0b011 Nothing NoDisp
  _       Nothing JR_EBX  NoDisp      -> r 0b00 0b011 Nothing NoDisp
  Scale1  JR_ESI  Nothing NoDisp      -> r 0b00 0b110 Nothing NoDisp
  _       Nothing JR_ESI  NoDisp      -> r 0b00 0b110 Nothing NoDisp
  Scale1  JR_EDI  Nothing NoDisp      -> r 0b00 0b111 Nothing NoDisp
  _       Nothing JR_EDI  NoDisp      -> r 0b00 0b111 Nothing NoDisp
  Scale1  JR_EBP  Nothing NoDisp      -> r 0b01 0b101 Nothing (Disp8 0)
  _       Nothing JR_EBP  NoDisp      -> r 0b01 0b101 Nothing (Disp8 0)
  Scale1  JR_ESP  Nothing NoDisp      -> r 0b00 0b100 (s Scale1 0b100 0b100) NoDisp
  _       Nothing JR_ESP  NoDisp      -> r 0b00 0b100 (s Scale1 0b100 0b100) NoDisp

  -- single unscaled register, disp8
  Scale1  JR_EAX  Nothing (Disp8 d)   -> r 0b01 0b000 Nothing (Disp8 d)
  _       Nothing JR_EAX  (Disp8 d)   -> r 0b01 0b000 Nothing (Disp8 d)
  Scale1  JR_ECX  Nothing (Disp8 d)   -> r 0b01 0b001 Nothing (Disp8 d)
  _       Nothing JR_ECX  (Disp8 d)   -> r 0b01 0b001 Nothing (Disp8 d)
  Scale1  JR_EDX  Nothing (Disp8 d)   -> r 0b01 0b010 Nothing (Disp8 d)
  _       Nothing JR_EDX  (Disp8 d)   -> r 0b01 0b010 Nothing (Disp8 d)
  Scale1  JR_EBX  Nothing (Disp8 d)   -> r 0b01 0b011 Nothing (Disp8 d)
  _       Nothing JR_EBX  (Disp8 d)   -> r 0b01 0b011 Nothing (Disp8 d)
  Scale1  JR_EBP  Nothing (Disp8 d)   -> r 0b01 0b101 Nothing (Disp8 d)
  _       Nothing JR_EBP  (Disp8 d)   -> r 0b01 0b101 Nothing (Disp8 d)
  Scale1  JR_ESI  Nothing (Disp8 d)   -> r 0b01 0b110 Nothing (Disp8 d)
  _       Nothing JR_ESI  (Disp8 d)   -> r 0b01 0b110 Nothing (Disp8 d)
  Scale1  JR_EDI  Nothing (Disp8 d)   -> r 0b01 0b111 Nothing (Disp8 d)
  _       Nothing JR_EDI  (Disp8 d)   -> r 0b01 0b111 Nothing (Disp8 d)
  Scale1  JR_ESP  Nothing (Disp8 d)   -> r 0b01 0b100 (s Scale1 0b100 0b100) (Disp8 d)
  _       Nothing JR_ESP  (Disp8 d)   -> r 0b01 0b100 (s Scale1 0b100 0b100) (Disp8 d)

  -- single unscaled register, disp32
  Scale1  JR_EAX  Nothing (Disp32 d)  -> r 0b10 0b000 Nothing (Disp32 d)
  _       Nothing JR_EAX  (Disp32 d)  -> r 0b10 0b000 Nothing (Disp32 d)
  Scale1  JR_ECX  Nothing (Disp32 d)  -> r 0b10 0b001 Nothing (Disp32 d)
  _       Nothing JR_ECX  (Disp32 d)  -> r 0b10 0b001 Nothing (Disp32 d)
  Scale1  JR_EDX  Nothing (Disp32 d)  -> r 0b10 0b010 Nothing (Disp32 d)
  _       Nothing JR_EDX  (Disp32 d)  -> r 0b10 0b010 Nothing (Disp32 d)
  Scale1  JR_EBX  Nothing (Disp32 d)  -> r 0b10 0b011 Nothing (Disp32 d)
  _       Nothing JR_EBX  (Disp32 d)  -> r 0b10 0b011 Nothing (Disp32 d)
  Scale1  JR_EBP  Nothing (Disp32 d)  -> r 0b10 0b101 Nothing (Disp32 d)
  _       Nothing JR_EBP  (Disp32 d)  -> r 0b10 0b101 Nothing (Disp32 d)
  Scale1  JR_ESI  Nothing (Disp32 d)  -> r 0b10 0b110 Nothing (Disp32 d)
  _       Nothing JR_ESI  (Disp32 d)  -> r 0b10 0b110 Nothing (Disp32 d)
  Scale1  JR_EDI  Nothing (Disp32 d)  -> r 0b10 0b111 Nothing (Disp32 d)
  _       Nothing JR_EDI  (Disp32 d)  -> r 0b10 0b111 Nothing (Disp32 d)
  Scale1  JR_ESP  Nothing (Disp32 d)  -> r 0b10 0b100 (s Scale1 0b100 0b100) (Disp32 d)
  _       Nothing JR_ESP  (Disp32 d)  -> r 0b10 0b100 (s Scale1 0b100 0b100) (Disp32 d)

  -- disp alone
  _       Nothing Nothing (Disp32 d)  -> r 0b00 0b101 Nothing (Disp32 d)
  _       Nothing Nothing (Disp8 d)   -> r 0b00 0b101 Nothing (Disp32 (i32FromI8 d))

  -- scaled index, no base, disp32
  sc (scaled_index -> Just i) Nothing (Disp32 d) -> r 0b00 0b100 (s sc i 0b101) (Disp32 d)

  -- scaled index, base, disp
  sc (scaled_index -> Just i) (base -> Just b) (Disp8 d)  -> r 0b01 0b100 (s sc i b) (Disp8 d)
  sc (scaled_index -> Just i) (base -> Just b) (Disp32 d) -> r 0b10 0b100 (s sc i b) (Disp32 d)

  -- scaled index without base:
  --  - if scale=2, try 1*r+r
  --  - otherwise use Disp32=0
  Scale2  i       Nothing d           -> encodeMem32 Scale1 i i       d
  sc      i       Nothing NoDisp      -> encodeMem32 sc     i Nothing (Disp32 0)

  -- any disp16 can be handled as a disp32
  sc      i       b       (Disp16 d)  -> encodeMem32 sc i b (Disp32 (i32FromI16 d))
  _       _       _       _           -> Nothing
  where
    r a b c d = Just (a,b,c,d)
    s a b c   = Just (mkSIB a b c)
    base = \case
      JR_EAX  -> Just 0b000
      JR_ECX  -> Just 0b001
      JR_EDX  -> Just 0b010
      JR_EBX  -> Just 0b011
      JR_ESP  -> Just 0b100
      JR_EBP  -> Just 0b101
      JR_ESI  -> Just 0b110
      JR_EDI  -> Just 0b111
      _       -> Nothing
    scaled_index = \case
      JR_EAX  -> Just 0b000
      JR_ECX  -> Just 0b001
      JR_EDX  -> Just 0b010
      JR_EBX  -> Just 0b011
      Nothing -> Just 0b100
      JR_EBP  -> Just 0b101
      JR_ESI  -> Just 0b110
      JR_EDI  -> Just 0b111
      _       -> Nothing
