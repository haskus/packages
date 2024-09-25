module Haskus.Arch.X86_64.ISA.Encoding.Mem
  ( encodeMem16
  , encodeMem32
  , encodeMem64
  , encodeMemRel
  )
where

import Prelude hiding (mod)
import Haskus.Arch.X86_64.ISA.Encoding.Disp
import Haskus.Arch.X86_64.ISA.Encoding.Reg
import Haskus.Arch.X86_64.ISA.Encoding.SIB
import Haskus.Arch.X86_64.ISA.Encoding.Rex
import Haskus.Arch.X86_64.ISA.Encoding.ModRM
import Haskus.Arch.X86_64.ISA.Size
import Haskus.Binary.Cast

-- | Return (modrm,disp) for the given 16-bit addressing form base-index-disp.
--
-- modrm.reg is set to 0
--
encodeMem16 :: Maybe Reg -> Maybe Reg -> Disp -> Maybe (ModRM,Disp)
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
    r a b c = Just (mkModRM_mod_rm a b,c)



-- | Return (modrm,disp,sib) for the given 32-bit addressing form scale-index-base-disp
encodeMem32 :: Scale -> Maybe Reg -> Maybe Reg -> Disp -> Maybe (ModRM,Disp,Maybe SIB)
encodeMem32 = \cases
  -- only support 32-bit registers that don't require REX
  _  (Just r) _ _ | regSize r /= OpSize32 || regREX r -> Nothing
  _  _ (Just r) _ | regSize r /= OpSize32 || regREX r -> Nothing

  -- nothing to encode
  _  Nothing Nothing NoDisp -> Nothing

  -- special case for EBP without disp: it has to be encoded with a 0 displacement
  sc i JR_EBP NoDisp
    -> encodeMem32 sc i JR_EBP (Disp8 0)

  ----------------
  -- disp alone --
  ----------------

  _  Nothing Nothing (Disp32 d) -> ret_disp32 d
  _  Nothing Nothing (Disp16 d) -> ret_disp32 (i32FromI16 d)
  _  Nothing Nothing (Disp8  d) -> ret_disp32 (i32FromI8 d)

  --------------------------
  -- without scaled index --
  --------------------------

  -- special case for ESP: it has to be encoded with a SIB
  _  Nothing JR_ESP (disp_mod -> (mod,disp))
    -> ret_sib mod disp (s Scale1 0b100 0b100)

  -- base case
  _  Nothing (Just r) (disp_mod -> (mod,disp))
    -> ret mod (regCode r) disp Nothing

  -----------------------
  -- with scaled index --
  -----------------------

  -- FIXME: the following transformations may change the default segment!

  -- 1-scale without base: use the index as base
  Scale1 i Nothing d -> encodeMem32 Scale1 Nothing i d

  -- 2-scale without base and disp: transform 2*r ==> r+r to avoid requiring a 0 disp
  Scale2 i Nothing NoDisp -> encodeMem32 Scale1 i i NoDisp

  -- ESP can't be scaled. Try to switch with base if 1-scaled
  Scale1 JR_ESP (Just b) d
    | b /= R_ESP -> encodeMem32 Scale1 (Just b) JR_ESP d

  -- otherwise bailout
  _ JR_ESP _ _ -> Nothing

  -- no-base case: we need a disp32
  scale (Just i) Nothing (disp32 -> disp)
    -> ret_sib 0b00 disp (s scale (regCode i) 0b101)

  -- base case
  scale (Just i) (Just b) (disp_mod -> (mod,disp))
    -> ret_sib mod disp (s scale (regCode i) (regCode b))

  where
    ret a b c d = Just (mkModRM_mod_rm a b,c,d)
    ret_sib a c d = ret a 0b100 c d
    ret_disp32 d = ret 0b00 0b101 (Disp32 d) Nothing
    s a b c   = Just (mkSIB a b c)
    disp_mod = \case
      NoDisp   -> (0b00, NoDisp)
      Disp8 d  -> (0b01, Disp8 d)
      Disp16 d -> (0b10, Disp32 (i32FromI16 d))
      Disp32 d -> (0b10, Disp32 d)
    disp32 = \case
      NoDisp   -> Disp32 0
      Disp8 d  -> Disp32 (i32FromI8 d)
      Disp16 d -> Disp32 (i32FromI16 d)
      Disp32 d -> Disp32 d

-- | Return (modrm,disp,sib,rex) for the given 64-bit addressing form scale-index-base-disp
encodeMem64 :: Scale -> Maybe Reg -> Maybe Reg -> Disp -> Maybe (ModRM,Disp,Maybe SIB,Maybe Rex)
encodeMem64 = \cases
  -- only support 64-bit registers (with or without REX)
  _  (Just r) _ _ | regSize r /= OpSize64 -> Nothing
  _  _ (Just r) _ | regSize r /= OpSize64 -> Nothing

  -- nothing to encode
  _  Nothing Nothing NoDisp -> Nothing

  -- special case for RBP/R13 without disp: they have to be encoded with a 0
  -- displacement
  sc i b NoDisp
    | b == JR_RBP || b == JR_R13
    -> encodeMem64 sc i b (Disp8 0)

  ----------------
  -- disp alone --
  ----------------

  _  Nothing Nothing (Disp32 d) -> ret_disp32 d
  _  Nothing Nothing (Disp16 d) -> ret_disp32 (i32FromI16 d)
  _  Nothing Nothing (Disp8  d) -> ret_disp32 (i32FromI8 d)

  --------------------------
  -- without scaled index --
  --------------------------

  -- special case for RSP/R12: they have to be encoded with a SIB
  _  Nothing JR_RSP (disp_mod -> (mod,disp))
    -> ret_sib mod disp (s Scale1 0b100 0b100) Nothing
  _  Nothing JR_R12 (disp_mod -> (mod,disp))
    -> ret_sib mod disp (s Scale1 0b100 0b100) (Just rexB)

  -- base case
  _  Nothing (Just (reg_base -> (rex,b))) (disp_mod -> (mod,disp))
    -> ret mod b disp Nothing rex

  -----------------------
  -- with scaled index --
  -----------------------

  -- FIXME: the following transformations may change the default segment!

  -- 1-scale without base: use the index as base
  Scale1 i Nothing d -> encodeMem64 Scale1 Nothing i d

  -- 2-scale without base and disp: transform 2*r ==> r+r to avoid requiring a 0 disp
  Scale2 i Nothing NoDisp -> encodeMem64 Scale1 i i NoDisp

  -- RSP can't be scaled. Try to switch with base if 1-scaled
  Scale1 JR_RSP (Just b) d
    | b /= R_RSP -> encodeMem64 Scale1 (Just b) JR_RSP d

  -- otherwise bailout
  _ JR_RSP _ _ -> Nothing

  -- no-base case: we need a disp32
  scale (Just (reg_index -> (rex,i))) Nothing (disp32 -> disp)
    -> ret_sib 0b00 disp (s scale i 0b101) rex

  -- base case
  scale (Just (reg_index -> (rex1,i))) (Just (reg_base -> (rex2,b))) (disp_mod -> (mod,disp))
    -> ret_sib mod disp (s scale i b) (rex1 <> rex2)

  where
    ret a b c d e   = Just (mkModRM_mod_rm a b,c,d,e)
    ret_sib a c d e = ret a 0b100 c d e
    -- in 64-bit mode, disp32-only form must be encoded with a SIB (the no-SIB
    -- form is taken for RIP-relative addressing)
    ret_disp32 d = ret_sib 0b00 (Disp32 d) (s Scale1 0b100 0b101) Nothing
    reg_base  (regCodeX -> (x,c)) = (if x then Just rexB else Nothing,c)
    reg_index (regCodeX -> (x,c)) = (if x then Just rexX else Nothing,c)
    s a b c   = Just (mkSIB a b c)
    disp_mod = \case
      NoDisp   -> (0b00, NoDisp)
      Disp8 d  -> (0b01, Disp8 d)
      Disp16 d -> (0b10, Disp32 (i32FromI16 d))
      Disp32 d -> (0b10, Disp32 d)
    disp32 = \case
      NoDisp   -> Disp32 0
      Disp8 d  -> Disp32 (i32FromI8 d)
      Disp16 d -> Disp32 (i32FromI16 d)
      Disp32 d -> Disp32 d


-- | Return (modrm,disp) for the given 64-bit RIP-relative addressing form RIP+disp
encodeMemRel :: Disp -> Maybe (ModRM,Disp)
encodeMemRel disp =
  let !disp' = case disp of
                NoDisp   -> 0
                Disp32 d -> d
                Disp16 d -> (i32FromI16 d)
                Disp8  d -> (i32FromI8 d)
  in Just (mkModRM_mod_rm 0b00 0b101, Disp32 disp')
