module Haskus.Arch.X86_64.ISA.Encoding.Mem
  ( encodeMem16
  , encodeMem32
  , encodeMem64
  , encodeMemRel
  , encode_mem16
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

encodeMem16 :: Maybe Reg -> Maybe Reg -> Disp -> Maybe (ModRM,Disp)
encodeMem16 base index disp
  -- base case
  | Just r <- encode_mem16 base index disp
  = Just r

  -- try switching the arguments if neither register is BP, otherwise the default
  -- segment would switch from DS to SS or vice-versa
  | index /= JR_BP && base /= JR_BP
  , Just r <- encode_mem16 index base disp
  = Just r

  -- if disp8, retry with it sign-extended to disp16
  | Disp8 d <- disp
  = encodeMem16 base index (Disp16 (i16FromI8 d))

  | otherwise
  = Nothing


-- | Return (modrm,disp) for the given 16-bit addressing form base-index-disp.
--
-- modrm.reg is set to 0
--
-- This function doesn't try to arrange its arguments differently to make them
-- encodable. Use encodeMem16 for this
encode_mem16 :: Maybe Reg -> Maybe Reg -> Disp -> Maybe (ModRM,Disp)
encode_mem16 = \cases
  JR_BX   JR_SI   NoDisp      -> r 0b00 0b000 NoDisp
  JR_BX   JR_DI   NoDisp      -> r 0b00 0b001 NoDisp
  JR_BP   JR_SI   NoDisp      -> r 0b00 0b010 NoDisp
  JR_BP   JR_DI   NoDisp      -> r 0b00 0b011 NoDisp
  JR_SI   Nothing NoDisp      -> r 0b00 0b100 NoDisp
  JR_DI   Nothing NoDisp      -> r 0b00 0b101 NoDisp
  Nothing Nothing (Disp16 d)  -> r 0b00 0b110 (Disp16 d)
  JR_BX   Nothing NoDisp      -> r 0b00 0b111 NoDisp

  JR_BX   JR_SI   (Disp8 d)   -> r 0b01 0b000 (Disp8 d)
  JR_BX   JR_DI   (Disp8 d)   -> r 0b01 0b001 (Disp8 d)
  JR_BP   JR_SI   (Disp8 d)   -> r 0b01 0b010 (Disp8 d)
  JR_BP   JR_DI   (Disp8 d)   -> r 0b01 0b011 (Disp8 d)
  JR_SI   Nothing (Disp8 d)   -> r 0b01 0b100 (Disp8 d)
  JR_DI   Nothing (Disp8 d)   -> r 0b01 0b101 (Disp8 d)
  JR_BP   Nothing (Disp8 d)   -> r 0b01 0b110 (Disp8 d)
  JR_BX   Nothing (Disp8 d)   -> r 0b01 0b111 (Disp8 d)

  JR_BX   JR_SI   (Disp16 d)  -> r 0b10 0b000 (Disp16 d)
  JR_BX   JR_DI   (Disp16 d)  -> r 0b10 0b001 (Disp16 d)
  JR_BP   JR_SI   (Disp16 d)  -> r 0b10 0b010 (Disp16 d)
  JR_BP   JR_DI   (Disp16 d)  -> r 0b10 0b011 (Disp16 d)
  JR_SI   Nothing (Disp16 d)  -> r 0b10 0b100 (Disp16 d)
  JR_DI   Nothing (Disp16 d)  -> r 0b10 0b101 (Disp16 d)
  JR_BP   Nothing (Disp16 d)  -> r 0b10 0b110 (Disp16 d)
  JR_BX   Nothing (Disp16 d)  -> r 0b10 0b111 (Disp16 d)

  _       _       _           -> Nothing
  where
    r a b c = Just (mkModRM_mod_rm a b,c)


-- | Return (modrm,disp,sib) for the given 32-bit addressing form scale-index-base-disp
--
encodeMem32 :: Scale -> Maybe Reg -> Maybe Reg -> Disp -> Maybe (ModRM,Disp,Maybe SIB)
encodeMem32 scale index base disp
  -- 2-scale without base and disp: transform 2*r ==> r+r to avoid requiring a 0 disp
  -- only if r /= ESP/EBP
  | Scale2 <- scale
  , Nothing <- base
  , NoDisp <- disp
  , index /= Nothing && index /= JR_ESP && index /= JR_EBP
  = encodeMem32 Scale1 index index NoDisp

  -- base case
  | Just r <- encode_mem32 scale index base disp
  = Just r

  -- 1-scale without base: try using the index as base (if not ESP/EBP)
  | Scale1 <- scale
  , Nothing <- base
  , index /= Nothing && index /= JR_ESP && index /= JR_EBP
  = encodeMem32 Scale1 Nothing index disp

  -- try to sign-extend disp16
  | Disp16 d <- disp
  = encodeMem32 scale index base (Disp32 (i32FromI16 d))

  -- try to sign-extend disp8
  | Disp8 d <- disp
  = encodeMem32 scale index base (Disp32 (i32FromI8 d))

  -- try with a 0 disp
  | NoDisp <- disp
  = encodeMem32 scale index base (Disp8 0)

  | otherwise
  = Nothing


-- | Return (modrm,disp,sib) for the given 32-bit addressing form scale-index-base-disp
--
-- modrm.reg is set to 0
--
-- This function doesn't try to arrange its arguments differently to make them
-- encodable. Use encodeMem16 for this
encode_mem32 :: Scale -> Maybe Reg -> Maybe Reg -> Disp -> Maybe (ModRM,Disp,Maybe SIB)
encode_mem32 = \cases
  -- without SIB byte
  _      Nothing JR_EAX  NoDisp      -> ret 0b00 0b000 NoDisp     Nothing
  _      Nothing JR_ECX  NoDisp      -> ret 0b00 0b001 NoDisp     Nothing
  _      Nothing JR_EDX  NoDisp      -> ret 0b00 0b010 NoDisp     Nothing
  _      Nothing JR_EBX  NoDisp      -> ret 0b00 0b011 NoDisp     Nothing
  _      Nothing Nothing (Disp32 d)  -> ret 0b00 0b101 (Disp32 d) Nothing
  _      Nothing JR_ESI  NoDisp      -> ret 0b00 0b110 NoDisp     Nothing
  _      Nothing JR_EDI  NoDisp      -> ret 0b00 0b111 NoDisp     Nothing

  _      Nothing JR_EAX  (Disp8 d)   -> ret 0b01 0b000 (Disp8 d)  Nothing
  _      Nothing JR_ECX  (Disp8 d)   -> ret 0b01 0b001 (Disp8 d)  Nothing
  _      Nothing JR_EDX  (Disp8 d)   -> ret 0b01 0b010 (Disp8 d)  Nothing
  _      Nothing JR_EBX  (Disp8 d)   -> ret 0b01 0b011 (Disp8 d)  Nothing
  _      Nothing JR_EBP  (Disp8 d)   -> ret 0b01 0b101 (Disp8 d)  Nothing
  _      Nothing JR_ESI  (Disp8 d)   -> ret 0b01 0b110 (Disp8 d)  Nothing
  _      Nothing JR_EDI  (Disp8 d)   -> ret 0b01 0b111 (Disp8 d)  Nothing

  _      Nothing JR_EAX  (Disp32 d)  -> ret 0b10 0b000 (Disp32 d) Nothing
  _      Nothing JR_ECX  (Disp32 d)  -> ret 0b10 0b001 (Disp32 d) Nothing
  _      Nothing JR_EDX  (Disp32 d)  -> ret 0b10 0b010 (Disp32 d) Nothing
  _      Nothing JR_EBX  (Disp32 d)  -> ret 0b10 0b011 (Disp32 d) Nothing
  _      Nothing JR_EBP  (Disp32 d)  -> ret 0b10 0b101 (Disp32 d) Nothing
  _      Nothing JR_ESI  (Disp32 d)  -> ret 0b10 0b110 (Disp32 d) Nothing
  _      Nothing JR_EDI  (Disp32 d)  -> ret 0b10 0b111 (Disp32 d) Nothing

  -- with SIB byte
  scale (scaled_index -> Just index) Nothing (Disp32 d)
    -> ret_sib 0b00 (Disp32 d) (s scale index 0b101)

  scale (scaled_index -> Just index) (sib_base -> Just base) (Disp8 d)
    -> ret_sib 0b01 (Disp8 d) (s scale index base)

  scale (scaled_index -> Just index) (sib_base -> Just base) (Disp32 d)
    -> ret_sib 0b10 (Disp32 d) (s scale index base)

  _ _ _ _ -> Nothing

  where
    ret a b c d = Just (mkModRM_mod_rm a b,c,d)
    ret_sib a c d = ret a 0b100 c d
    s a b c   = Just (mkSIB a b c)
    sib_base = \case
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
