module Haskus.Arch.X86_64.ISA.Encoding.Mem
  ( Disp (..)
  , Scale (..)
  , Mem (..)
  , encodeMem16
  , encodeMem32
--  , encodeMem64
  , encodeMemRel
  )
where

import Prelude hiding (mod)
import Haskus.Arch.X86_64.ISA.Encoding.Reg
import Haskus.Arch.X86_64.ISA.Encoding.SIB
import Haskus.Arch.X86_64.ISA.Size
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



-- | Return (mod,rm,disp,sib) for the given 32-bit addressing form scale-index-base-disp
encodeMem32 :: Scale -> Maybe Reg -> Maybe Reg -> Disp -> Maybe (U8,U8,Disp,Maybe SIB)
encodeMem32 = \cases
  -- only support 32-bit registers that don't require REX
  _  (Just r) _ _ | regSize r /= OpSize32 || regREX r -> Nothing
  _  _ (Just r) _ | regSize r /= OpSize32 || regREX r -> Nothing

  -- nothing to encode
  _  Nothing Nothing NoDisp -> Nothing

  ----------------
  -- disp alone --
  ----------------

  _  Nothing Nothing (Disp32 d) -> ret 0b00 0b101 (Disp32 d)              Nothing
  _  Nothing Nothing (Disp16 d) -> ret 0b00 0b101 (Disp32 (i32FromI16 d)) Nothing
  _  Nothing Nothing (Disp8  d) -> ret 0b00 0b101 (Disp32 (i32FromI8 d))  Nothing

  --------------------------
  -- without scaled index --
  --------------------------

  -- special case for ESP: it has to be encoded with a SIB
  _  Nothing JR_ESP (disp_mod -> (mod,disp))
    -> ret mod 0b100 disp (s Scale1 0b100 0b100)

  -- special case for EBP without disp: it has to be encoded with a 0 displacement
  _  Nothing JR_EBP NoDisp
    -> encodeMem32 Scale1 Nothing JR_EBP (Disp8 0)

  -- base case
  _  Nothing (Just r) (disp_mod -> (mod,disp))
    -> ret mod (regCode r) disp Nothing

  -----------------------
  -- with scaled index --
  -----------------------

  -- 1-scale without base: use the index as base
  Scale1 i Nothing d -> encodeMem32 Scale1 Nothing i d

  -- ESP can't be scaled. Try to switch with base if 1-scaled
  Scale1 JR_ESP (Just b) d
    | b /= R_ESP -> encodeMem32 Scale1 (Just b) JR_ESP d

  -- otherwise bailout
  _ JR_ESP _ _ -> Nothing

  -- special case for EBP without disp: it has to be encoded with a 0 displacement
  scale (Just i) JR_EBP NoDisp
    -> encodeMem32 scale (Just i) JR_EBP (Disp8 0)

  -- no-base case
  scale (Just i) Nothing (disp_mod -> (mod,disp))
    -> ret_sib mod disp (s scale (regCode i) 0b101)

  -- base case
  scale (Just i) (Just b) (disp_mod -> (mod,disp))
    -> ret_sib mod disp (s scale (regCode i) (regCode b))

  where
    ret a b c d = Just (a,b,c,d)
    ret_sib a c d = ret a 0b100 c d
    s a b c   = Just (mkSIB a b c)
    disp_mod = \case
      NoDisp   -> (0b00, NoDisp)
      Disp8 d  -> (0b01, Disp8 d)
      Disp16 d -> (0b10, Disp32 (i32FromI16 d))
      Disp32 d -> (0b10, Disp32 d)

-- | Return (mod,rm,disp,sib,rex) for the given 64-bit addressing form scale-index-base-disp
-- encodeMem64 :: Scale -> Maybe Reg -> Maybe Reg -> Disp -> Maybe (U8,U8,Disp,Maybe SIB,Maybe Rex)
-- encodeMem64 = \cases
--   -- only support 64-bit registers
--   _  (Just r) _ _ | regSize r /= OpSize64 -> Nothing
--   _  _ (Just r) _ | regSize r /= OpSize64 -> Nothing
-- 
--   _       Nothing JR_RSP  d           -> encodeMem64 Scale1 JS_RSP Nothing d
-- 
--   -- no scaled index, base, no displacement
--   _       Nothing JR_RAX  NoDisp      -> r 0b00 0b000 NoDisp    Nothing     Nothing
--   _       Nothing JR_RCX  NoDisp      -> r 0b00 0b001 NoDisp    Nothing     Nothing
--   _       Nothing JR_RDX  NoDisp      -> r 0b00 0b010 NoDisp    Nothing     Nothing
--   _       Nothing JR_RBX  NoDisp      -> r 0b00 0b011 NoDisp    Nothing     Nothing
--   _       Nothing JR_RBP  NoDisp      -> r 0b01 0b101 (Disp8 0) Nothing     Nothing
--   _       Nothing JR_RSI  NoDisp      -> r 0b00 0b110 NoDisp    Nothing     Nothing
--   _       Nothing JR_RDI  NoDisp      -> r 0b00 0b111 NoDisp    Nothing     Nothing
-- 
--   -- no scaled index, base, disp8
--   _       Nothing JR_RAX  (Disp8 d)   -> r 0b01 0b000 (Disp8 d) Nothing     Nothing
--   _       Nothing JR_RCX  (Disp8 d)   -> r 0b01 0b001 (Disp8 d) Nothing     Nothing
--   _       Nothing JR_RDX  (Disp8 d)   -> r 0b01 0b010 (Disp8 d) Nothing     Nothing
--   _       Nothing JR_RBX  (Disp8 d)   -> r 0b01 0b011 (Disp8 d) Nothing     Nothing
--   _       Nothing JR_RBP  (Disp8 d)   -> r 0b01 0b101 (Disp8 d) Nothing     Nothing
--   _       Nothing JR_RSI  (Disp8 d)   -> r 0b01 0b110 (Disp8 d) Nothing     Nothing
--   _       Nothing JR_RDI  (Disp8 d)   -> r 0b01 0b111 (Disp8 d) Nothing     Nothing
-- 
--   -- no scaled index, base, disp32
--   _       Nothing JR_RAX  (Disp32 d)  -> r 0b10 0b000 (Disp32 d) Nothing    Nothing
--   _       Nothing JR_RCX  (Disp32 d)  -> r 0b10 0b001 (Disp32 d) Nothing    Nothing
--   _       Nothing JR_RDX  (Disp32 d)  -> r 0b10 0b010 (Disp32 d) Nothing    Nothing
--   _       Nothing JR_RBX  (Disp32 d)  -> r 0b10 0b011 (Disp32 d) Nothing    Nothing
--   _       Nothing JR_RBP  (Disp32 d)  -> r 0b10 0b101 (Disp32 d) Nothing    Nothing
--   _       Nothing JR_RSI  (Disp32 d)  -> r 0b10 0b110 (Disp32 d) Nothing    Nothing
--   _       Nothing JR_RDI  (Disp32 d)  -> r 0b10 0b111 (Disp32 d) Nothing    Nothing
-- 
--   -- disp alone
--   _       Nothing Nothing (Disp32 d)  -> r 0b00 0b101 (Disp32 d)             (s Scale1 0b100 0b101)
--   _       Nothing Nothing (Disp8 d)   -> r 0b00 0b101 (Disp32 (i32FromI8 d)) Nothing
-- 
--   -- scaled index
--   Scale1  i                        Nothing          d
--     -- encode 1-scaled register without SIB when possible.
--     -- However RSP can only be encoded with a SIB
--     | i /= JR_RSP -> encodeMem64 Scale1 i Nothing d
-- 
--   sc      (scaled_index -> Just i) Nothing          (Disp32 d) -> r 0b00 0b100 (Disp32 d) (s sc i 0b101)
--   sc      (scaled_index -> Just i) (base -> Just b) (Disp8 d)  -> r 0b01 0b100 (Disp8 d)  (s sc i b)
--   sc      (scaled_index -> Just i) (base -> Just b) (Disp32 d) -> r 0b10 0b100 (Disp32 d) (s sc i b)
--   Scale2  i                        Nothing          d          -> encodeMem64 Scale1 i i d
--   sc      i                        Nothing          NoDisp     -> encodeMem64 sc     i Nothing (Disp32 0)
-- 
--   -- any disp16 can be handled as a disp32
--   sc      i       b       (Disp16 d)  -> encodeMem64 sc i b (Disp32 (i32FromI16 d))
--   _       _       _       _           -> Nothing
--   where
--     r a b c d = Just (a,b,c,d)
--     s a b c   = Just (mkSIB a b c)
--     base = \case
--       JR_RAX  -> Just 0b000
--       JR_RCX  -> Just 0b001
--       JR_RDX  -> Just 0b010
--       JR_RBX  -> Just 0b011
--       JR_RSP  -> Just 0b100
--       JR_RBP  -> Just 0b101
--       JR_RSI  -> Just 0b110
--       JR_RDI  -> Just 0b111
--       _       -> Nothing
--     scaled_index = \case
--       JR_RAX  -> Just 0b000
--       JR_RCX  -> Just 0b001
--       JR_RDX  -> Just 0b010
--       JR_RBX  -> Just 0b011
--       Nothing -> Just 0b100
--       JR_RBP  -> Just 0b101
--       JR_RSI  -> Just 0b110
--       JR_RDI  -> Just 0b111
--       _       -> Nothing

-- | Return (mod,rm,disp) for the given 64-bit RIP-relative addressing form RIP+disp
encodeMemRel :: Disp -> Maybe (U8,U8,Disp)
encodeMemRel = \case
  NoDisp   -> Just (0b00,0b101,Disp32 0)
  Disp32 d -> Just (0b00,0b101,Disp32 d)
  Disp16 d -> Just (0b00,0b101,Disp32 (i32FromI16 d))
  Disp8  d -> Just (0b00,0b101,Disp32 (i32FromI8 d))
