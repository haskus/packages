module Haskus.Arch.X86_64.ISA.Encoder
  ( encodeInsn
  , InsnEncErr (..)
  , Operation (..)
  , Operand (..)
  , Mem(..)
  )
where

import Haskus.Arch.X86_64.ISA.Encoding.Enc
import Haskus.Arch.X86_64.ISA.Encoding.Prefix
import Haskus.Arch.X86_64.ISA.Encoding.Reg
import Haskus.Arch.X86_64.ISA.Encoding.Rex
import Haskus.Arch.X86_64.ISA.Encoding.ModRM
import Haskus.Arch.X86_64.ISA.Encoding.Mem
import Haskus.Arch.X86_64.ISA.Encoding.Disp
import Haskus.Arch.X86_64.ISA.Encoding.SIB
import Haskus.Arch.X86_64.ISA.Encoding.Segment
import Haskus.Arch.X86_64.ISA.Context
import Haskus.Arch.X86_64.ISA.Size

import Haskus.Binary.Word

data Operation
  -- Binary-coded-decimal (BCD) operations
  = AAA     -- ^ Adjust AL after addition
  | AAS     -- ^ Adjust AL after subtraction
  | AAD     -- ^ Adjust AX before division
  | AAM     -- ^ Adjust AX after multiply
  | ADC     -- ^ Add with carry: DEST := DEST + SRC + CF
  | ADD     -- ^ Add: DEST := DEST + SRC
  deriving (Show,Eq,Ord)

data Operand
  = Imm !SizedValue
  | Reg !Reg
  | Mem !Mem
  deriving (Show,Eq,Ord)

data RegMem
  = RM_Reg !Reg
  | RM_Mem !Mem
  deriving (Show,Eq,Ord)

-- | Only some operands are valid together (not for every operation of course)
data Operands
  = NoOperand
  | OPS_I8       !U8              -- ^ imm8
  | OPS_I16      !U16             -- ^ imm16
  | OPS_I32      !U32             -- ^ imm32
  | OPS_I64      !U64             -- ^ imm64
  | OPS_RM8_I8   !RegMem !U8      -- ^ reg/mem8, imm8
  | OPS_RM16_I16 !RegMem !U16     -- ^ reg/mem16, imm16
  | OPS_RM32_I32 !RegMem !U32     -- ^ reg/mem32, imm32
  | OPS_RM64_I32 !RegMem !U32     -- ^ reg/mem64, imm32 (sign-extended)
  | OPS_RM16_I8  !RegMem !U8      -- ^ reg/mem16, imm8 (sign-extended)
  | OPS_RM32_I8  !RegMem !U8      -- ^ reg/mem32, imm8 (sign-extended)
  | OPS_RM64_I8  !RegMem !U8      -- ^ reg/mem64, imm8 (sign-extended)
  | OPS_RM8_R8   !RegMem !Reg     -- ^ reg/mem8, reg8
  | OPS_RM16_R16 !RegMem !Reg     -- ^ reg/mem16, reg16
  | OPS_RM32_R32 !RegMem !Reg     -- ^ reg/mem32, reg32
  | OPS_RM64_R64 !RegMem !Reg     -- ^ reg/mem64, reg64
  | OPS_R8_RM8   !Reg    !RegMem  -- ^ reg8, reg/mem8
  | OPS_R16_RM16 !Reg    !RegMem  -- ^ reg16, reg/mem16
  | OPS_R32_RM32 !Reg    !RegMem  -- ^ reg32, reg/mem32
  | OPS_R64_RM64 !Reg    !RegMem  -- ^ reg64, reg/mem64
  deriving (Show,Eq,Ord)

-- | Memory addressing
data Mem
  = M_16 !(Maybe Segment)        !(Maybe Reg) !(Maybe Reg) !Disp  -- ^ 16-bit index-base-disp
  | M_32 !(Maybe Segment) !Scale !(Maybe Reg) !(Maybe Reg) !Disp  -- ^ 32-bit scaled-index-base-disp
  | M_64 !(Maybe Segment) !Scale !(Maybe Reg) !(Maybe Reg) !Disp  -- ^ 64-bit scaled-index-base-disp
  | M_Rel !(Maybe Segment) !Disp                                  -- ^ RIP-relative displacement
  deriving (Show,Eq,Ord)

-- | Encode a memory operand. Return Nothing in case of failure to encode.
encodeMem :: Mem -> Enc -> Maybe Enc
encodeMem mem e = case mem of
  M_16 seg i b d
    | Just (modrm,disp) <- encodeMem16 i b d
    -> Just $ e { encModRM    = encModRM e <> Just modrm
                , encDisp     = disp
                , encPrefixes = seg_prefix seg (encPrefixes e)
                }
  M_32 seg sc i b d
    | Just (modrm,disp,sib) <- encodeMem32 sc i b d
    -> Just $ e { encModRM    = encModRM e <> Just modrm
                , encDisp     = disp
                , encSIB      = sib
                , encPrefixes = seg_prefix seg (encPrefixes e)
                }
  M_64 seg sc i b d
    | Just (modrm,disp,sib,rex) <- encodeMem64 sc i b d
    -> Just $ e { encModRM    = encModRM e <> Just modrm
                , encDisp     = disp
                , encSIB      = sib
                , encRex      = encRex e <> rex
                , encPrefixes = seg_prefix seg (encPrefixes e)
                }
  M_Rel seg d
    | Just (modrm,disp) <- encodeMemRel d
    -> Just $ e { encModRM    = encModRM e <> Just modrm
                , encDisp     = disp
                , encPrefixes = seg_prefix seg (encPrefixes e)
                }
  _ -> Nothing

  where
    seg_prefix seg ps = case seg of
      Nothing -> ps
      Just s  -> segmentOverridePrefix s : ps


data InsnEncErr
  = OpNotAllowedIn64bitMode   -- ^ Operation not allowed in 64-bit mode
  | OpAllowedOnlyIn64bitMode  -- ^ Operation only allowed in 64-bit mode
  | InvalidOperands           -- ^ Invalid operands
  | UnknownEncodingError      -- ^ Unknown encoding error. Most likely a missed case in the assembler. Report it!
  deriving (Show,Eq,Ord)

-- | Get the encoding specification of an instruction and its operands
encodeInsn :: Context -> Operation -> Operands -> Either InsnEncErr Enc
encodeInsn ctx op args = do
  let mode64            = is64bitMode (ctxMode ctx)
      assert_mode64     = if mode64     then Right () else Left OpAllowedOnlyIn64bitMode
      assert_not_mode64 = if not mode64 then Right () else Left OpNotAllowedIn64bitMode

      assert_no_args = case args of
        NoOperand -> Right ()
        _         -> invalid_operands

      invalid_operands = Left InvalidOperands
      imm8_arg = case args of
        OPS_I8 x -> Right x
        _        -> invalid_operands
      
      primary   oc = emptyEnc { encOpcode = Just (Op oc) }
      secondary oc = emptyEnc { encOpcode = Just (Op_0F oc) }

      primary_imm8  oc i = (primary oc) { encImm = Just (SizedValue8 i) }
      primary_imm16 oc i = (primary oc) { encImm = Just (SizedValue16 i) }
      primary_imm32 oc i = (primary oc) { encImm = Just (SizedValue32 i) }

      set_opsize16 e = case defaultOperationSize ctx of
        OpSize16 -> e
        OpSize32 -> e { encPrefixes = P_66 : encPrefixes e }
        opsize   -> error $ "set_opsize16: invalid operand size: " ++ show opsize

      set_opsize32 e = case defaultOperationSize ctx of
        OpSize32 -> e
        OpSize16 -> e { encPrefixes = P_66 : encPrefixes e }
        opsize   -> error $ "set_opsize32: invalid operand size: " ++ show opsize

      set_opsize64 e = case encRex e of
        Nothing -> e { encRex = Just rexW }
        Just r  -> e { encRex = Just (setRexW r) }

      -- several instructions have special encodings for the rAX, immN cases
      -- (ADD, ADC, etc.). We handle them here.
      handle_acc_imm oc = case args of
        OPS_RM8_I8 (RM_Reg R_AL) i
          -> Just $ pure (primary_imm8 oc i)
        OPS_RM16_I16 (RM_Reg R_AX) i
          -> Just $ pure (set_opsize16 (primary_imm16 (oc+1) i))
        OPS_RM32_I32 (RM_Reg R_EAX) i
          -> Just $ pure (set_opsize32 (primary_imm32 (oc+1) i))
        OPS_RM64_I32 (RM_Reg R_RAX) i
          -> Just do
            assert_mode64
            pure (set_opsize64 (primary_imm32 (oc+1) i))
        _ -> Nothing

      -- handle regN, immN cases (reg in ModRM.rm field)
      handle_reg_imm oc ext = case args of
        OPS_RM8_I8   (RM_Reg r) i
          -> Just $ pure $ set_ext_reg (primary_imm8 oc i) ext r
        OPS_RM16_I16 (RM_Reg r) i
          -> Just $ pure $ set_opsize16 $ set_ext_reg (primary_imm16 (oc+1) i) ext r
        OPS_RM32_I32 (RM_Reg r) i
          -> Just $ pure $ set_opsize32 $ set_ext_reg (primary_imm32 (oc+1) i) ext r
        OPS_RM64_I32 (RM_Reg r) i
          -> Just $ do
            assert_mode64
            pure $ set_opsize64 $ set_ext_reg (primary_imm32 (oc+1) i) ext r
        _ -> Nothing

      -- handle regN, imm8 cases (reg in ModRM.rm field)
      handle_reg_imm8 oc ext = case args of
        OPS_RM16_I8 (RM_Reg r) i
          -> Just $ pure $ set_opsize16 $ set_ext_reg (primary_imm8 oc i) ext r
        OPS_RM32_I8 (RM_Reg r) i
          -> Just $ pure $ set_opsize32 $ set_ext_reg (primary_imm8 oc i) ext r
        OPS_RM64_I8 (RM_Reg r) i
          -> Just $ do
            assert_mode64
            pure $ set_opsize64 $ set_ext_reg (primary_imm8 oc i) ext r
        _ -> Nothing

      -- handle reg:ModRM.rm, reg:ModRM.reg cases
      handle_regs_rm_reg oc = case args of
        OPS_RM8_R8   (RM_Reg r1) r2
          -> Just $ pure $ set_regs_rm_reg (primary oc) r1 r2
        OPS_RM16_R16 (RM_Reg r1) r2
          -> Just $ pure $ set_opsize16 $ set_regs_rm_reg (primary (oc+1)) r1 r2
        OPS_RM32_R32 (RM_Reg r1) r2
          -> Just $ pure $ set_opsize32 $ set_regs_rm_reg (primary (oc+1)) r1 r2
        OPS_RM64_R64 (RM_Reg r1) r2
          -> Just $ do
            assert_mode64
            pure $ set_opsize64 $ set_regs_rm_reg (primary (oc+1)) r1 r2
        _ -> Nothing

      -- handle reg:ModRM.reg, reg:ModRM.rm cases
      handle_regs_reg_rm oc = case args of
        OPS_R8_RM8   r1 (RM_Reg r2)
          -> Just $ pure $ set_regs_reg_rm (primary oc) r1 r2
        OPS_R16_RM16 r1 (RM_Reg r2)
          -> Just $ pure $ set_opsize16 $ set_regs_reg_rm (primary (oc+1)) r1 r2
        OPS_R32_RM32 r1 (RM_Reg r2)
          -> Just $ pure $ set_opsize32 $ set_regs_reg_rm (primary (oc+1)) r1 r2
        OPS_R64_RM64 r1 (RM_Reg r2)
          -> Just $ do
            assert_mode64
            pure $ set_opsize64 $ set_regs_reg_rm (primary (oc+1)) r1 r2
        _ -> Nothing

      -- store ext in ModRM.reg and reg in ModRM.rm
      set_ext_reg enc ext r =
        let !(xc,c) = regCodeX r
            -- handle registers that require REX
            mrex1 = if regREX r then Just emptyRex else Nothing
            -- register extension in REX.B
            mrex2 = if xc       then Just rexB     else Nothing
        in enc { encRex   = mrex1 <> mrex2
               , encModRM = Just (mkModRM_ext_reg ext c)
               }

      -- store r1 in ModRM.reg and r2 in ModRM.rm
      set_regs_reg_rm enc r1 r2
        | not (compatibleRegs r1 r2) = error "Trying to encode incompatible registers"
        | otherwise =
        let 
            -- handle registers that require REX
            mrex1 = if regREX r1 || regREX r2 then Just emptyRex else Nothing
            -- ModRM.rm extension in REX.B; ModRM.reg extension in REX.R
            !(xr,r) = regCodeX r1
            !(xm,m) = regCodeX r2
            mrex2 = if xr then Just rexR else Nothing
            mrex3 = if xm then Just rexB else Nothing
        in enc { encRex   = mrex1 <> mrex2 <> mrex3
               , encModRM = Just (mkModRM_regs_reg_rm r m)
               }

      -- store r1 in ModRM.rm and r2 in ModRM.reg
      set_regs_rm_reg enc r1 r2 = set_regs_reg_rm enc r2 r1


  case op of
    AAA -> do
      assert_not_mode64
      assert_no_args
      pure $ primary 0x37

    AAS -> do
      assert_not_mode64
      assert_no_args
      pure $ primary 0x3F

    AAD -> do
      assert_not_mode64
      i <- imm8_arg
      pure $ primary_imm8 0xD5 i

    AAM -> do
      assert_not_mode64
      i <- imm8_arg
      pure $ primary_imm8 0xD4 i

    -- TODO: handle lock. Maybe a different instruction to avoid considering
    -- modifiers every time? Or apply modifiers to an Enc afterwards
    ADC
      | Just r <- handle_acc_imm  0x14     -> r
      | Just r <- handle_reg_imm  0x80 0x2 -> r
      | Just r <- handle_reg_imm8 0x83 0x2 -> r
      | Just r <- handle_regs_rm_reg 0x10  -> r
      | Just r <- handle_regs_reg_rm 0x12  -> r
      -- TODO: mem, imm
      -- TODO: mem, reg
      -- TODO: reg, mem
      | otherwise -> invalid_operands

    ADD
      | Just r <- handle_acc_imm  0x04     -> r
      | Just r <- handle_reg_imm  0x80 0x0 -> r
      | Just r <- handle_reg_imm8 0x83 0x0 -> r
      | Just r <- handle_regs_rm_reg 0x00  -> r
      | Just r <- handle_regs_reg_rm 0x02  -> r
      -- TODO: mem, imm
      -- TODO: mem, reg
      -- TODO: reg, mem
      | otherwise -> invalid_operands
