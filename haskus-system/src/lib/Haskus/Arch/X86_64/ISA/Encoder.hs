module Haskus.Arch.X86_64.ISA.Encoder
  ( encodeInsn
  , InsnEncErr (..)
  , Operation (..)
  , Operand (..)
  )
where

import Haskus.Arch.X86_64.ISA.Encoding.Enc
import Haskus.Arch.X86_64.ISA.Encoding.Prefix
import Haskus.Arch.X86_64.ISA.Encoding.Reg
import Haskus.Arch.X86_64.ISA.Encoding.Rex
import Haskus.Arch.X86_64.ISA.Context
import Haskus.Arch.X86_64.ISA.Size

import Haskus.Binary.Word

data Operation
  -- Binary-coded-decimal (BCD) operations
  = AdjustAfterAddition    -- ^ AAA
  | AdjustAfterSubtraction -- ^ AAS
  | AdjustBeforeDivision   -- ^ AAD
  | AdjustAfterMultiply    -- ^ AAM
  | AddWithCarry           -- ^ ADC
  | Add                    -- ^ ADD
  deriving (Show,Eq,Ord)

data Mem
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

data InsnEncErr
  = OpNotAllowedIn64bitMode   -- ^ Operation not allowed in 64-bit mode
  | OpAllowedOnlyIn64bitMode  -- ^ Operation only allowed in 64-bit mode
  | InvalidOperands           -- ^ Invalid operands
  | UnknownEncodingError      -- ^ Unknown encoding error. Most likely a missed case in the assembler. Report it!
  deriving (Show,Eq,Ord)

-- | Get the encoding specification of an instruction and its operands
encodeInsn :: Context -> Operation -> Operands -> Either InsnEncErr Enc
encodeInsn ctx op args = do
  let mode64 = is64bitMode (ctxMode ctx)
      assert_mode64 = if mode64 then Right () else Left OpAllowedOnlyIn64bitMode
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
          -> Just (pure (primary_imm8 oc i))
        OPS_RM16_I16 (RM_Reg R_AX) i
          -> Just (pure (set_opsize16 (primary_imm16 (oc+1) i)))
        OPS_RM32_I32 (RM_Reg R_EAX) i
          -> Just (pure (set_opsize32 (primary_imm32 (oc+1) i)))
        OPS_RM64_I32 (RM_Reg R_RAX) i
          -> Just do
            assert_mode64
            pure (set_opsize64 (primary_imm32 (oc+1) i))
        _ -> Nothing

  case op of
    AdjustAfterAddition -> do
      assert_not_mode64
      assert_no_args
      pure (primary 0x37)

    AdjustAfterSubtraction -> do
      assert_not_mode64
      assert_no_args
      pure (primary 0x3F)

    AdjustBeforeDivision -> do
      assert_not_mode64
      i <- imm8_arg
      pure (primary_imm8 0xD5 i)

    AdjustAfterMultiply -> do
      assert_not_mode64
      i <- imm8_arg
      pure (primary_imm8 0xD4 i)

    -- TODO: handle lock. Maybe a different instruction to avoid considering
    -- modifiers every time? Or apply modifiers to an Enc afterwards
    AddWithCarry
      | Just r <- handle_acc_imm 0x14 -> r
      | otherwise -> invalid_operands

    Add
      | Just r <- handle_acc_imm 0x04 -> r
      | otherwise -> invalid_operands
