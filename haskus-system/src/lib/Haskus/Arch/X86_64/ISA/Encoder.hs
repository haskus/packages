module Haskus.Arch.X86_64.ISA.Encoder
  ( encodeInsn
  , InsnEncErr (..)
  , Operation (..)
  , Operand (..)
  )
where

import Haskus.Arch.X86_64.ISA.Encoding.Enc
import Haskus.Arch.X86_64.ISA.Context
import Haskus.Arch.X86_64.ISA.Size

data Operation
  -- Binary-coded-decimal (BCD) operations
  = AdjustAfterAddition    -- ^ AAA
  | AdjustAfterSubtraction -- ^ AAS
  | AdjustBeforeDivision   -- ^ AAD
  | AdjustAfterMultiply    -- ^ AAM
  deriving (Show,Eq,Ord)

data Operand
  = Imm !SizedValue
  deriving (Show,Eq,Ord)

data InsnEncErr
  = OpNotAllowedIn64bitMode   -- ^ Operation not allowed in 64-bit mode
  | OpAllowedOnlyIn64bitMode  -- ^ Operation only allowed in 64-bit mode
  | InvalidOperands [Operand] -- ^ Invalid operands
  | UnknownEncodingError      -- ^ Unknown encoding error. Most likely a missed case in the assembler. Report it!
  deriving (Show,Eq,Ord)

-- | Get the encoding specification of an instruction and its operands
encodeInsn :: Context -> Operation -> [Operand] -> Either InsnEncErr Enc
encodeInsn ctx op args = do
  let mode64 = is64bitMode (ctxMode ctx)
      assert_mode64 = if mode64 then Right () else Left OpAllowedOnlyIn64bitMode
      assert_not_mode64 = if not mode64 then Right () else Left OpNotAllowedIn64bitMode

      assert_no_args = if null args then Right () else invalid_operands args
      invalid_operands xs = Left (InvalidOperands xs)
      imm8_arg = case args of
        [Imm (SizedValue8 x)] -> Right x
        _                     -> invalid_operands args
      
      primary   oc = emptyEnc { encOpcode = Just (Op oc) }
      secondary oc = emptyEnc { encOpcode = Just (Op_0F oc) }

      primary_imm8 oc i = (primary oc) { encImm = Just (SizedValue8 i) }

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
      x <- imm8_arg
      pure (primary_imm8 0xD5 x)

    AdjustAfterMultiply -> do
      assert_not_mode64
      x <- imm8_arg
      pure (primary_imm8 0xD4 x)
