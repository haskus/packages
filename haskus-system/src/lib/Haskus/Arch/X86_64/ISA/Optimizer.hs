{-# LANGUAGE OverloadedLists #-}

-- | Assembly optimizer
module Haskus.Arch.X86_64.ISA.Optimizer
  ( OptimOpts (..)
  , defaultOptimOpts
  , optimizeInsn
  )
where

import Haskus.Arch.X86_64.ISA.Context
import Haskus.Arch.X86_64.ISA.Encoding.Operand
import Haskus.Arch.X86_64.ISA.Encoding.Operation
import Haskus.Arch.X86_64.ISA.Encoding.Reg
import Haskus.Binary.Bits
import Haskus.Binary.Cast

data OptimOpts = OptimOpts
  { optZeroExtend32 :: !Bool
      -- ^ Writing to the 32-bit lower half of a 64-bit register actually zeroes
      -- the upper half. Use this fact to rewrite some 64-bit operations as
      -- 32-bit ones.

  , optSignExtendImm :: !Bool
      -- ^ Rewrite MOVs into MOVSX if it can reduce the immediate size.
  }

defaultOptimOpts :: OptimOpts
defaultOptimOpts = OptimOpts
  { optZeroExtend32   = True
  , optSignExtendImm  = True
  }

optimizeInsn :: OptimOpts -> Context -> Operation -> Operands -> Maybe (Operation,Operands)
optimizeInsn !opts !ctx !op !args = case optimize_insn opts ctx op args of
  Nothing -> Nothing
  Just (op',args')
    -- optimize insn until fixpoint is reached
    -> let go o as = case optimize_insn opts ctx o as of
            Nothing       -> Just (o,as)
            Just (o',as') -> go o' as'
       in go op' args'

optimize_insn :: OptimOpts -> Context -> Operation -> Operands -> Maybe (Operation,Operands)
optimize_insn !opts !_ctx = \cases

  -- Transform: MOV RaX, 0..0_xxxx (imm64 with higher half set to 0)
  -- Into: MOV EaX, xxxx (imm32)
  -- Reason: 32-bit MOV is zero-extended into the upper half
  MOV [R64 r, I64 i]
    | optZeroExtend32 opts
    , i .&. 0xFFFFFFFF00000000 == 0
    -> Just (MOV, [OpReg (asReg32 r), I32 (narrowU64ToU32 i)])

  -- Transform: MOV RaX, 1..1_1xxx (imm64 with higher half set to 1 and lower half signed)
  -- Into: MOVSX EaX, 1xxx (imm32)
  MOV [R64 r, I64 i]
    | optSignExtendImm opts
    , i .&. 0xFFFFFFFF80000000 == 0xFFFFFFFF80000000
    -> Just (MOVSX, [OpReg r, I32 (narrowU64ToU32 i)])

  _ _ -> Nothing
