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

data OptimOpts = OptimOpts
  { optZeroExtend32 :: !Bool
      -- ^ Writing to the 32-bit lower half of a 64-bit register actually zeroes
      -- the upper half. Use this fact to rewrite some 64-bit operations as
      -- 32-bit ones.
  }

defaultOptimOpts :: OptimOpts
defaultOptimOpts = OptimOpts
  { optZeroExtend32  = True
  }

optimizeInsn :: OptimOpts -> Context -> Operation -> Operands -> Maybe (Operation,Operands)
optimizeInsn !opts !_ctx = \cases
  MOV [R64 r, OpImm i]
    | optZeroExtend32 opts
    -> Just (MOV, [OpReg (asReg32 r), OpImm i])
  _ _ -> Nothing
