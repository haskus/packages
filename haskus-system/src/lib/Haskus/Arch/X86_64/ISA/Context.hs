-- | Execution context for some code.
module Haskus.Arch.X86_64.ISA.Context
   ( module Haskus.Arch.X86_64.ISA.OperatingMode
   , module Haskus.Arch.X86_64.ISA.Extension
     -- * Execution context
   , Context (..)
   , context64
   , defaultContext64
   , defaultOperationSize
   , defaultAddressSize
   , overriddenOperationSize
   , overriddenAddressSize
   , overriddenOperationSize64
   , defaultStackSize
   , extensionAvailable
   )
where

import Haskus.Arch.X86_64.ISA.Size
import Haskus.Arch.X86_64.ISA.OperatingMode
import Haskus.Arch.X86_64.ISA.Extension

-- | Execution context
--
-- This is useful to know which instructions are available, which registers,
-- etc. and how to encode some instructions.
data Context = Context
  { ctxMode       :: !Mode
     -- ^ Operating mode
  , ctxCS_D       :: !Bool
     -- ^ CS.D flag: used in 32-bit mode to enable 32-bit operand/address size
     -- by default
  , ctxSS_B       :: !Bool
     -- ^ SS.B flag: used in 32-bit mode to enable 32-bit stack size by default
  , ctxExtensions :: !ExtensionSet
     -- ^ Enabled extensions
  }


-- | 64-bit context
context64 :: ExtensionSet -> Context
context64 = Context Mode64 False False

-- | 64-bit context with all extensions enabled
defaultContext64 :: Context
defaultContext64 = context64 allExtensions


-- | Indicate if an extension is enabled
extensionAvailable :: Context -> Extension -> Bool
extensionAvailable ctx ext = hasExtension (ctxExtensions ctx) ext

-- | Default address size
defaultAddressSize :: Context -> AddressSize
defaultAddressSize ctx = case ctxMode ctx of
  Mode64         -> AddrSize64
  Mode64_32
   | ctxCS_D ctx -> AddrSize32
   | otherwise   -> AddrSize16
  Mode32
   | ctxCS_D ctx -> AddrSize32
   | otherwise   -> AddrSize16
  Mode32_16      -> AddrSize16
  Mode16         -> AddrSize16

-- | Default operation size
defaultOperationSize :: Context -> OperandSize
defaultOperationSize ctx = case ctxMode ctx of
  Mode64         -> OpSize32
  Mode64_32
   | ctxCS_D ctx -> OpSize32
   | otherwise   -> OpSize16
  Mode32
   | ctxCS_D ctx -> OpSize32
   | otherwise   -> OpSize16
  Mode32_16      -> OpSize16
  Mode16         -> OpSize16

-- | Default stack size
defaultStackSize :: Context -> AddressSize
defaultStackSize ctx = case ctxMode ctx of
  Mode64          -> AddrSize32
  Mode64_32
    | ctxSS_B ctx -> AddrSize32
    | otherwise   -> AddrSize16
  Mode32
    | ctxSS_B ctx -> AddrSize32
    | otherwise   -> AddrSize16
  Mode32_16       -> AddrSize16
  Mode16          -> AddrSize16

-- | Compute the overridden address size, given the presence or not of the 0x67
-- prefix
overriddenAddressSize :: Bool -> Context -> AddressSize
overriddenAddressSize False ctx = defaultAddressSize ctx
overriddenAddressSize True  ctx =
  case defaultAddressSize ctx of
     AddrSize16 -> AddrSize32
     AddrSize32 -> AddrSize16
     AddrSize64 -> AddrSize32

-- | Compute the overridden operation size, given the presence or not of the
-- 0x66 prefix
overriddenOperationSize :: Bool -> Context -> OperandSize
overriddenOperationSize False ctx = defaultOperationSize ctx
overriddenOperationSize True  ctx =
  case defaultOperationSize ctx of
     OpSize16 -> OpSize32
     _        -> OpSize16

-- | Compute the overridden operation size in 64-bit, given the presence or not
-- of the 0x66 prefix, the presence of the W prefix and whether the instruction
-- defaults to 64-bit operation size
overriddenOperationSize64 :: Bool -> Bool -> Bool -> Context -> OperandSize
overriddenOperationSize64 p66 pW p64 ctx =
  case ctxMode ctx of
     -- in 64-bit mode, most 64-bit instructions default to 32-bit operand
     -- size, except those with the DefaultOperandSize64 property.
     -- REX.W/VEX.W/XOP.W can be used to set a 64-bit operand size (it has
     -- precedence over the 0x66 legacy prefix)
     Mode64
        | p64 || pW -> OpSize64
     _              -> overriddenOperationSize p66 ctx
