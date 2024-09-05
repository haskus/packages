-- | X86 architectures support several operating modes.
-- This module gives information for each mode
module Haskus.Arch.X86_64.ISA.Mode
   (
   -- * Operating mode
     X86Mode (..)
   , allModes
   , ModeInfo (..)
   , getModeInfo
   , is64bitMode
   , is32bitMode
   , isLongMode
   , modeName
   -- * Execution mode
   , ExecMode (..)
   , defaultOperationSize
   , defaultAddressSize
   , overriddenOperationSize
   , overriddenAddressSize
   , overriddenOperationSize64
   , defaultStackSize
   , hasExtension
   -- * Extensions
   , X86Extension(..)
   , allExtensions
   )
where

import Haskus.Arch.X86_64.ISA.Size


-- | X86 and X86-64 operating mode
data X86Mode
  = Mode64    -- ^ 64-bit mode ("long 64-bit mode")
  | Mode64_32 -- ^ 64-bit sub mode: compat with 32-bit code ("long compatibility mode")
  | Mode32    -- ^ 32-bit mode ("protected mode")
  | Mode32_16 -- ^ 32-bit sub mode: compat with 16-bit code ("virtual-8086 mode")
  | Mode16    -- ^ 16-bit mode ("real mode")
  deriving (Show,Eq,Ord,Enum,Bounded)

-- | All the X86 modes
allModes :: [X86Mode]
allModes  = [ Mode16 .. Mode64]

-- | Return the mode name
modeName :: X86Mode -> String
modeName = \case
  Mode64    -> "Long 64-bit mode"
  Mode64_32 -> "Long compatibility mode"
  Mode32    -> "Protected mode"
  Mode32_16 -> "Virtual-8086 mode"
  Mode16    -> "Real-mode"

data X86Extension
   = VEX             -- ^ VEX encoded instruction support
   | XOP             -- ^ XOP encoded instruction support
   | ADX             -- ^ ADX extension
   | MMX             -- ^ MMX
   | AVX             -- ^ AVX extension
   | AVX2            -- ^ AVX2 extension
   | SSE             -- ^ SSE extension
   | SSE2            -- ^ SSE2 extension
   | SSE3            -- ^ SSE3 extension
   | SSSE3           -- ^ SSSE3 extension
   | SSE4_1          -- ^ SSE4.1 extension
   | SSE4_2          -- ^ SSE4.2 extension
   | AES             -- ^ AES extension
   | BMI1            -- ^ BMI1 extension
   | BMI2            -- ^ BMI2 extension
   | SMAP            -- ^ Supervisor Mode Access Prevention (SMAP)
   | CLFLUSH         -- ^ CLFLUSH instruction
   | CX8             -- ^ CMPXCHG8B instruction
   | FPU             -- ^ x87 instructions
   | CMOV            -- ^ CMOVs instructions (and FCMOVcc if FPU is set too)
   | INVPCID         -- ^ Invalid process-context identifier (INVPCID) extension
   | MONITOR         -- ^ MONITOR/MWAIT
   | PCLMULQDQ       -- ^ PCLMULQDQ instruction
   | PRFCHW          -- ^ PREFETCHW instruction
   | PREFETCHWT1     -- ^ PREFETCHWT1 instruction
   | FSGSBASE        -- ^ RDFSBASE instruction
   | OSPKE           -- ^ RDPKRU instruction
   | RDRAND          -- ^ RDRAND instruction
   | RDSEDD          -- ^ RDSEED instruction
   | LSAHF           -- ^ LAHF/SAHF instruction in 64-bit mode
   | F16C            -- ^ VCVTPH2PS/VCVTPS2PH instructions
   | FMA             -- ^ Fused multiply-add extension
   | RTM             -- ^ Transactional memory
   | AMD3DNow        -- ^ AMD 3DNow! instructions
   | CET             -- ^ Control-flow enforcement technology
   deriving (Show,Eq,Enum,Bounded)

-- | All the X86 extensions
allExtensions :: [X86Extension]
allExtensions = [minBound .. maxBound]

-- | Indicate if an extension is enabled
hasExtension :: ExecMode -> X86Extension -> Bool
hasExtension mode ext = ext `elem` extensions mode

-- | IP-relative addressing support
data RelativeAddressing
   = FullRelativeAddressing      -- ^ Supported by all instructions
   | ControlRelativeAddressing   -- ^ Supported by control instructions
   deriving (Show,Eq)

-- | Information on a given mode
data ModeInfo = ModeInfo
   { relativeAddressing :: RelativeAddressing -- ^ IP-relative addressing support
   }

-- | Return information for the selected mode
getModeInfo :: X86Mode -> ModeInfo
getModeInfo = \case
   Mode64    -> ModeInfo FullRelativeAddressing
   Mode64_32 -> ModeInfo ControlRelativeAddressing
   Mode32    -> ModeInfo ControlRelativeAddressing
   Mode32_16 -> ModeInfo ControlRelativeAddressing
   Mode16    -> ModeInfo ControlRelativeAddressing

-- | Indicate if it is 64 bit mode
is64bitMode :: X86Mode -> Bool
is64bitMode = \case
  Mode64 -> True
  _      -> False

-- | Indicate if it is 32 bit mode
is32bitMode :: X86Mode -> Bool
is32bitMode = \case
  Mode64_32 -> True
  Mode32    -> True
  _         -> False

-- | Indicate if it is Long mode
isLongMode :: X86Mode -> Bool
isLongMode = \case
  Mode64    -> True
  Mode64_32 -> True
  _         -> False


-- | Execution mode
data ExecMode = ExecMode
   { x86Mode            :: X86Mode        -- ^ x86 mode
   , csDescriptorFlagD  :: Bool           -- ^ D flag: true for 32-bit default sizes
   , ssDescriptorFlagB  :: Bool           -- ^ B flag: true for 32-bit stack size
   , extensions         :: [X86Extension] -- ^ Enabled extensions
   }

-- | Default address size
defaultAddressSize :: ExecMode -> AddressSize
defaultAddressSize mode = case x86Mode mode of
  Mode64                    -> AddrSize64
  Mode64_32
   | csDescriptorFlagD mode -> AddrSize32
   | otherwise              -> AddrSize16
  Mode32
   | csDescriptorFlagD mode -> AddrSize32
   | otherwise              -> AddrSize16
  Mode32_16                 -> AddrSize16
  Mode16                    -> AddrSize16

-- | Default operation size
defaultOperationSize :: ExecMode -> OperandSize
defaultOperationSize mode = case x86Mode mode of
  Mode64                    -> OpSize32
  Mode64_32
   | csDescriptorFlagD mode -> OpSize32
   | otherwise              -> OpSize16
  Mode32
   | csDescriptorFlagD mode -> OpSize32
   | otherwise              -> OpSize16
  Mode32_16                 -> OpSize16
  Mode16                    -> OpSize16

-- | Default stack size
defaultStackSize :: ExecMode -> AddressSize
defaultStackSize mode = case x86Mode mode of
  Mode64                     -> AddrSize32
  Mode64_32
    | ssDescriptorFlagB mode -> AddrSize32
    | otherwise              -> AddrSize16
  Mode32
    | ssDescriptorFlagB mode -> AddrSize32
    | otherwise              -> AddrSize16
  Mode32_16                  -> AddrSize16
  Mode16                     -> AddrSize16

-- | Compute the overridden address size, given the presence or not of the 0x67
-- prefix
overriddenAddressSize :: Bool -> ExecMode -> AddressSize
overriddenAddressSize False mode = defaultAddressSize mode
overriddenAddressSize True  mode =
  case defaultAddressSize mode of
     AddrSize16 -> AddrSize32
     AddrSize32 -> AddrSize16
     AddrSize64 -> AddrSize32

-- | Compute the overridden operation size, given the presence or not of the
-- 0x66 prefix
overriddenOperationSize :: Bool -> ExecMode -> OperandSize
overriddenOperationSize False mode = defaultOperationSize mode
overriddenOperationSize True  mode =
  case defaultOperationSize mode of
     OpSize16 -> OpSize32
     _        -> OpSize16

-- | Compute the overridden operation size in 64-bit, given the presence or not
-- of the 0x66 prefix, the presence of the W prefix and whether the instruction
-- defaults to 64-bit operation size
overriddenOperationSize64 :: Bool -> Bool -> Bool -> ExecMode -> OperandSize
overriddenOperationSize64 p66 pW p64 mode =
  case x86Mode mode of
     -- in 64-bit mode, most 64-bit instructions default to 32-bit operand
     -- size, except those with the DefaultOperandSize64 property.
     -- REX.W/VEX.W/XOP.W can be used to set a 64-bit operand size (it has
     -- precedence over the 0x66 legacy prefix)
     Mode64
        | p64 || pW -> OpSize64
     _              -> overriddenOperationSize p66 mode
