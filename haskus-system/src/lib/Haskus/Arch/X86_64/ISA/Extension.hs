-- | Extensions to the x86 architecture
module Haskus.Arch.X86_64.ISA.Extension
  ( Extension (..)
  , ExtensionSet
  , allExtensions
  , hasExtension
  )
where

data Extension
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

type ExtensionSet = [Extension] -- FIXME: use Enum based BitSet

-- | All the X86 extensions
allExtensions :: ExtensionSet
allExtensions = [minBound .. maxBound]

-- | Indicate if an extension is in the set
hasExtension :: ExtensionSet -> Extension -> Bool
hasExtension xs x = x `elem` xs
