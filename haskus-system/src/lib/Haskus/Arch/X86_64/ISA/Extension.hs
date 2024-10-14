-- | Extensions to the x86 architecture
module Haskus.Arch.X86_64.ISA.Extension
  ( Extension (..)
  , ExtensionSet
  , allExtensions
  , hasExtension
  )
where

-- | Instruction extensions
--
-- Table 5.1 and Table 5.2 in Intel manuals
data Extension
   = FPU             -- ^ X87 FPU
   | MMX             -- ^ MMX
   | SSE             -- ^ SSE extension
   | SSE2            -- ^ SSE2 extension
   | SSE3            -- ^ SSE3 extension
   | SSSE3           -- ^ SSSE3 extension
   | SSE4_1          -- ^ SSE4.1 extension
   | SSE4_2          -- ^ SSE4.2 extension
   | AVX             -- ^ AVX extension
   | AVX2            -- ^ AVX2 extension
   | VMX             -- ^ VMX extension
   | SMX             -- ^ SMX extension
   | ADX             -- ^ ADX extension (ADOX, ADCX instructions)
   | MOVBE           -- ^ MOVBE instruction
   | CRC32           -- ^ CRC32 extension
   | POPCNT          -- ^ POPCNT extension
   | AES             -- ^ AES extension
   | PCLMULQDQ       -- ^ PCLMULQDQ instruction
   | F16C            -- ^ VCVTPH2PS/VCVTPS2PH instructions
   | RDRAND          -- ^ RDRAND instruction
   | FSGSBASE        -- ^ RDFSBASE instruction
   | FMA             -- ^ Fused multiply-add extension
   | BMI1            -- ^ BMI1 extension
   | BMI2            -- ^ BMI2 extension
   | INVPCID         -- ^ Invalid process-context identifier (INVPCID) extension
   | LZCNT           -- ^ LZCNT instruction
   | TSX             -- ^ TSX extension
   | PREFETCHW       -- ^ PREFETCHW instruction
   | RDSEED          -- ^ RDSEED instruction
   | CLAC            -- ^ CLAC instruction
   | STAC            -- ^ STAC instruction
   | PREFETCHWT1     -- ^ PREFETCHWT1 instruction
   | CLFLUSH         -- ^ CLFLUSH instruction
   | VEX             -- ^ VEX encoded instruction support
   | XOP             -- ^ XOP encoded instruction support
   | SMAP            -- ^ Supervisor Mode Access Prevention (SMAP)
   | CX8             -- ^ CMPXCHG8B instruction
   | CMOV            -- ^ CMOVs instructions (and FCMOVcc if FPU is set too)
   | MONITOR         -- ^ MONITOR/MWAIT
   | OSPKE           -- ^ RDPKRU instruction
   | LSAHF           -- ^ LAHF/SAHF instruction in 64-bit mode
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
