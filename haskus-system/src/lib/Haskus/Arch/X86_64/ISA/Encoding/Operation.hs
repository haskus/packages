module Haskus.Arch.X86_64.ISA.Encoding.Operation
  ( Operation (..)
  , Cond(..)
  , condCode
  )
where

import Haskus.Binary.Word
import Haskus.Arch.X86_64.ISA.Size

data Operation
  ---------------------------------------
  -- General-purpose instructions
  ---------------------------------------

  -- Data transfer instructions
  = MOV                 -- ^ Move
  | CMOV !Cond          -- ^ Conditional move
  | XCHG                -- ^ Exchange values
  | XADD                -- ^ Exchange and add
  | PUSH                -- ^ Push a value onto the stack
  | POP                 -- ^ Pop a value from the stack
  | CMPXCHG             -- ^ Compare and exchange
  | CMPXCHGB            -- ^ Compare and exchange bytes
  | POPA  !OperandSize  -- ^ Pop all general purpose registers
  | PUSHA !OperandSize  -- ^ Push all general purpose registers
  | SXA                 -- ^ Sign-extend rAX: rAX := SX(low_half(rAX))
  | SXAD                -- ^ Sign-extend rAX: rDX:rAX := SX(rAX)

  -- Binary-coded-decimal (BCD) operations
  | AAA     -- ^ Adjust AL after addition
  | AAS     -- ^ Adjust AL after subtraction
  | AAD     -- ^ Adjust AX before division
  | AAM     -- ^ Adjust AX after multiply
  | DAA     -- ^ Decimal adjust after addition
  | DAS     -- ^ Decimal adjust after subtraction

  -- Arithmetic
  | ADC     -- ^ Add with carry: DEST := DEST + SRC + CF
  | ADD     -- ^ Add: DEST := DEST + SRC
  | SBB     -- ^ Subtract with borrow: DEST := DEST - (SRC+CF)
  | SUB     -- ^ Subtract: DEST := DEST - SRC
  | DEC     -- ^ Decrement by 1
  | INC     -- ^ Increment by 1
  | DIV     -- ^ Unsigned divide: (rAX,rDX) := rDX:rAX `quotRem` SRC
  | MUL     -- ^ Unsigned multiply: rDX:rAX := rAX * SRC
  | IDIV    -- ^ Signed divide: (rAX,rDX) := rDX:rAX `quotRem` SRC
  | IMUL    -- ^ Signed multiply: rDX:rAX := rAX * SRC
  | NEG     -- ^ Two's complement negation

  | ADCX    -- ^ Unsigned add with carry flag: CF:DEST := DEST + SRC + CF
  | ADOX    -- ^ Unsigned add with overflow flag: OF:DEST := DEST + SRC + OF

  -- Moves
  | LEA           -- ^ Load effective address: DEST := EA(SRC)
  | MOVSX         -- ^ Move with sign-extension (use this for MOVSXD too)
  | MOVZX         -- ^ Move with zero-extension
  | MOVBE         -- ^ Move with byte swap
  | SETcc !Cond   -- ^ Set byte on condition
  | MOVNTI        -- ^ Move using non-temporal hint
  | XLAT          -- ^ Table look-up translation: AL <- [eS:rBX + AL]

  | IN                              -- ^ Input from port
  | OUT                             -- ^ Output to port
  | INS   !OperandSize !AddressSize -- ^ Input from port to string
  | OUTS  !OperandSize !AddressSize -- ^ Output string to port
  | MOVS  !OperandSize !AddressSize -- ^ Move data from string to string (DS:rSI to ES:rDI)
  | STOS  !OperandSize !AddressSize -- ^ Store string (acc to rDI)
  | LODS  !OperandSize !AddressSize -- ^ Load string (DS:rSI into acc)
  | CMPS  !OperandSize !AddressSize -- ^ Compare data at DS:rSI with data at ES:rDI)
  | SCAS  !OperandSize !AddressSize -- ^ Scan string at ES:rDI and compare with rAX

  | LDS   -- ^ Load DS:r with far pointer from memory
  | LES   -- ^ Load ES:r with far pointer from memory
  | LFS   -- ^ Load FS:r with far pointer from memory
  | LGS   -- ^ Load GS:r with far pointer from memory
  | LSS   -- ^ Load SS:r with far pointer from memory
  -- MOVD

  -- Comparisons
  | TEST        -- ^ Logical compare
  | CMP         -- ^ Compare

  -- Binary
  | AND     -- ^ Bitwise AND
  | OR      -- ^ Bitwise OR
  | XOR     -- ^ Bitwise XOR
  | NOT     -- ^ One's complement negation
  | SHL     -- ^ Shift left
  | SHR     -- ^ Shift right
  | SAR     -- ^ Shift arithmetic right
  | ROL     -- ^ Rotate left
  | ROR     -- ^ Rotate right
  | RCL     -- ^ Rotate through carry left
  | RCR     -- ^ Rotate through carry right
  | BSWAP   -- ^ Byte swap
  | POPCNT  -- ^ Count number of bits set to 1
  | LZCNT   -- ^ Count the number of leading zero bits
  | TZCNT   -- ^ Count the number of trailing zero bits
  | BSR     -- ^ Bit scan reverse (search first msb set to 1)
  | BSF     -- ^ Bit scan forward (search first lsb set to 1)
  | BT      -- ^ Bit test (result in CF)
  | BTC     -- ^ Bit test and complement (result in CF)
  | BTR     -- ^ Bit test and reset (result in CF)
  | BTS     -- ^ Bit test and set (result in CF)
  | SHLD    -- ^ Double precision shift left
  | SHRD    -- ^ Double precision shift right

  -- BMI1 & BMI2
  | ANDN    -- ^ Logical AND NOT
  | BEXTR   -- ^ Bit field extract
  | BLSI    -- ^ Extract lowest set isolated bit
  | BLSMSK  -- ^ Get mask up to lowest set bit
  | BLSR    -- ^ Reset lowest set bit
  | PDEP    -- ^ Parallel bits deposit
  | PEXT    -- ^ Parallel bits extract
  | RORX    -- ^ Rotate right logical without affecting flags
  | SARX    -- ^ Shit arithmetic right without affecting flags
  | SHRX    -- ^ Shit logical right without affecting flags
  | SHLX    -- ^ Shit logical left without affecting flags
  | BZHI    -- ^ Zero high bits starting with specified bit position
  | MULX    -- ^ Unsigned multiply without affecting flags

  -- Control-flow
  | JMP       -- ^ Unconditional jump
  | Jcc !Cond -- ^ Conditional jump
  | LOOP      -- ^ Decrement given rCX register. Jump if /=0
  | LOOPE     -- ^ Decrement given rCX register. Jump if /=0 and ZF=1
  | LOOPNE    -- ^ Decrement given rCX register. Jump if /=0 and ZF=0
  | INTO      -- ^ Generate overflow trap if OF=1
  | INT1      -- ^ Generate debug trap
  | INT3      -- ^ Generate breakpoint trap
  | INT       -- ^ Generate software interruption
  | RET       -- ^ Near return (i.e. same CS)
  | RET_FAR   -- ^ Far return (i.e. different CS)
  | CALL      -- ^ Near call (i.e. same CS)
  | CALL_FAR  -- ^ Far call (i.e. different CS)
  | UD0       -- ^ Undefined instruction
  | UD1       -- ^ Undefined instruction
  | UD2       -- ^ Undefined instruction

  | SYSCALL       -- ^ System call
  | SYSRET !Bool  -- ^ System call return (True=enable 32-bit mode)
  | SYSENTER      -- ^ System call enter
  | SYSEXIT !Bool -- ^ System call exit (True=enable 32-bit mode)
  | LEAVE !OperandSize -- ^ High level procedure exit
  -- ENTER -- requires 2 immediates (imm16 then imm8, or imm24) :/
  -- JCXZ, jECXZ, JRCXZ

  -- Flags
  | CLC     -- ^ Clear carry flag
  | STC     -- ^ Set carry flag
  | CLI     -- ^ Clear interrupt flag
  | STI     -- ^ Set interrupt flag
  | CLD     -- ^ Clear direction flag
  | STD     -- ^ Set direction flag
  | CMC     -- ^ Complement carry flag
  | LAHF    -- ^ Load status flags into AH
  | SAHF    -- ^ Store AH into flags

  | POPF  !OperandSize -- ^ Pop stack into FLAGS register
  | PUSHF !OperandSize -- ^ Push FLAGS register onto the stack

  -- Cache management and memory barriers
  | PREFETCHW         -- ^ Prefetch data into caches in anticipation of a write
  | MFENCE            -- ^ Memory fence
  | LFENCE            -- ^ Load fence
  | SFENCE            -- ^ Store fence
  -- CLFLUSH
  -- CLFLUSHOPT
  -- CLWB
  -- CLZERO
  -- MCOMMIT
  -- PREFETCH, PREFETCHn

  -- Misc instructions
  | CPUID     -- ^ CPU identification
  | PAUSE     -- ^ Spin loop hint
  | NOP !U8   -- ^ No operation (size up to 9 bytes)
  | MWAIT     -- ^ Monitor wait
  | RDRAND    -- ^ Read random number
  | RDSEED    -- ^ Read random seed
  -- BOUND
  -- CRC32
  -- LLWPCB
  -- LWPINS
  -- LWPVAL
  -- MONITORX
  -- MWAITX
  -- SLWPCB

  ---------------------------------------
  -- System instructions
  ---------------------------------------
  | STR         -- ^ Store task register
  | LTR         -- ^ Load task register
  | SGDT        -- ^ Store global descriptor table register
  | LGDT        -- ^ Load global descriptor table register
  | SIDT        -- ^ Store interrupt descriptor table register
  | LIDT        -- ^ Load interrupt descriptor table register
  | SLDT        -- ^ Store local descriptor table register
  | LLDT        -- ^ Load local descriptor table register
  | RDFSBASE    -- ^ Load FS base address
  | RDGSBASE    -- ^ Load GS base address
  | WRFSBASE    -- ^ Set FS base address
  | WRGSBASE    -- ^ Set GS base address
  | SWAPGS      -- ^ Swap GS base register
  | RDPID       -- ^ Read processor ID
  | RDTSC       -- ^ Read time-stamp counter
  | RDTSCP      -- ^ Read time-stamp counter and processor ID
  | LSL         -- ^ Load segment limit
  | RDPRU       -- ^ Read processor register

  ---------------------------------------
  -- Vector instructions
  ---------------------------------------

  | ADDPS           -- ^ Add packed F32 (SSE)
  | ADDPD           -- ^ Add packed F64 (SSE)
  | ADDSS           -- ^ Add scalar F32 (SSE)
  | ADDSD           -- ^ Add scalar F64 (SSE)
  | MULPS           -- ^ Multiply packed F32 (SSE)
  | MULPD           -- ^ Multiply packed F64 (SSE)
  | MULSS           -- ^ Multiply scalar F32 (SSE)
  | MULSD           -- ^ Multiply scalar F64 (SSE)
  | DIVPS           -- ^ Divide packed F32 (SSE)
  | DIVPD           -- ^ Divide packed F64 (SSE)
  | DIVSS           -- ^ Divide scalar F32 (SSE)
  | DIVSD           -- ^ Divide scalar F64 (SSE)
  | SUBPS           -- ^ Sub packed F32 (SSE)
  | SUBPD           -- ^ Sub packed F64 (SSE)
  | SUBSS           -- ^ Sub scalar F32 (SSE)
  | SUBSD           -- ^ Sub scalar F64 (SSE)
  | ANDPS           -- ^ AND packed F32 (SSE)
  | ANDPD           -- ^ AND packed F64 (SSE)
  | ANDNPS          -- ^ AND NOT packed F32 (SSE)
  | ANDNPD          -- ^ AND NOT packed F64 (SSE)
  | ORPS            -- ^ OR packed F32 (SSE)
  | ORPD            -- ^ OR packed F64 (SSE)
  | ADDSUBPS        -- ^ Add/Sub packed F32 (SSE)
  | ADDSUBPD        -- ^ Add/Sub packed F64 (SSE)
  | MAXPS           -- ^ Maximum of packed F32 (SSE)
  | MAXPD           -- ^ Maximum of packed F64 (SSE)
  | MAXSS           -- ^ Maximum of scalar F32 (SSE)
  | MAXSD           -- ^ Maximum of scalar F64 (SSE)
  | MINPS           -- ^ Minimum of packed F32 (SSE)
  | MINPD           -- ^ Minimum of packed F64 (SSE)
  | MINSS           -- ^ Minimum of scalar F32 (SSE)
  | MINSD           -- ^ Minimum of scalar F64 (SSE)
  | MOVAPD          -- ^ Move aligned packed F64 (SSE)
  | MOVAPS          -- ^ Move aligned packed F32 (SSE)
  | MOVUPD          -- ^ Move unaligned packed F64 (SSE)
  | MOVUPS          -- ^ Move unaligned packed F32 (SSE)
  | MOVMSKPD        -- ^ Move sign bits of packed F64 (SSE)
  | MOVMSKPS        -- ^ Move sign bits of packed F32 (SSE)
  | SQRTPS          -- ^ Square root of packed F32 (SSE)
  | SQRTPD          -- ^ Square root of packed F64 (SSE)
  | SQRTSS          -- ^ Square root of scalar F32 (SSE)
  | SQRTSD          -- ^ Square root of scalar F64 (SSE)
  | RSQRTPS         -- ^ Reciprocal of square root of packed F32 (SSE)
  | RSQRTSS         -- ^ Reciprocal of square root of scalar F32 (SSE)
  | RCPPS           -- ^ Reciprocal of packed F32 (SSE)
  | RCPSS           -- ^ Reciprocal of scalar F32 (SSE)
  | ROUNDPS         -- ^ Round packed F32 (SSE)
  | ROUNDPD         -- ^ Round packed F64 (SSE)
  | ROUNDSS         -- ^ Round scalar F32 (SSE)
  | ROUNDSD         -- ^ Round scalar F64 (SSE)

  | VADDPS          -- ^ Add packed F32 (AVX)
  | VADDPD          -- ^ Add packed F64 (AVX)
  | VADDSS          -- ^ Add scalar F32 (AVX)
  | VADDSD          -- ^ Add scalar F64 (AVX)
  | VMULPS          -- ^ Multiply packed F32 (AVX)
  | VMULPD          -- ^ Multiply packed F64 (AVX)
  | VMULSS          -- ^ Multiply scalar F32 (AVX)
  | VMULSD          -- ^ Multiply scalar F64 (AVX)
  | VDIVPS          -- ^ Divide packed F32 (AVX)
  | VDIVPD          -- ^ Divide packed F64 (AVX)
  | VDIVSS          -- ^ Divide scalar F32 (AVX)
  | VDIVSD          -- ^ Divide scalar F64 (AVX)
  | VSUBPS          -- ^ Sub packed F32 (AVX)
  | VSUBPD          -- ^ Sub packed F64 (AVX)
  | VSUBSS          -- ^ Sub scalar F32 (AVX)
  | VSUBSD          -- ^ Sub scalar F64 (AVX)
  | VANDPS          -- ^ AND packed F32 (AVX)
  | VANDPD          -- ^ AND packed F64 (AVX)
  | VANDNPS         -- ^ AND NOT packed F32 (AVX)
  | VANDNPD         -- ^ AND NOT packed F64 (AVX)
  | VORPS           -- ^ OR packed F32 (AVX)
  | VORPD           -- ^ OR packed F64 (AVX)
  | VADDSUBPS       -- ^ Add/Sub packed F32 (AVX)
  | VADDSUBPD       -- ^ Add/Sub packed F64 (AVX)
  | VMAXPS          -- ^ Maximum of packed F32 (AVX)
  | VMAXPD          -- ^ Maximum of packed F64 (AVX)
  | VMAXSS          -- ^ Maximum of scalar F32 (AVX)
  | VMAXSD          -- ^ Maximum of scalar F64 (AVX)
  | VMINPS          -- ^ Minimum of packed F32 (AVX)
  | VMINPD          -- ^ Minimum of packed F64 (AVX)
  | VMINSS          -- ^ Minimum of scalar F32 (AVX)
  | VMINSD          -- ^ Minimum of scalar F64 (AVX)
  | VMOVAPD         -- ^ Move aligned packed F64 (AVX)
  | VMOVAPS         -- ^ Move aligned packed F32 (AVX)
  | VMOVUPD         -- ^ Move unaligned packed F64 (AVX)
  | VMOVUPS         -- ^ Move unaligned packed F32 (AVX)
  | VMOVMSKPD       -- ^ Move sign bits of packed F64 (AVX)
  | VMOVMSKPS       -- ^ Move sign bits of packed F32 (AVX)
  | VSQRTPS         -- ^ Square root of packed F32 (AVX)
  | VSQRTPD         -- ^ Square root of packed F64 (AVX)
  | VSQRTSS         -- ^ Square root of scalar F32 (AVX)
  | VSQRTSD         -- ^ Square root of scalar F64 (AVX)
  | VRSQRTPS        -- ^ Reciprocal of square root of packed F32 (AVX)
  | VRSQRTSS        -- ^ Reciprocal of square root of scalar F32 (AVX)
  | VRCPPS          -- ^ Reciprocal of packed F32 (AVX)
  | VRCPSS          -- ^ Reciprocal of scalar F32 (AVX)
  | VROUNDPS        -- ^ Round packed F32 (AVX)
  | VROUNDPD        -- ^ Round packed F64 (AVX)
  | VROUNDSS        -- ^ Round scalar F32 (AVX)
  | VROUNDSD        -- ^ Round scalar F64 (AVX)
  -- POR, PXOR, PTEST, PAND
  -- PSLRW, PSLRD, PSLRQ, PSLRDQ
  -- PADD, PSUB...
  -- PUNPACKL, PUNPACKH
  -- UNPACKL, UNPACKH
  -- SERIALIZE
  -- UCOMISS
  -- CVTxx2yy
  -- VCVTxx2yy

  -- XOP TBM:
  --
  -- BLCFILL
  -- BLCI
  -- BLCIC
  -- BLCMSK
  -- BLCS
  -- BLSFILL
  -- BLSIC
  -- T1MSKC
  -- TZMSK

  deriving (Show,Eq,Ord)


-- | Condition (for Jcc and CMOVcc)
data Cond
  = C_C   -- ^ CF=1. NAE (not above or equal), B (below), C (carry)
  | C_NC  -- ^ CF=0. AE (above or equal), NB (not below), NC (not carry)
  | C_O   -- ^ OF=1. O (overflow)
  | C_NO  -- ^ OF=0. NO (not overflow)
  | C_P   -- ^ PF=1. P (parity), PE (parity even)
  | C_NP  -- ^ PF=0. NP (not parity), PO (parity odd)
  | C_S   -- ^ SF=1. S (sign)
  | C_NS  -- ^ SF=0. NS (not sign)
  | C_Z   -- ^ ZF=1. E (equal), Z (zero)
  | C_NZ  -- ^ ZF=0. NE (not equal), NZ (not zero)
  | C_A   -- ^ CF=0 and ZF=0. A (above), NBE (not below or equal)
  | C_NA  -- ^ CF=1 or  ZF=1. BE (below or equal), NA (not above)
  | C_G   -- ^ ZF=0 and SF=OF. G (greater)
  | C_NG  -- ^ ZF=1 or SF/=OF. LE (less or equal), NG (not greater)
  | C_L   -- ^ SF/=OF. L (less), NGE (not greater or equal)
  | C_NL  -- ^ SF=OF. GE (greater or equal), NL (not less)
  deriving (Show,Eq,Ord)

condCode :: Cond -> U8
condCode = \case
  C_O  -> 0x0
  C_NO -> 0x1
  C_C  -> 0x2
  C_NC -> 0x3
  C_Z  -> 0x4
  C_NZ -> 0x5
  C_NA -> 0x6
  C_A  -> 0x7
  C_S  -> 0x8
  C_NS -> 0x9
  C_P  -> 0xA
  C_NP -> 0xB
  C_L  -> 0xC
  C_NL -> 0xD
  C_NG -> 0xE
  C_G  -> 0xF
