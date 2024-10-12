module Haskus.Arch.X86_64.ISA.Encoding.Operation
  ( Operation (..)
  , Cond(..)
  , condCode
  )
where

import Haskus.Binary.Word

data Operation
  ---------------------------------------
  -- General-purpose instructions
  ---------------------------------------

  -- Binary-coded-decimal (BCD) operations
  = AAA     -- ^ Adjust AL after addition
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
  -- MULX

  -- Conversions
  | SXA     -- ^ Sign-extend rAX: rAX := SX(low_half(rAX))
  | SXAD    -- ^ Sign-extend rAX: rDX:rAX := SX(rAX)

  -- Moves
  | MOV        -- ^ Move
  | LEA        -- ^ Load effective address: DEST := EA(SRC)
  | CMOV !Cond -- ^ Conditional move
  | MOVSX      -- ^ Move with sign-extension (use this for MOVSXD too)
  | MOVZX      -- ^ Move with zero-extension
  | IN         -- ^ Input from port
  | OUT        -- ^ Output to port
  | PUSH       -- ^ Push a value onto the stack
  | POP        -- ^ Pop a value from the stack
  -- INS, INSB, INSW, INSD
  -- LDS, LES, LFS, LGS, LSS
  -- LODSB, LODSW, LODSD, LODSQ
  -- MOVBE
  -- MOVD
  -- MOVNTI
  -- MOVSB, MOVSW, MOVSD, MOVSQ
  -- OUTSB, OUTSW, OUTSD
  -- POPA, POPAD
  -- PUSHA, PUSHAD
  -- SETcc
  -- STOSB, STOSW, STOSD, STOSQ
  -- XADD
  -- XCHG
  -- XLAT

  -- Comparisons
  -- CMP
  -- CMPSB, CMPSW, CMPSD, CMPSQ
  -- CMPXCHG
  -- CMPXCHG8B
  -- CMPXCHG16B
  -- SCASB, SCASW, SCASD, SCASQ

  -- Binary
  | AND
  -- ANDN
  -- BEXTR
  -- BLCFILL
  -- BLCI
  -- BLCIC
  -- BLCMSK
  -- BLCS
  -- BLSFILL
  -- BLSI
  -- BLSIC
  -- BLSMSK
  -- BLSR
  -- BSF
  -- BSR
  -- BSWAP
  -- BT
  -- BTC
  -- BTR
  -- BTS
  -- BZHI
  -- LZCNT
  -- NOT
  -- OR
  -- PDEP
  -- PEXT
  -- POPCNT
  -- RCL
  -- RCR
  -- ROL
  -- ROR
  -- RORX
  -- SAL
  -- SHL
  -- SAR
  -- SARX
  -- SHLD
  -- SHLX
  -- SHR
  -- SHRD
  -- SHRX
  -- T1MSKC
  -- TEST
  -- TZCNT
  -- TZMSK
  -- XOR

  -- Control-flow
  | JMP       -- ^ Unconditional jump
  | Jcc !Cond -- ^ Conditional jump
  | INTO      -- ^ Generate overflow trap if OF=1
  | INT1      -- ^ Generate debug trap
  | INT3      -- ^ Generate breakpoint trap
  | INT       -- ^ Generate software interruption
  | SYSCALL   -- ^ Syscall
  | RET       -- ^ Near return (i.e. same CS)
  | RET_FAR   -- ^ Far return (i.e. different CS)
  | CALL      -- ^ Near call (i.e. same CS)
  | CALL_FAR  -- ^ Far call (i.e. different CS)
  | UD0       -- ^ Undefined instruction
  | UD1       -- ^ Undefined instruction
  | UD2       -- ^ Undefined instruction
  -- ENTER -- requires 2 immediates (imm16 then imm8, or imm24) :/
  -- LEAVE
  -- JCXZ, jECXZ, JRCXZ
  -- LOOP

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
  -- POPF, POPFD, POPFQ
  -- PUSHF, PUSHFD, PUSHFQ

  -- Cache management and memory barriers
  -- CLFLUSH
  -- CLFLUSHOPT
  -- CLWB
  -- CLZERO
  -- LFENCE
  -- MFENCE
  -- SFENCE
  -- MCOMMIT
  -- PREFETCH, PREFETCHW, PREFETCHn

  -- Misc instructions
  -- BOUND
  -- CPUID
  -- CRC32
  -- LLWPCB
  -- LWPINS
  -- LWPVAL
  -- MONITORX
  -- MWAITX
  -- NOP
  -- PAUSE
  -- RDRAND
  -- RDSEED
  -- SLWPCB

  ---------------------------------------
  -- System instructions
  ---------------------------------------
  -- RDFSBASE
  -- RDGSBASE
  -- WRFSBASE
  -- WRGSBASE
  -- RDPID
  -- RDPRU
  --

  ---------------------------------------
  -- Vector instructions
  ---------------------------------------
  | ADDPD   -- ^ Parallel add 2 rightmost doubles; other leftmost doubles unmodified (SSE)
  -- MOVMSKPD
  -- MOVMSKPS

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
