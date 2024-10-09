module Haskus.Arch.X86_64.ISA.Encoding.Operation
  ( Operation (..)
  )
where

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
  | MOV     -- ^ Move
  | LEA     -- ^ Load effective address: DEST := EA(SRC)

  -- CMOV
  -- IN
  -- INS, INSB, INSW, INSD
  -- LDS, LES, LFS, LGS, LSS
  -- LODSB, LODSW, LODSD, LODSQ
  -- MOVBE
  -- MOVD
  -- MOVNTI
  -- MOVSB, MOVSW, MOVSD, MOVSQ
  -- MOVSX
  -- MOVSXD
  -- MOVZX
  -- OUT
  -- OUTSB, OUTSW, OUTSD
  -- POP
  -- POPA, POPAD
  -- PUSH
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
  -- CALL
  -- ENTER
  -- INT
  -- INTO
  -- JCC
  -- JCXZ, jECXZ, JRCXZ
  -- JMP
  -- LEAVE
  -- LOOP
  -- RET
  -- UD0, UD1, UD2

  -- Flags
  -- CLC
  -- CLD
  -- CMC
  -- LAHF
  -- POPF, POPFD, POPFQ
  -- PUSHF, PUSHFD, PUSHFQ
  -- SAHF
  -- STC
  -- STD

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


