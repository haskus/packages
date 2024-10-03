module Haskus.Arch.X86_64.ISA.Encoder
  ( encodeInsn
  , Operation (..)
  , Operand (..)
  )
where

import Haskus.Arch.X86_64.ISA.Encoding.Enc
import Haskus.Arch.X86_64.ISA.Encoding.Prefix
import Haskus.Arch.X86_64.ISA.Encoding.Reg
import Haskus.Arch.X86_64.ISA.Encoding.Rex
import Haskus.Arch.X86_64.ISA.Encoding.ModRM
import Haskus.Arch.X86_64.ISA.Encoding.Mem
import Haskus.Arch.X86_64.ISA.Encoding.Vec
import Haskus.Arch.X86_64.ISA.Context
import Haskus.Arch.X86_64.ISA.Size

import Haskus.Binary.Word
import Control.Applicative

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
  | ADCX    -- ^ Unsigned add with carry flag: CF:DEST := DEST + SRC + CF
  | ADOX    -- ^ Unsigned add with overflow flag: OF:DEST := DEST + SRC + OF
  -- DEC
  -- DIV
  -- IDIV
  -- IMUL
  -- INC
  -- MUL
  -- MULX
  -- NEG
  -- SBB
  -- SUB

  -- Conversions
  -- CBW -- ^ Sign extension of rAX in rAX
  -- CWD -- ^ Sign-extension in rAX in rDX

  -- Moves
  -- CMOV
  -- IN
  -- INS, INSB, INSW, INSD
  -- LDS, LES, LFS, LGS, LSS
  -- LEA
  -- LODSB, LODSW, LODSD, LODSQ
  -- MOV
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

data Operand
  = Imm !SizedValue
  | Reg !Reg
  | Mem !Mem
  deriving (Show,Eq,Ord)

data RegMem
  = RM_Reg !Reg
  | RM_Mem !Mem
  deriving (Show,Eq,Ord)

data VecMem
  = VM_Vec !Vec
  | VM_Mem !Mem
  deriving (Show,Eq,Ord)

-- | Only some operands are valid together (not for every operation of course)
data Operands
  = NoOperand
  | OPS_I8         !U8              -- ^ imm8
  | OPS_I16        !U16             -- ^ imm16
  | OPS_I32        !U32             -- ^ imm32
  | OPS_I64        !U64             -- ^ imm64
  | OPS_RM8_I8     !RegMem !U8      -- ^ reg/mem8, imm8
  | OPS_RM16_I16   !RegMem !U16     -- ^ reg/mem16, imm16
  | OPS_RM32_I32   !RegMem !U32     -- ^ reg/mem32, imm32
  | OPS_RM64_I32   !RegMem !U32     -- ^ reg/mem64, imm32 (sign-extended)
  | OPS_RM16_I8    !RegMem !U8      -- ^ reg/mem16, imm8 (sign-extended)
  | OPS_RM32_I8    !RegMem !U8      -- ^ reg/mem32, imm8 (sign-extended)
  | OPS_RM64_I8    !RegMem !U8      -- ^ reg/mem64, imm8 (sign-extended)
  | OPS_RM8_R8     !RegMem !Reg     -- ^ reg/mem8, reg8
  | OPS_RM16_R16   !RegMem !Reg     -- ^ reg/mem16, reg16
  | OPS_RM32_R32   !RegMem !Reg     -- ^ reg/mem32, reg32
  | OPS_RM64_R64   !RegMem !Reg     -- ^ reg/mem64, reg64
  | OPS_R8_RM8     !Reg    !RegMem  -- ^ reg8, reg/mem8
  | OPS_R16_RM16   !Reg    !RegMem  -- ^ reg16, reg/mem16
  | OPS_R32_RM32   !Reg    !RegMem  -- ^ reg32, reg/mem32
  | OPS_R64_RM64   !Reg    !RegMem  -- ^ reg64, reg/mem64
  | OPS_V128_VM128 !Vec    !VecMem  -- ^ vec128, vec/mem128
  deriving (Show,Eq,Ord)

-- | Get the encoding specification of an instruction and its operands
encodeInsn :: Context -> Operation -> Operands -> Maybe Enc
encodeInsn ctx op args = do
  let mode64            = is64bitMode (ctxMode ctx)
      assert_mode64     = if mode64     then Just () else Nothing
      assert_not_mode64 = if not mode64 then Just () else Nothing
      has_extension x   = if extensionAvailable ctx x then Just () else Nothing

      assert_no_args = case args of
        NoOperand -> Just ()
        _         -> Nothing

      imm8_arg = case args of
        OPS_I8 x -> Just x
        _        -> Nothing
      
      primary   oc = emptyEnc { encOpcode = Just (Op oc) }
      map_0F    oc = emptyEnc { encOpcode = Just (Op_0F oc) }
      map_0F38  oc = emptyEnc { encOpcode = Just (Op_0F38 oc) }

      prefix_66 e  = e { encPrefixes = P_66 : encPrefixes e }
      prefix_F3 e  = e { encPrefixes = P_F3 : encPrefixes e }
      map_66_0F   oc = prefix_66 $ map_0F   oc
      map_66_0F38 oc = prefix_66 $ map_0F38 oc
      map_F3_0F38 oc = prefix_F3 $ map_0F38 oc

      primary_imm8  oc i = set_imm (SizedValue8 i)  $ primary oc
      primary_imm16 oc i = set_imm (SizedValue16 i) $ primary oc
      primary_imm32 oc i = set_imm (SizedValue32 i) $ primary oc

      set_imm imm e = e { encImm = Just imm }

      set_opsize16 e = case defaultOperationSize ctx of
        OpSize16 -> e
        OpSize32 -> e { encPrefixes = P_66 : encPrefixes e }
        opsize   -> error $ "set_opsize16: invalid operand size: " ++ show opsize

      set_opsize32 e = case defaultOperationSize ctx of
        OpSize32 -> e
        OpSize16 -> e { encPrefixes = P_66 : encPrefixes e }
        opsize   -> error $ "set_opsize32: invalid operand size: " ++ show opsize

      set_opsize64 e = e { encRex = encRex e <> Just rexW }

      -- store opcode extension in ModRM.reg
      set_r_ext ext e = e { encModRM = encModRM e <> Just (mkModRM_reg ext) }

      -- store gpr in REX.R:ModRM.reg
      set_r_gpr r e = set_r_reg (regREX r) xc c e
        where !(xc,c) = regCodeX r

      -- store vec in REX.R:ModRM.reg
      set_r_vec r e = set_r_reg False xc c e
        where !(xc,c) = vecCodeX r

      -- store register code in REX.R:ModRM.reg
      set_r_reg force_rex xr r e =
        let -- handle registers that require REX
            mrex1 = if force_rex then Just emptyRex else Nothing
            -- register code extension in REX.R
            mrex2 = if xr        then Just rexR     else Nothing
        in e { encRex   = encRex e <> mrex1 <> mrex2
             , encModRM = encModRM e <> Just (mkModRM_reg r)
             }

      -- store gpr in REX.B:ModRM.rm
      set_m_gpr r e = set_m_reg (regREX r) xc c e
        where !(xc,c) = regCodeX r

      -- store register code in REX.B:ModRM.rm
      set_m_reg force_rex xr r e =
        let -- handle registers that require REX
            mrex1 = if force_rex then Just emptyRex else Nothing
            -- register code extension in REX.B
            mrex2 = if xr        then Just rexB     else Nothing
        in e { encRex   = encRex e <> mrex1 <> mrex2
             , encModRM = encModRM e <> Just (mkModRM_mod_rm 0b11 r)
             }

      -- encode memory operand
      set_m_mem mem e = case encodeMem ctx mem e of
        Nothing -> error $ "Couldn't encode memory operand: " ++ show mem
        Just e' -> e'

      -- store opcode extension in ModRM.reg and reg in ModRM.rm
      set_rm_ext_reg x r e = set_r_ext x $ set_m_gpr r e
      set_rm_reg_mem r m e = set_r_gpr r $ set_m_mem m e
      set_rm_ext_mem x m e = set_r_ext x $ set_m_mem m e
      set_rm_vec_mem v m e = set_r_vec v $ set_m_mem m e

      -- store v1 in ModRM.reg and v2 in ModRM.rm
      set_rm_vec_vec v1 v2 enc =
        let
            -- ModRM.rm extension in REX.B; ModRM.reg extension in REX.R
            !(xr,r) = vecCodeX v1
            !(xm,m) = vecCodeX v2
            mrex1 = if xr then Just rexR else Nothing
            mrex2 = if xm then Just rexB else Nothing
        in enc { encRex   = encRex enc <> mrex1 <> mrex2
               , encModRM = Just (mkModRM 0b11 r m)
               }

      -- store r1 in ModRM.reg and r2 in ModRM.rm
      set_rm_reg_reg r1 r2 enc
        | not (compatibleRegs r1 r2) = error "Trying to encode incompatible registers"
        | otherwise =
        let
            -- handle registers that require REX
            mrex1 = if regREX r1 || regREX r2 then Just emptyRex else Nothing
            -- ModRM.rm extension in REX.B; ModRM.reg extension in REX.R
            !(xr,r) = regCodeX r1
            !(xm,m) = regCodeX r2
            mrex2 = if xr then Just rexR else Nothing
            mrex3 = if xm then Just rexB else Nothing
        in enc { encRex   = encRex enc <> mrex1 <> mrex2 <> mrex3
               , encModRM = Just (mkModRM 0b11 r m)
               }

      -- store r1 in ModRM.rm and r2 in ModRM.reg
      set_mr_reg_reg r1 r2 e = set_rm_reg_reg r2 r1 e

      -- several instructions have special encodings for the rAX, immN cases
      -- (ADD, ADC, etc.). We handle them here.
      handle_acc_imm oc = case args of
        OPS_RM8_I8 (RM_Reg R_AL) i -> do
          pure $ primary_imm8 oc i
        OPS_RM16_I16 (RM_Reg R_AX) i -> do
          pure $ set_opsize16 $ primary_imm16 (oc+1) i
        OPS_RM32_I32 (RM_Reg R_EAX) i -> do
          pure $ set_opsize32 $ primary_imm32 (oc+1) i
        OPS_RM64_I32 (RM_Reg R_RAX) i -> do
          assert_mode64
          pure $ set_opsize64 $ primary_imm32 (oc+1) i
        _ -> Nothing

      -- handle regN, immN cases (reg in ModRM.rm field)
      handle_reg_imm oc ext = case args of
        OPS_RM8_I8   (RM_Reg r) i -> do
          pure $ set_rm_ext_reg ext r $ primary_imm8 oc i
        OPS_RM16_I16 (RM_Reg r) i -> do
          pure $ set_opsize16 $ set_rm_ext_reg ext r $ primary_imm16 (oc+1) i
        OPS_RM32_I32 (RM_Reg r) i -> do
          pure $ set_opsize32 $ set_rm_ext_reg ext r $ primary_imm32 (oc+1) i
        OPS_RM64_I32 (RM_Reg r) i -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_reg ext r $ primary_imm32 (oc+1) i
        _ -> Nothing

      -- handle regN, imm8 cases (reg in ModRM.rm field)
      handle_reg_imm8 oc ext = case args of
        OPS_RM16_I8 (RM_Reg r) i -> do
          pure $ set_opsize16 $ set_rm_ext_reg ext r $ primary_imm8 oc i
        OPS_RM32_I8 (RM_Reg r) i -> do
          pure $ set_opsize32 $ set_rm_ext_reg ext r $ primary_imm8 oc i
        OPS_RM64_I8 (RM_Reg r) i -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_reg ext r $ primary_imm8 oc i
        _ -> Nothing

      -- handle memN, immN cases
      handle_mem_imm oc ext = case args of
        OPS_RM8_I8   (RM_Mem m) i -> do
          pure $ set_rm_ext_mem ext m $ primary_imm8 oc i
        OPS_RM16_I16 (RM_Mem m) i -> do
          pure $ set_opsize16 $ set_rm_ext_mem ext m $ primary_imm16 (oc+1) i
        OPS_RM32_I32 (RM_Mem m) i -> do
          pure $ set_opsize32 $ set_rm_ext_mem ext m $ primary_imm32 (oc+1) i
        OPS_RM64_I32 (RM_Mem m) i -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_mem ext m $ primary_imm32 (oc+1) i
        _ -> Nothing

      -- handle memN, imm8 cases
      handle_mem_imm8 oc ext = case args of
        OPS_RM16_I8 (RM_Mem m) i -> do
          pure $ set_opsize16 $ set_rm_ext_mem ext m $ primary_imm8 oc i
        OPS_RM32_I8 (RM_Mem m) i -> do
          pure $ set_opsize32 $ set_rm_ext_mem ext m $ primary_imm8 oc i
        OPS_RM64_I8 (RM_Mem m) i -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_mem ext m $ primary_imm8 oc i
        _ -> Nothing

      -- handle reg:ModRM.rm, reg:ModRM.reg cases
      handle_regs_rm_reg oc = case args of
        OPS_RM8_R8   (RM_Reg r1) r2 -> do
          pure $ set_mr_reg_reg r1 r2 $ primary oc
        OPS_RM16_R16 (RM_Reg r1) r2 -> do
          pure $ set_opsize16 $ set_mr_reg_reg r1 r2 $ primary (oc+1)
        OPS_RM32_R32 (RM_Reg r1) r2 -> do
          pure $ set_opsize32 $ set_mr_reg_reg r1 r2 $ primary (oc+1)
        OPS_RM64_R64 (RM_Reg r1) r2 -> do
          assert_mode64
          pure $ set_opsize64 $ set_mr_reg_reg r1 r2 $ primary (oc+1)
        _ -> Nothing

      -- handle reg:ModRM.reg, reg:ModRM.rm cases
      handle_regs_reg_rm oc = case args of
        OPS_R8_RM8   r1 (RM_Reg r2) -> do
          pure $ set_rm_reg_reg r1 r2 $ primary oc
        OPS_R16_RM16 r1 (RM_Reg r2) -> do
          pure $ set_opsize16 $ set_rm_reg_reg r1 r2 $ primary (oc+1)
        OPS_R32_RM32 r1 (RM_Reg r2) -> do
          pure $ set_opsize32 $ set_rm_reg_reg r1 r2 $ primary (oc+1)
        OPS_R64_RM64 r1 (RM_Reg r2) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_reg r1 r2 $ primary (oc+1)
        _ -> Nothing

      -- handle mem:ModRM.rm, reg:ModRM.reg cases
      handle_mem_reg oc = case args of
        OPS_RM8_R8   (RM_Mem m) r -> do
          pure $ set_rm_reg_mem r m $ primary oc
        OPS_RM16_R16 (RM_Mem m) r -> do
          pure $ set_opsize16 $ set_rm_reg_mem r m $ primary (oc+1)
        OPS_RM32_R32 (RM_Mem m) r -> do
          pure $ set_opsize32 $ set_rm_reg_mem r m $ primary (oc+1)
        OPS_RM64_R64 (RM_Mem m) r -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_mem r m $ primary (oc+1)
        _ -> Nothing

      -- handle reg:ModRM.reg, mem:ModRM.rm cases
      handle_reg_mem oc = case args of
        OPS_R8_RM8   r (RM_Mem m) -> do
          pure $ set_rm_reg_mem r m $ primary oc
        OPS_R16_RM16 r (RM_Mem m) -> do
          pure $ set_opsize16 $ set_rm_reg_mem r m $ primary (oc+1)
        OPS_R32_RM32 r (RM_Mem m) -> do
          pure $ set_opsize32 $ set_rm_reg_mem r m $ primary (oc+1)
        OPS_R64_RM64 r (RM_Mem m) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_mem r m $ primary (oc+1)
        _ -> Nothing

      handle_rm_imm  oc ext = handle_reg_imm oc ext  <|> handle_mem_imm oc ext
      handle_rm_imm8 oc ext = handle_reg_imm8 oc ext <|> handle_mem_imm8 oc ext
      handle_rm_reg  oc     = handle_regs_rm_reg oc  <|> handle_mem_reg oc
      handle_reg_rm  oc     = handle_regs_reg_rm oc  <|> handle_reg_mem oc

  case op of
    AAA -> do
      assert_not_mode64
      assert_no_args
      pure $ primary 0x37

    AAS -> do
      assert_not_mode64
      assert_no_args
      pure $ primary 0x3F

    AAD -> do
      assert_not_mode64
      primary_imm8 0xD5 <$> imm8_arg

    AAM -> do
      assert_not_mode64
      primary_imm8 0xD4 <$> imm8_arg

    DAA -> do
      assert_not_mode64
      assert_no_args
      pure $ primary 0x27

    DAS -> do
      assert_not_mode64
      assert_no_args
      pure $ primary 0x2F

    ADC -> asum
      [ handle_acc_imm  0x14
      , handle_rm_imm   0x80 0x2
      , handle_rm_imm8  0x83 0x2
      , handle_rm_reg   0x10
      , handle_reg_rm   0x12
      ]

    ADCX -> do
      has_extension ADX
      case args of
        OPS_R32_RM32 r (RM_Mem m) -> do
          pure $ set_rm_reg_mem r m $ map_66_0F38 0xF6
        OPS_R64_RM64 r (RM_Mem m) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_mem r m $ map_66_0F38 0xF6
        OPS_R32_RM32 r (RM_Reg rm) -> do
          pure $ set_rm_reg_reg r rm $ map_66_0F38 0xF6
        OPS_R64_RM64 r (RM_Reg rm) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_reg r rm $ map_66_0F38 0xF6
        _ -> Nothing

    ADD -> asum
      [ handle_acc_imm  0x04
      , handle_rm_imm   0x80 0x0
      , handle_rm_imm8  0x83 0x0
      , handle_rm_reg   0x00
      , handle_reg_rm   0x02
      ]

    ADOX -> do
      has_extension ADX
      case args of
        OPS_R32_RM32 r (RM_Mem m) -> do
          pure $ set_rm_reg_mem r m $ map_F3_0F38 0xF6
        OPS_R64_RM64 r (RM_Mem m) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_mem r m $ map_F3_0F38 0xF6
        OPS_R32_RM32 r (RM_Reg rm) -> do
          pure $ set_rm_reg_reg r rm $ map_F3_0F38 0xF6
        OPS_R64_RM64 r (RM_Reg rm) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_reg r rm $ map_F3_0F38 0xF6
        _ -> Nothing

    AND -> asum
      [ handle_acc_imm  0x24
      , handle_rm_imm   0x80 0x4
      , handle_rm_imm8  0x83 0x4
      , handle_rm_reg   0x20
      , handle_reg_rm   0x22
      ]


    ADDPD -> 
      case args of
        OPS_V128_VM128 v (VM_Mem m) -> do
          has_extension SSE2
          pure $ set_rm_vec_mem v m $ map_66_0F 0x58
        OPS_V128_VM128 v1 (VM_Vec v2) -> do
          has_extension SSE2
          pure $ set_rm_vec_vec v1 v2 $ map_66_0F 0x58
        _ -> Nothing
