
module Haskus.Arch.X86_64.ISA.Encoder
  ( encodeInsn
  )
where

import Haskus.Arch.X86_64.ISA.Encoding.Enc
import Haskus.Arch.X86_64.ISA.Encoding.Operand
import Haskus.Arch.X86_64.ISA.Encoding.Operation
import Haskus.Arch.X86_64.ISA.Encoding.Prefix
import Haskus.Arch.X86_64.ISA.Encoding.Reg
import Haskus.Arch.X86_64.ISA.Encoding.Rex
import Haskus.Arch.X86_64.ISA.Encoding.ModRM
import Haskus.Arch.X86_64.ISA.Encoding.Mem
import Haskus.Arch.X86_64.ISA.Encoding.Vec
import Haskus.Arch.X86_64.ISA.Context
import Haskus.Arch.X86_64.ISA.Size

import Control.Applicative

-- | Get the encoding specification of an instruction and its operands
encodeInsn :: Context -> Operation -> Operands -> Maybe Enc
encodeInsn !ctx !op !args = do
  let mode64            = is64bitMode (ctxMode ctx)
      assert_mode64     = if mode64     then Just () else Nothing
      assert_not_mode64 = if not mode64 then Just () else Nothing
      has_extension x   = if extensionAvailable ctx x then Just () else Nothing

      assert_no_args = case args of
        Ops0 -> Just ()
        _    -> Nothing

      imm8_arg = case args of
        Ops1 (I8 x) -> Just x
        _           -> Nothing
      
      primary   oc = emptyEnc { encOpcode = Just (Op oc) }
      map_0F    oc = emptyEnc { encOpcode = Just (Op_0F oc) }
      map_0F38  oc = emptyEnc { encOpcode = Just (Op_0F38 oc) }

      prefix_66 e  = e { encPrefixes = P_66 : encPrefixes e }
      prefix_F3 e  = e { encPrefixes = P_F3 : encPrefixes e }
      map_66_0F   oc = prefix_66 $ map_0F   oc
      map_66_0F38 oc = prefix_66 $ map_0F38 oc
      map_F3_0F38 oc = prefix_F3 $ map_0F38 oc

      set_imm8  i = set_imm (SizedValue8 i)
      set_imm16 i = set_imm (SizedValue16 i)
      set_imm32 i = set_imm (SizedValue32 i)
      set_imm64 i = set_imm (SizedValue64 i)

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

      -- store register code in REX.R:opcode
      set_oc_reg mk_oc oc r =
        let !(xc,c) = regCodeX r
            -- handle registers that require REX
            mrex1 = if regREX r then Just emptyRex else Nothing
            -- register code extension in REX.R
            mrex2 = if xc       then Just rexR     else Nothing
        in (mk_oc (oc + c)) { encRex = mrex1 <> mrex2 }

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
      handle_acc_imm mk_oc oc = case args of
        Ops2 (OpReg R_AL)  (I8  i) -> do
          pure $ set_imm8 i $ mk_oc oc
        Ops2 (OpReg R_AX)  (I16 i) -> do
          pure $ set_opsize16 $ set_imm16 i $ mk_oc (oc+1)
        Ops2 (OpReg R_EAX) (I32 i) -> do
          pure $ set_opsize32 $ set_imm32 i $ mk_oc (oc+1)
        Ops2 (OpReg R_RAX) (I32 i) -> do
          assert_mode64
          pure $ set_opsize64 $ set_imm32 i $ mk_oc (oc+1)
        _ -> Nothing

      -- handle regN, immN cases (reg in ModRM.rm field)
      handle_reg_imm mk_oc oc ext = case args of
        Ops2 (R8  r) (I8  i) -> do
          pure $ set_rm_ext_reg ext r $ set_imm8 i $ mk_oc oc
        Ops2 (R16 r) (I16 i) -> do
          pure $ set_opsize16 $ set_rm_ext_reg ext r $ set_imm16 i $ mk_oc (oc+1)
        Ops2 (R32 r) (I32 i) -> do
          pure $ set_opsize32 $ set_rm_ext_reg ext r $ set_imm32 i $ mk_oc (oc+1)
        Ops2 (R64 r) (I32 i) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_reg ext r $ set_imm32 i $ mk_oc (oc+1)
        _ -> Nothing

      -- handle regN, imm8 cases (reg in ModRM.rm field)
      handle_reg_imm8 mk_oc oc ext = case args of
        Ops2 (R16 r) (I8 i) -> do
          pure $ set_opsize16 $ set_rm_ext_reg ext r $ set_imm8 i $ mk_oc oc
        Ops2 (R32 r) (I8 i) -> do
          pure $ set_opsize32 $ set_rm_ext_reg ext r $ set_imm8 i $ mk_oc oc
        Ops2 (R64 r) (I8 i) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_reg ext r $ set_imm8 i $ mk_oc oc
        _ -> Nothing

      -- handle memN, immN cases
      handle_mem_imm mk_oc oc ext = case args of
        Ops2 (M8  m) (I8  i) -> do
          pure $ set_rm_ext_mem ext m $ set_imm8 i $ mk_oc oc
        Ops2 (M16 m) (I16 i) -> do
          pure $ set_opsize16 $ set_rm_ext_mem ext m $ set_imm16 i $ mk_oc (oc+1)
        Ops2 (M32 m) (I32 i) -> do
          pure $ set_opsize32 $ set_rm_ext_mem ext m $ set_imm32 i $ mk_oc (oc+1)
        Ops2 (M64 m) (I32 i) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_mem ext m $ set_imm32 i $ mk_oc (oc+1)
        _ -> Nothing

      -- handle memN, imm8 cases
      handle_mem_imm8 mk_oc oc ext = case args of
        Ops2 (M16 m) (I8 i) -> do
          pure $ set_opsize16 $ set_rm_ext_mem ext m $ set_imm8 i $ mk_oc oc
        Ops2 (M32 m) (I8 i) -> do
          pure $ set_opsize32 $ set_rm_ext_mem ext m $ set_imm8 i $ mk_oc oc
        Ops2 (M64 m) (I8 i) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_mem ext m $ set_imm8 i $ mk_oc oc
        _ -> Nothing

      -- handle reg:ModRM.rm, reg:ModRM.reg cases
      handle_regs_rm_reg mk_oc oc = case args of
        Ops2 (R8  r1) (R8  r2) -> do
          pure $ set_mr_reg_reg r1 r2 $ mk_oc oc
        Ops2 (R16 r1) (R16 r2) -> do
          pure $ set_opsize16 $ set_mr_reg_reg r1 r2 $ mk_oc (oc+1)
        Ops2 (R32 r1) (R32 r2) -> do
          pure $ set_opsize32 $ set_mr_reg_reg r1 r2 $ mk_oc (oc+1)
        Ops2 (R64 r1) (R64 r2) -> do
          assert_mode64
          pure $ set_opsize64 $ set_mr_reg_reg r1 r2 $ mk_oc (oc+1)
        _ -> Nothing

      -- handle reg:ModRM.reg, reg:ModRM.rm cases
      handle_regs_reg_rm mk_oc oc = case args of
        Ops2 (R8 r1) (R8 r2) -> do
          pure $ set_rm_reg_reg r1 r2 $ mk_oc oc
        Ops2 (R16 r1) (R16 r2) -> do
          pure $ set_opsize16 $ set_rm_reg_reg r1 r2 $ mk_oc (oc+1)
        Ops2 (R32 r1) (R32 r2) -> do
          pure $ set_opsize32 $ set_rm_reg_reg r1 r2 $ mk_oc (oc+1)
        Ops2 (R64 r1) (R64 r2) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_reg r1 r2 $ mk_oc (oc+1)
        _ -> Nothing
      
      -- handle reg:ModRM.reg, reg:ModRM.rm, i8 cases
      --
      -- Opsize: 16, 32, 64
      handle_regs_reg_rm_i8 mk_oc oc = case args of
        Ops3 (R16 r1) (R16 r2) (I8 i) -> do
          pure $ set_imm8 i $ set_opsize16 $ set_rm_reg_reg r1 r2 $ mk_oc oc
        Ops3 (R32 r1) (R32 r2) (I8 i) -> do
          pure $ set_imm8 i $ set_opsize32 $ set_rm_reg_reg r1 r2 $ mk_oc oc
        Ops3 (R64 r1) (R64 r2) (I8 i) -> do
          assert_mode64
          pure $ set_imm8 i $ set_opsize64 $ set_rm_reg_reg r1 r2 $ mk_oc oc
        _ -> Nothing

      -- handle reg:ModRM.reg, reg:ModRM.rm, imm cases
      --
      -- Opsize: 16, 32, 64
      handle_regs_reg_rm_imm mk_oc oc = case args of
        Ops3 (R16 r1) (R16 r2) (I16 i) -> do
          pure $ set_imm16 i $ set_opsize16 $ set_rm_reg_reg r1 r2 $ mk_oc oc
        Ops3 (R32 r1) (R32 r2) (I32 i) -> do
          pure $ set_imm32 i $ set_opsize32 $ set_rm_reg_reg r1 r2 $ mk_oc oc
        Ops3 (R64 r1) (R64 r2) (I32 i) -> do
          assert_mode64
          pure $ set_imm32 i $ set_opsize64 $ set_rm_reg_reg r1 r2 $ mk_oc oc
        _ -> Nothing

      -- handle mem:ModRM.rm, reg:ModRM.reg cases
      handle_mem_reg mk_oc oc = case args of
        Ops2 (M8 m) (R8   r) -> do
          pure $ set_rm_reg_mem r m $ mk_oc oc
        Ops2 (M16 m) (R16 r) -> do
          pure $ set_opsize16 $ set_rm_reg_mem r m $ mk_oc (oc+1)
        Ops2 (M32 m) (R32 r) -> do
          pure $ set_opsize32 $ set_rm_reg_mem r m $ mk_oc (oc+1)
        Ops2 (M64 m) (R64 r) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_mem r m $ mk_oc (oc+1)
        _ -> Nothing

      -- handle reg:ModRM.reg, mem:ModRM.rm cases
      handle_reg_mem mk_oc oc = case args of
        Ops2 (R8  r) (M8  m) -> do
          pure $ set_rm_reg_mem r m $ mk_oc oc
        Ops2 (R16 r) (M16 m) -> do
          pure $ set_opsize16 $ set_rm_reg_mem r m $ mk_oc (oc+1)
        Ops2 (R32 r) (M32 m) -> do
          pure $ set_opsize32 $ set_rm_reg_mem r m $ mk_oc (oc+1)
        Ops2 (R64 r) (M64 m) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_mem r m $ mk_oc (oc+1)
        _ -> Nothing

      -- handle reg:ModRM.reg, mem:ModRM.rm, i8 cases
      --
      -- Opsize: 16, 32, 64
      handle_reg_mem_i8 mk_oc oc = case args of
        Ops3 (R16 r) (M16 m) (I8 i) -> do
          pure $ set_imm8 i $ set_opsize16 $ set_rm_reg_mem r m $ mk_oc oc
        Ops3 (R32 r) (M32 m) (I8 i) -> do
          pure $ set_imm8 i $ set_opsize32 $ set_rm_reg_mem r m $ mk_oc oc
        Ops3 (R64 r) (M64 m) (I8 i) -> do
          assert_mode64
          pure $ set_imm8 i $ set_opsize64 $ set_rm_reg_mem r m $ mk_oc oc
        _ -> Nothing

      -- handle reg:ModRM.reg, mem:ModRM.rm, imm cases
      --
      -- Opsize: 16, 32, 64
      handle_reg_mem_imm mk_oc oc = case args of
        Ops3 (R16 r) (M16 m) (I16 i) -> do
          pure $ set_imm16 i $ set_opsize16 $ set_rm_reg_mem r m $ mk_oc oc
        Ops3 (R32 r) (M32 m) (I32 i) -> do
          pure $ set_imm32 i $ set_opsize32 $ set_rm_reg_mem r m $ mk_oc oc
        Ops3 (R64 r) (M64 m) (I32 i) -> do
          assert_mode64
          pure $ set_imm32 i $ set_opsize64 $ set_rm_reg_mem r m $ mk_oc oc
        _ -> Nothing

      handle_ext_rm mk_oc oc ext = case args of
        Ops1 (M8  m) -> do
          pure $ set_rm_ext_mem ext m $ mk_oc oc
        Ops1 (M16 m) -> do
          pure $ set_opsize16 $ set_rm_ext_mem ext m $ mk_oc (oc+1)
        Ops1 (M32 m) -> do
          pure $ set_opsize32 $ set_rm_ext_mem ext m $ mk_oc (oc+1)
        Ops1 (M64 m) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_mem ext m $ mk_oc (oc+1)

        -- FIXME: Intel doc says that reg extension goes into REX.R. Is that true??
        -- ModRM.rm extension is supposed to go into REX.B
        Ops1 (R8 r) -> do
          pure $ set_rm_ext_reg ext r $ mk_oc oc
        Ops1 (R16 r) -> do
          pure $ set_opsize16 $ set_rm_ext_reg ext r $ mk_oc (oc+1)
        Ops1 (R32 r) -> do
          pure $ set_opsize32 $ set_rm_ext_reg ext r $ mk_oc (oc+1)
        Ops1 (R64 r) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_reg ext r $ mk_oc (oc+1)
        _ -> Nothing

      handle_rm_imm  mk_oc oc ext = handle_reg_imm  mk_oc oc ext <|> handle_mem_imm mk_oc oc ext
      handle_rm_imm8 mk_oc oc ext = handle_reg_imm8 mk_oc oc ext <|> handle_mem_imm8 mk_oc oc ext
      handle_rm_reg  mk_oc oc     = handle_regs_rm_reg mk_oc oc  <|> handle_mem_reg mk_oc oc
      handle_reg_rm  mk_oc oc     = handle_regs_reg_rm mk_oc oc  <|> handle_reg_mem mk_oc oc
      handle_reg_rm_i8 mk_oc oc   = handle_regs_reg_rm_i8 mk_oc oc <|> handle_reg_mem_i8 mk_oc oc
      handle_reg_rm_imm mk_oc oc  = handle_regs_reg_rm_imm mk_oc oc <|> handle_reg_mem_imm mk_oc oc

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
      i <- imm8_arg
      pure $ set_imm8 i $ primary 0xD5

    AAM -> do
      assert_not_mode64
      i <- imm8_arg
      pure $ set_imm8 i $ primary 0xD4

    DAA -> do
      assert_not_mode64
      assert_no_args
      pure $ primary 0x27

    DAS -> do
      assert_not_mode64
      assert_no_args
      pure $ primary 0x2F

    ADC -> asum
      [ handle_acc_imm  primary 0x14
      , handle_rm_imm   primary 0x80 0x2
      , handle_rm_imm8  primary 0x83 0x2
      , handle_rm_reg   primary 0x10
      , handle_reg_rm   primary 0x12
      ]

    ADD -> asum
      [ handle_acc_imm  primary 0x04
      , handle_rm_imm   primary 0x80 0x0
      , handle_rm_imm8  primary 0x83 0x0
      , handle_rm_reg   primary 0x00
      , handle_reg_rm   primary 0x02
      ]

    AND -> asum
      [ handle_acc_imm  primary 0x24
      , handle_rm_imm   primary 0x80 0x4
      , handle_rm_imm8  primary 0x83 0x4
      , handle_rm_reg   primary 0x20
      , handle_reg_rm   primary 0x22
      ]

    SUB -> asum
      [ handle_acc_imm  primary 0x2C
      , handle_rm_imm   primary 0x80 0x5
      , handle_rm_imm8  primary 0x83 0x5
      , handle_rm_reg   primary 0x28
      , handle_reg_rm   primary 0x2A
      ]

    SBB -> asum
      [ handle_acc_imm  primary 0x1C
      , handle_rm_imm   primary 0x80 0x3
      , handle_rm_imm8  primary 0x83 0x3
      , handle_rm_reg   primary 0x18
      , handle_reg_rm   primary 0x1A
      ]

    DEC -> asum
      [ case args of
          -- shorter forms, except in 64-bit mode (reused for REX prefixes)
          Ops1 (R16 r)
            | not mode64
            , (False,c) <- regCodeX r
            -> pure $ set_opsize16 $ primary (0x48 + c)
          Ops1 (R32 r)
            | not mode64
            , (False,c) <- regCodeX r
            -> pure $ set_opsize32 $ primary (0x48 + c)
          _ -> Nothing
      , handle_ext_rm primary 0xFE 0x1
      ]

    INC -> asum
      [ case args of
          -- shorter forms, except in 64-bit mode (reused for REX prefixes)
          Ops1 (R16 r)
            | not mode64
            , (False,c) <- regCodeX r
            -> pure $ set_opsize16 $ primary (0x40 + c)
          Ops1 (R32 r)
            | not mode64
            , (False,c) <- regCodeX r
            -> pure $ set_opsize32 $ primary (0x40 + c)
          _ -> Nothing
      , handle_ext_rm primary 0xFE 0x0
      ]

    NEG  -> handle_ext_rm primary 0xF6 0x3
    DIV  -> handle_ext_rm primary 0xF6 0x6
    MUL  -> handle_ext_rm primary 0xF6 0x4
    IDIV -> handle_ext_rm primary 0xF6 0x7
    IMUL -> asum
      [ handle_ext_rm     primary 0xF6 0x5
      , handle_reg_rm     map_0F  0xAF
      , handle_reg_rm_i8  primary 0x6B
      , handle_reg_rm_imm primary 0x69
      ]

    SXA -> case args of
      -- Intel uses CBW/CWDE/CDQE mnemonics to differentiate the operand size.
      -- This sucks. Let's use the destination register (AX,EAX,RAX) as an
      -- operand instead.
      Ops1 (OpReg R_AX)  -> pure $ set_opsize16 $ primary 0x98
      Ops1 (OpReg R_EAX) -> pure $ set_opsize32 $ primary 0x98
      Ops1 (OpReg R_RAX) -> pure $ set_opsize64 $ primary 0x98
      _                  -> Nothing

    SXAD -> case args of
      -- Intel uses CWD/CDQ/CQO mnemonics to differentiate the operand size.
      -- This sucks. Let's use the source/destination register (AX,EAX,RAX) as an
      -- operand instead.
      Ops1 (OpReg R_AX)  -> pure $ set_opsize16 $ primary 0x99
      Ops1 (OpReg R_EAX) -> pure $ set_opsize32 $ primary 0x99
      Ops1 (OpReg R_RAX) -> pure $ set_opsize64 $ primary 0x99
      _                  -> Nothing

    MOV -> asum
      [ handle_rm_reg primary 0x88
      , handle_reg_rm primary 0x8A
        -- shorter forms for reg,imm + imm64 form!
      , case args of
          Ops2 (R8  r) (I8  i) ->
            pure $ set_imm8 i $ set_oc_reg primary 0xB0 r
          Ops2 (R16 r) (I16 i) ->
            pure $ set_opsize16 $ set_imm16 i $ set_oc_reg primary 0xB8 r
          Ops2 (R32 r) (I32 i) ->
            pure $ set_opsize32 $ set_imm32 i $ set_oc_reg primary 0xB8 r
          Ops2 (R64 r) (I64 i) ->
            pure $ set_opsize64 $ set_imm64 i $ set_oc_reg primary 0xB8 r
          _ -> Nothing
      , handle_rm_imm primary 0xC6 0x0
      -- TODO: mov rm, sreg
      -- TODO: mov sreg, rm
      -- TODO: mov acc, moffs
      -- TODO: mov moffs, acc
      ]

    LEA -> case args of
      -- we don't care about the size of the targetted memory...
      Ops2 (R16 r) (OpMem m) -> do
        pure $ set_opsize16 $ set_rm_reg_mem r m $ primary 0x8D
      Ops2 (R32 r) (OpMem m) -> do
        pure $ set_opsize32 $ set_rm_reg_mem r m $ primary 0x8D
      Ops2 (R64 r) (OpMem m) -> do
        assert_mode64
        pure $ set_opsize64 $ set_rm_reg_mem r m $ primary 0x8D
      _ -> Nothing


    ADCX -> do
      has_extension ADX
      case args of
        Ops2 (R32 r) (M32 m) -> do
          pure $ set_rm_reg_mem r m $ map_66_0F38 0xF6
        Ops2 (R64 r) (M64 m) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_mem r m $ map_66_0F38 0xF6
        Ops2 (R32 r) (R32 rm) -> do
          pure $ set_rm_reg_reg r rm $ map_66_0F38 0xF6
        Ops2 (R64 r) (R64 rm) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_reg r rm $ map_66_0F38 0xF6
        _ -> Nothing

    ADOX -> do
      has_extension ADX
      case args of
        Ops2 (R32 r) (M32 m) -> do
          pure $ set_rm_reg_mem r m $ map_F3_0F38 0xF6
        Ops2 (R64 r) (M64 m) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_mem r m $ map_F3_0F38 0xF6
        Ops2 (R32 r) (R32 rm) -> do
          pure $ set_rm_reg_reg r rm $ map_F3_0F38 0xF6
        Ops2 (R64 r) (R64 rm) -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_reg r rm $ map_F3_0F38 0xF6
        _ -> Nothing

    ADDPD -> 
      case args of
        Ops2 (V128 v) (M128 m) -> do
          has_extension SSE2
          pure $ set_rm_vec_mem v m $ map_66_0F 0x58
        Ops2 (V128 v1) (V128 v2) -> do
          has_extension SSE2
          pure $ set_rm_vec_vec v1 v2 $ map_66_0F 0x58
        _ -> Nothing
