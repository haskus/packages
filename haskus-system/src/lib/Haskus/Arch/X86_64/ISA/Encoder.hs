{-# LANGUAGE OverloadedLists #-}

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
import Haskus.Arch.X86_64.ISA.Encoding.Segment
import Haskus.Arch.X86_64.ISA.Context
import qualified Haskus.Arch.X86_64.ISA.Extension as Ext
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
        [] -> Just ()
        _  -> Nothing

      imm8_arg = case args of
        [I8 x] -> Just x
        _      -> Nothing
      
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

      set_addrsize asz e
        | overriddenAddressSize False ctx == asz = Just e
        | overriddenAddressSize True  ctx == asz = Just $ e { encPrefixes = P_67 : encPrefixes e }
        | otherwise                              = Nothing

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

      -- store register code in REX.B:opcode
      set_oc_reg mk_oc oc r =
        let !(xc,c) = regCodeX r
            -- handle registers that require REX
            mrex1 = if regREX r then Just emptyRex else Nothing
            -- register code extension in REX.B
            mrex2 = if xc       then Just rexB     else Nothing
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
        [R8 R_AL,  I8  i] -> do
          pure $ set_imm8 i $ mk_oc oc
        [R16 R_AX,  I16 i] -> do
          pure $ set_opsize16 $ set_imm16 i $ mk_oc (oc+1)
        [R32 R_EAX, I32 i] -> do
          pure $ set_opsize32 $ set_imm32 i $ mk_oc (oc+1)
        [R64 R_RAX, I32 i] -> do
          assert_mode64
          pure $ set_opsize64 $ set_imm32 i $ mk_oc (oc+1)
        _ -> Nothing

      -- handle regN, immN cases (reg in ModRM.rm field)
      handle_reg_imm mk_oc oc ext = case args of
        [R8  r, I8  i] -> do
          pure $ set_rm_ext_reg ext r $ set_imm8 i $ mk_oc oc
        [R16 r, I16 i] -> do
          pure $ set_opsize16 $ set_rm_ext_reg ext r $ set_imm16 i $ mk_oc (oc+1)
        [R32 r, I32 i] -> do
          pure $ set_opsize32 $ set_rm_ext_reg ext r $ set_imm32 i $ mk_oc (oc+1)
        [R64 r, I32 i] -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_reg ext r $ set_imm32 i $ mk_oc (oc+1)
        _ -> Nothing

      -- handle regN, imm8 cases (reg in ModRM.rm field)
      handle_reg_imm8 mk_oc oc ext = case args of
        [R16 r, I8 i] -> do
          pure $ set_opsize16 $ set_rm_ext_reg ext r $ set_imm8 i $ mk_oc oc
        [R32 r, I8 i] -> do
          pure $ set_opsize32 $ set_rm_ext_reg ext r $ set_imm8 i $ mk_oc oc
        [R64 r, I8 i] -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_reg ext r $ set_imm8 i $ mk_oc oc
        _ -> Nothing

      -- handle memN, immN cases
      handle_mem_imm mk_oc oc ext = case args of
        [M8  m, I8  i] -> do
          pure $ set_rm_ext_mem ext m $ set_imm8 i $ mk_oc oc
        [M16 m, I16 i] -> do
          pure $ set_opsize16 $ set_rm_ext_mem ext m $ set_imm16 i $ mk_oc (oc+1)
        [M32 m, I32 i] -> do
          pure $ set_opsize32 $ set_rm_ext_mem ext m $ set_imm32 i $ mk_oc (oc+1)
        [M64 m, I32 i] -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_mem ext m $ set_imm32 i $ mk_oc (oc+1)
        _ -> Nothing

      -- handle memN, imm8 cases
      handle_mem_imm8 mk_oc oc ext = case args of
        [M16 m, I8 i] -> do
          pure $ set_opsize16 $ set_rm_ext_mem ext m $ set_imm8 i $ mk_oc oc
        [M32 m, I8 i] -> do
          pure $ set_opsize32 $ set_rm_ext_mem ext m $ set_imm8 i $ mk_oc oc
        [M64 m, I8 i] -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_mem ext m $ set_imm8 i $ mk_oc oc
        _ -> Nothing

      -- handle reg:ModRM.rm, reg:ModRM.reg cases
      handle_regs_rm_reg mk_oc oc = case args of
        [R8  r1, R8  r2] -> do
          pure $ set_mr_reg_reg r1 r2 $ mk_oc oc
        [R16 r1, R16 r2] -> do
          pure $ set_opsize16 $ set_mr_reg_reg r1 r2 $ mk_oc (oc+1)
        [R32 r1, R32 r2] -> do
          pure $ set_opsize32 $ set_mr_reg_reg r1 r2 $ mk_oc (oc+1)
        [R64 r1, R64 r2] -> do
          assert_mode64
          pure $ set_opsize64 $ set_mr_reg_reg r1 r2 $ mk_oc (oc+1)
        _ -> Nothing

      -- handle reg:ModRM.reg, reg:ModRM.rm cases
      handle_regs_reg_rm mk_oc oc = case args of
        [R8 r1, R8 r2] -> do
          pure $ set_rm_reg_reg r1 r2 $ mk_oc oc
        [R16 r1, R16 r2] -> do
          pure $ set_opsize16 $ set_rm_reg_reg r1 r2 $ mk_oc (oc+1)
        [R32 r1, R32 r2] -> do
          pure $ set_opsize32 $ set_rm_reg_reg r1 r2 $ mk_oc (oc+1)
        [R64 r1, R64 r2] -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_reg r1 r2 $ mk_oc (oc+1)
        _ -> Nothing
      
      -- handle reg:ModRM.reg, reg:ModRM.rm, i8 cases
      --
      -- Opsize: 16, 32, 64
      handle_regs_reg_rm_i8 mk_oc oc = case args of
        [R16 r1, R16 r2, I8 i] -> do
          pure $ set_imm8 i $ set_opsize16 $ set_rm_reg_reg r1 r2 $ mk_oc oc
        [R32 r1, R32 r2, I8 i] -> do
          pure $ set_imm8 i $ set_opsize32 $ set_rm_reg_reg r1 r2 $ mk_oc oc
        [R64 r1, R64 r2, I8 i] -> do
          assert_mode64
          pure $ set_imm8 i $ set_opsize64 $ set_rm_reg_reg r1 r2 $ mk_oc oc
        _ -> Nothing

      -- handle reg:ModRM.reg, reg:ModRM.rm, imm cases
      --
      -- Opsize: 16, 32, 64
      handle_regs_reg_rm_imm mk_oc oc = case args of
        [R16 r1, R16 r2, I16 i] -> do
          pure $ set_imm16 i $ set_opsize16 $ set_rm_reg_reg r1 r2 $ mk_oc oc
        [R32 r1, R32 r2, I32 i] -> do
          pure $ set_imm32 i $ set_opsize32 $ set_rm_reg_reg r1 r2 $ mk_oc oc
        [R64 r1, R64 r2, I32 i] -> do
          assert_mode64
          pure $ set_imm32 i $ set_opsize64 $ set_rm_reg_reg r1 r2 $ mk_oc oc
        _ -> Nothing

      -- handle mem:ModRM.rm, reg:ModRM.reg cases
      handle_mem_reg mk_oc oc = case args of
        [M8 m,  R8   r] -> do
          pure $ set_rm_reg_mem r m $ mk_oc oc
        [M16 m, R16 r] -> do
          pure $ set_opsize16 $ set_rm_reg_mem r m $ mk_oc (oc+1)
        [M32 m, R32 r] -> do
          pure $ set_opsize32 $ set_rm_reg_mem r m $ mk_oc (oc+1)
        [M64 m, R64 r] -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_mem r m $ mk_oc (oc+1)
        _ -> Nothing

      -- handle reg:ModRM.reg, mem:ModRM.rm cases
      handle_reg_mem mk_oc oc = case args of
        [R8  r, M8  m] -> do
          pure $ set_rm_reg_mem r m $ mk_oc oc
        [R16 r, M16 m] -> do
          pure $ set_opsize16 $ set_rm_reg_mem r m $ mk_oc (oc+1)
        [R32 r, M32 m] -> do
          pure $ set_opsize32 $ set_rm_reg_mem r m $ mk_oc (oc+1)
        [R64 r, M64 m] -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_mem r m $ mk_oc (oc+1)
        _ -> Nothing

      -- handle reg:ModRM.reg, mem:ModRM.rm, i8 cases
      --
      -- Opsize: 16, 32, 64
      handle_reg_mem_i8 mk_oc oc = case args of
        [R16 r, M16 m, I8 i] -> do
          pure $ set_imm8 i $ set_opsize16 $ set_rm_reg_mem r m $ mk_oc oc
        [R32 r, M32 m, I8 i] -> do
          pure $ set_imm8 i $ set_opsize32 $ set_rm_reg_mem r m $ mk_oc oc
        [R64 r, M64 m, I8 i] -> do
          assert_mode64
          pure $ set_imm8 i $ set_opsize64 $ set_rm_reg_mem r m $ mk_oc oc
        _ -> Nothing

      -- handle reg:ModRM.reg, mem:ModRM.rm, imm cases
      --
      -- Opsize: 16, 32, 64
      handle_reg_mem_imm mk_oc oc = case args of
        [R16 r, M16 m, I16 i] -> do
          pure $ set_imm16 i $ set_opsize16 $ set_rm_reg_mem r m $ mk_oc oc
        [R32 r, M32 m, I32 i] -> do
          pure $ set_imm32 i $ set_opsize32 $ set_rm_reg_mem r m $ mk_oc oc
        [R64 r, M64 m, I32 i] -> do
          assert_mode64
          pure $ set_imm32 i $ set_opsize64 $ set_rm_reg_mem r m $ mk_oc oc
        _ -> Nothing

      handle_ext_rm mk_oc oc ext = case args of
        [M8  m] -> do
          pure $ set_rm_ext_mem ext m $ mk_oc oc
        [M16 m] -> do
          pure $ set_opsize16 $ set_rm_ext_mem ext m $ mk_oc (oc+1)
        [M32 m] -> do
          pure $ set_opsize32 $ set_rm_ext_mem ext m $ mk_oc (oc+1)
        [M64 m] -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_mem ext m $ mk_oc (oc+1)

        -- FIXME: Intel doc says that reg extension goes into REX.R. Is that true??
        -- ModRM.rm extension is supposed to go into REX.B
        [R8 r] -> do
          pure $ set_rm_ext_reg ext r $ mk_oc oc
        [R16 r] -> do
          pure $ set_opsize16 $ set_rm_ext_reg ext r $ mk_oc (oc+1)
        [R32 r] -> do
          pure $ set_opsize32 $ set_rm_ext_reg ext r $ mk_oc (oc+1)
        [R64 r] -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_ext_reg ext r $ mk_oc (oc+1)
        _ -> Nothing

      handle_rm_imm  mk_oc oc ext = handle_reg_imm  mk_oc oc ext <|> handle_mem_imm mk_oc oc ext
      handle_rm_imm8 mk_oc oc ext = handle_reg_imm8 mk_oc oc ext <|> handle_mem_imm8 mk_oc oc ext
      handle_rm_reg  mk_oc oc     = handle_regs_rm_reg mk_oc oc  <|> handle_mem_reg mk_oc oc
      handle_reg_rm  mk_oc oc     = handle_regs_reg_rm mk_oc oc  <|> handle_reg_mem mk_oc oc
      handle_reg_rm_i8 mk_oc oc   = handle_regs_reg_rm_i8 mk_oc oc <|> handle_reg_mem_i8 mk_oc oc
      handle_reg_rm_imm mk_oc oc  = handle_regs_reg_rm_imm mk_oc oc <|> handle_reg_mem_imm mk_oc oc

      alts :: [Maybe a] -> Maybe a
      alts = asum

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

    ADC -> alts
      [ handle_acc_imm  primary 0x14
      , handle_rm_imm   primary 0x80 0x2
      , handle_rm_imm8  primary 0x83 0x2
      , handle_rm_reg   primary 0x10
      , handle_reg_rm   primary 0x12
      ]

    ADD -> alts
      [ handle_acc_imm  primary 0x04
      , handle_rm_imm   primary 0x80 0x0
      , handle_rm_imm8  primary 0x83 0x0
      , handle_rm_reg   primary 0x00
      , handle_reg_rm   primary 0x02
      ]

    TEST -> alts
      [ handle_acc_imm  primary 0xA8
      , handle_rm_imm   primary 0xF6 0x0
      , handle_rm_reg   primary 0x84
      ]

    CMP -> alts
      [ handle_acc_imm  primary 0x3C
      , handle_rm_imm   primary 0x80 0x7
      , handle_rm_imm8  primary 0x83 0x7
      , handle_rm_reg   primary 0x38
      , handle_reg_rm   primary 0x3A
      ]

    CMPXCHG -> case args of
      [R8  rm, R8  r] -> pure $ set_rm_reg_reg r rm $ map_0F 0xB0
      [M8  rm, R8  r] -> pure $ set_rm_reg_mem r rm $ map_0F 0xB0
      [R16 rm, R16 r] -> pure $ set_opsize16 $ set_rm_reg_reg r rm $ map_0F 0xB1
      [M16 rm, R16 r] -> pure $ set_opsize16 $ set_rm_reg_mem r rm $ map_0F 0xB1
      [R32 rm, R32 r] -> pure $ set_opsize32 $ set_rm_reg_reg r rm $ map_0F 0xB1
      [M32 rm, R32 r] -> pure $ set_opsize32 $ set_rm_reg_mem r rm $ map_0F 0xB1
      [R64 rm, R64 r] -> pure $ set_opsize64 $ set_rm_reg_reg r rm $ map_0F 0xB1
      [M64 rm, R64 r] -> pure $ set_opsize64 $ set_rm_reg_mem r rm $ map_0F 0xB1
      _ -> Nothing

    CMPXCHGB -> case args of
      [M64  m] -> pure $                set_rm_ext_mem 0x1 m $ map_0F 0xC7
      [M128 m] -> pure $ set_opsize64 $ set_rm_ext_mem 0x1 m $ map_0F 0xC7
      _ -> Nothing

    AND -> alts
      [ handle_acc_imm  primary 0x24
      , handle_rm_imm   primary 0x80 0x4
      , handle_rm_imm8  primary 0x83 0x4
      , handle_rm_reg   primary 0x20
      , handle_reg_rm   primary 0x22
      ]

    OR -> alts
      [ handle_acc_imm  primary 0x0C
      , handle_rm_imm   primary 0x80 0x1
      , handle_rm_imm8  primary 0x83 0x1
      , handle_rm_reg   primary 0x08
      , handle_reg_rm   primary 0x0A
      ]

    XOR -> alts
      [ handle_acc_imm  primary 0x34
      , handle_rm_imm   primary 0x80 0x6
      , handle_rm_imm8  primary 0x83 0x6
      , handle_rm_reg   primary 0x30
      , handle_reg_rm   primary 0x32
      ]

    NOT -> case args of
      [R8  r] -> pure $ set_rm_ext_reg 0x2 r $ primary 0xF6
      [M8  m] -> pure $ set_rm_ext_mem 0x2 m $ primary 0xF6
      [R16 r] -> pure $ set_opsize16 $ set_rm_ext_reg 0x2 r $ primary 0xF7
      [R32 r] -> pure $ set_opsize32 $ set_rm_ext_reg 0x2 r $ primary 0xF7
      [R64 r] -> pure $ set_opsize64 $ set_rm_ext_reg 0x2 r $ primary 0xF7
      [M16 m] -> pure $ set_opsize16 $ set_rm_ext_mem 0x2 m $ primary 0xF7
      [M32 m] -> pure $ set_opsize32 $ set_rm_ext_mem 0x2 m $ primary 0xF7
      [M64 m] -> pure $ set_opsize64 $ set_rm_ext_mem 0x2 m $ primary 0xF7
      _ -> Nothing

    SHL -> case args of
      [R8  r, I8 1] -> pure $                set_rm_ext_reg 0x4 r $ primary 0xD0
      [R16 r, I8 1] -> pure $ set_opsize16 $ set_rm_ext_reg 0x4 r $ primary 0xD1
      [R32 r, I8 1] -> pure $ set_opsize32 $ set_rm_ext_reg 0x4 r $ primary 0xD1
      [R64 r, I8 1] -> pure $ set_opsize64 $ set_rm_ext_reg 0x4 r $ primary 0xD1

      [M8  m, I8 1] -> pure $                set_rm_ext_mem 0x4 m $ primary 0xD0
      [M16 m, I8 1] -> pure $ set_opsize16 $ set_rm_ext_mem 0x4 m $ primary 0xD1
      [M32 m, I8 1] -> pure $ set_opsize32 $ set_rm_ext_mem 0x4 m $ primary 0xD1
      [M64 m, I8 1] -> pure $ set_opsize64 $ set_rm_ext_mem 0x4 m $ primary 0xD1

      [R8  r, I8 i] -> pure $                set_rm_ext_reg 0x4 r $ set_imm8 i $ primary 0xC0
      [R16 r, I8 i] -> pure $ set_opsize16 $ set_rm_ext_reg 0x4 r $ set_imm8 i $ primary 0xC1
      [R32 r, I8 i] -> pure $ set_opsize32 $ set_rm_ext_reg 0x4 r $ set_imm8 i $ primary 0xC1
      [R64 r, I8 i] -> pure $ set_opsize64 $ set_rm_ext_reg 0x4 r $ set_imm8 i $ primary 0xC1

      [M8  m, I8 i] -> pure $                set_rm_ext_mem 0x4 m $ set_imm8 i $ primary 0xC0
      [M16 m, I8 i] -> pure $ set_opsize16 $ set_rm_ext_mem 0x4 m $ set_imm8 i $ primary 0xC1
      [M32 m, I8 i] -> pure $ set_opsize32 $ set_rm_ext_mem 0x4 m $ set_imm8 i $ primary 0xC1
      [M64 m, I8 i] -> pure $ set_opsize64 $ set_rm_ext_mem 0x4 m $ set_imm8 i $ primary 0xC1

      [R8  r, R8 R_CL] -> pure $                set_rm_ext_reg 0x4 r $ primary 0xD2
      [R16 r, R8 R_CL] -> pure $ set_opsize16 $ set_rm_ext_reg 0x4 r $ primary 0xD3
      [R32 r, R8 R_CL] -> pure $ set_opsize32 $ set_rm_ext_reg 0x4 r $ primary 0xD3
      [R64 r, R8 R_CL] -> pure $ set_opsize64 $ set_rm_ext_reg 0x4 r $ primary 0xD3

      [M8  m, R8 R_CL] -> pure $                set_rm_ext_mem 0x4 m $ primary 0xD2
      [M16 m, R8 R_CL] -> pure $ set_opsize16 $ set_rm_ext_mem 0x4 m $ primary 0xD3
      [M32 m, R8 R_CL] -> pure $ set_opsize32 $ set_rm_ext_mem 0x4 m $ primary 0xD3
      [M64 m, R8 R_CL] -> pure $ set_opsize64 $ set_rm_ext_mem 0x4 m $ primary 0xD3
      _ -> Nothing

    SAR -> case args of
      [R8  r, I8 1] -> pure $                set_rm_ext_reg 0x7 r $ primary 0xD0
      [R16 r, I8 1] -> pure $ set_opsize16 $ set_rm_ext_reg 0x7 r $ primary 0xD1
      [R32 r, I8 1] -> pure $ set_opsize32 $ set_rm_ext_reg 0x7 r $ primary 0xD1
      [R64 r, I8 1] -> pure $ set_opsize64 $ set_rm_ext_reg 0x7 r $ primary 0xD1

      [M8  m, I8 1] -> pure $                set_rm_ext_mem 0x7 m $ primary 0xD0
      [M16 m, I8 1] -> pure $ set_opsize16 $ set_rm_ext_mem 0x7 m $ primary 0xD1
      [M32 m, I8 1] -> pure $ set_opsize32 $ set_rm_ext_mem 0x7 m $ primary 0xD1
      [M64 m, I8 1] -> pure $ set_opsize64 $ set_rm_ext_mem 0x7 m $ primary 0xD1

      [R8  r, I8 i] -> pure $                set_rm_ext_reg 0x7 r $ set_imm8 i $ primary 0xC0
      [R16 r, I8 i] -> pure $ set_opsize16 $ set_rm_ext_reg 0x7 r $ set_imm8 i $ primary 0xC1
      [R32 r, I8 i] -> pure $ set_opsize32 $ set_rm_ext_reg 0x7 r $ set_imm8 i $ primary 0xC1
      [R64 r, I8 i] -> pure $ set_opsize64 $ set_rm_ext_reg 0x7 r $ set_imm8 i $ primary 0xC1

      [M8  m, I8 i] -> pure $                set_rm_ext_mem 0x7 m $ set_imm8 i $ primary 0xC0
      [M16 m, I8 i] -> pure $ set_opsize16 $ set_rm_ext_mem 0x7 m $ set_imm8 i $ primary 0xC1
      [M32 m, I8 i] -> pure $ set_opsize32 $ set_rm_ext_mem 0x7 m $ set_imm8 i $ primary 0xC1
      [M64 m, I8 i] -> pure $ set_opsize64 $ set_rm_ext_mem 0x7 m $ set_imm8 i $ primary 0xC1

      [R8  r, R8 R_CL] -> pure $                set_rm_ext_reg 0x7 r $ primary 0xD2
      [R16 r, R8 R_CL] -> pure $ set_opsize16 $ set_rm_ext_reg 0x7 r $ primary 0xD3
      [R32 r, R8 R_CL] -> pure $ set_opsize32 $ set_rm_ext_reg 0x7 r $ primary 0xD3
      [R64 r, R8 R_CL] -> pure $ set_opsize64 $ set_rm_ext_reg 0x7 r $ primary 0xD3

      [M8  m, R8 R_CL] -> pure $                set_rm_ext_mem 0x7 m $ primary 0xD2
      [M16 m, R8 R_CL] -> pure $ set_opsize16 $ set_rm_ext_mem 0x7 m $ primary 0xD3
      [M32 m, R8 R_CL] -> pure $ set_opsize32 $ set_rm_ext_mem 0x7 m $ primary 0xD3
      [M64 m, R8 R_CL] -> pure $ set_opsize64 $ set_rm_ext_mem 0x7 m $ primary 0xD3
      _ -> Nothing

    SHR -> case args of
      [R8  r, I8 1] -> pure $                set_rm_ext_reg 0x5 r $ primary 0xD0
      [R16 r, I8 1] -> pure $ set_opsize16 $ set_rm_ext_reg 0x5 r $ primary 0xD1
      [R32 r, I8 1] -> pure $ set_opsize32 $ set_rm_ext_reg 0x5 r $ primary 0xD1
      [R64 r, I8 1] -> pure $ set_opsize64 $ set_rm_ext_reg 0x5 r $ primary 0xD1

      [M8  m, I8 1] -> pure $                set_rm_ext_mem 0x5 m $ primary 0xD0
      [M16 m, I8 1] -> pure $ set_opsize16 $ set_rm_ext_mem 0x5 m $ primary 0xD1
      [M32 m, I8 1] -> pure $ set_opsize32 $ set_rm_ext_mem 0x5 m $ primary 0xD1
      [M64 m, I8 1] -> pure $ set_opsize64 $ set_rm_ext_mem 0x5 m $ primary 0xD1

      [R8  r, I8 i] -> pure $                set_rm_ext_reg 0x5 r $ set_imm8 i $ primary 0xC0
      [R16 r, I8 i] -> pure $ set_opsize16 $ set_rm_ext_reg 0x5 r $ set_imm8 i $ primary 0xC1
      [R32 r, I8 i] -> pure $ set_opsize32 $ set_rm_ext_reg 0x5 r $ set_imm8 i $ primary 0xC1
      [R64 r, I8 i] -> pure $ set_opsize64 $ set_rm_ext_reg 0x5 r $ set_imm8 i $ primary 0xC1

      [M8  m, I8 i] -> pure $                set_rm_ext_mem 0x5 m $ set_imm8 i $ primary 0xC0
      [M16 m, I8 i] -> pure $ set_opsize16 $ set_rm_ext_mem 0x5 m $ set_imm8 i $ primary 0xC1
      [M32 m, I8 i] -> pure $ set_opsize32 $ set_rm_ext_mem 0x5 m $ set_imm8 i $ primary 0xC1
      [M64 m, I8 i] -> pure $ set_opsize64 $ set_rm_ext_mem 0x5 m $ set_imm8 i $ primary 0xC1

      [R8  r, R8 R_CL] -> pure $                set_rm_ext_reg 0x5 r $ primary 0xD2
      [R16 r, R8 R_CL] -> pure $ set_opsize16 $ set_rm_ext_reg 0x5 r $ primary 0xD3
      [R32 r, R8 R_CL] -> pure $ set_opsize32 $ set_rm_ext_reg 0x5 r $ primary 0xD3
      [R64 r, R8 R_CL] -> pure $ set_opsize64 $ set_rm_ext_reg 0x5 r $ primary 0xD3

      [M8  m, R8 R_CL] -> pure $                set_rm_ext_mem 0x5 m $ primary 0xD2
      [M16 m, R8 R_CL] -> pure $ set_opsize16 $ set_rm_ext_mem 0x5 m $ primary 0xD3
      [M32 m, R8 R_CL] -> pure $ set_opsize32 $ set_rm_ext_mem 0x5 m $ primary 0xD3
      [M64 m, R8 R_CL] -> pure $ set_opsize64 $ set_rm_ext_mem 0x5 m $ primary 0xD3
      _ -> Nothing

    ROL -> case args of
      [R8  r, I8 1] -> pure $                set_rm_ext_reg 0x0 r $ primary 0xD0
      [R16 r, I8 1] -> pure $ set_opsize16 $ set_rm_ext_reg 0x0 r $ primary 0xD1
      [R32 r, I8 1] -> pure $ set_opsize32 $ set_rm_ext_reg 0x0 r $ primary 0xD1
      [R64 r, I8 1] -> pure $ set_opsize64 $ set_rm_ext_reg 0x0 r $ primary 0xD1

      [M8  m, I8 1] -> pure $                set_rm_ext_mem 0x0 m $ primary 0xD0
      [M16 m, I8 1] -> pure $ set_opsize16 $ set_rm_ext_mem 0x0 m $ primary 0xD1
      [M32 m, I8 1] -> pure $ set_opsize32 $ set_rm_ext_mem 0x0 m $ primary 0xD1
      [M64 m, I8 1] -> pure $ set_opsize64 $ set_rm_ext_mem 0x0 m $ primary 0xD1

      [R8  r, I8 i] -> pure $                set_rm_ext_reg 0x0 r $ set_imm8 i $ primary 0xC0
      [R16 r, I8 i] -> pure $ set_opsize16 $ set_rm_ext_reg 0x0 r $ set_imm8 i $ primary 0xC1
      [R32 r, I8 i] -> pure $ set_opsize32 $ set_rm_ext_reg 0x0 r $ set_imm8 i $ primary 0xC1
      [R64 r, I8 i] -> pure $ set_opsize64 $ set_rm_ext_reg 0x0 r $ set_imm8 i $ primary 0xC1

      [M8  m, I8 i] -> pure $                set_rm_ext_mem 0x0 m $ set_imm8 i $ primary 0xC0
      [M16 m, I8 i] -> pure $ set_opsize16 $ set_rm_ext_mem 0x0 m $ set_imm8 i $ primary 0xC1
      [M32 m, I8 i] -> pure $ set_opsize32 $ set_rm_ext_mem 0x0 m $ set_imm8 i $ primary 0xC1
      [M64 m, I8 i] -> pure $ set_opsize64 $ set_rm_ext_mem 0x0 m $ set_imm8 i $ primary 0xC1

      [R8  r, R8 R_CL] -> pure $                set_rm_ext_reg 0x0 r $ primary 0xD2
      [R16 r, R8 R_CL] -> pure $ set_opsize16 $ set_rm_ext_reg 0x0 r $ primary 0xD3
      [R32 r, R8 R_CL] -> pure $ set_opsize32 $ set_rm_ext_reg 0x0 r $ primary 0xD3
      [R64 r, R8 R_CL] -> pure $ set_opsize64 $ set_rm_ext_reg 0x0 r $ primary 0xD3

      [M8  m, R8 R_CL] -> pure $                set_rm_ext_mem 0x0 m $ primary 0xD2
      [M16 m, R8 R_CL] -> pure $ set_opsize16 $ set_rm_ext_mem 0x0 m $ primary 0xD3
      [M32 m, R8 R_CL] -> pure $ set_opsize32 $ set_rm_ext_mem 0x0 m $ primary 0xD3
      [M64 m, R8 R_CL] -> pure $ set_opsize64 $ set_rm_ext_mem 0x0 m $ primary 0xD3
      _ -> Nothing

    ROR -> case args of
      [R8  r, I8 1] -> pure $                set_rm_ext_reg 0x1 r $ primary 0xD0
      [R16 r, I8 1] -> pure $ set_opsize16 $ set_rm_ext_reg 0x1 r $ primary 0xD1
      [R32 r, I8 1] -> pure $ set_opsize32 $ set_rm_ext_reg 0x1 r $ primary 0xD1
      [R64 r, I8 1] -> pure $ set_opsize64 $ set_rm_ext_reg 0x1 r $ primary 0xD1

      [M8  m, I8 1] -> pure $                set_rm_ext_mem 0x1 m $ primary 0xD0
      [M16 m, I8 1] -> pure $ set_opsize16 $ set_rm_ext_mem 0x1 m $ primary 0xD1
      [M32 m, I8 1] -> pure $ set_opsize32 $ set_rm_ext_mem 0x1 m $ primary 0xD1
      [M64 m, I8 1] -> pure $ set_opsize64 $ set_rm_ext_mem 0x1 m $ primary 0xD1

      [R8  r, I8 i] -> pure $                set_rm_ext_reg 0x1 r $ set_imm8 i $ primary 0xC0
      [R16 r, I8 i] -> pure $ set_opsize16 $ set_rm_ext_reg 0x1 r $ set_imm8 i $ primary 0xC1
      [R32 r, I8 i] -> pure $ set_opsize32 $ set_rm_ext_reg 0x1 r $ set_imm8 i $ primary 0xC1
      [R64 r, I8 i] -> pure $ set_opsize64 $ set_rm_ext_reg 0x1 r $ set_imm8 i $ primary 0xC1

      [M8  m, I8 i] -> pure $                set_rm_ext_mem 0x1 m $ set_imm8 i $ primary 0xC0
      [M16 m, I8 i] -> pure $ set_opsize16 $ set_rm_ext_mem 0x1 m $ set_imm8 i $ primary 0xC1
      [M32 m, I8 i] -> pure $ set_opsize32 $ set_rm_ext_mem 0x1 m $ set_imm8 i $ primary 0xC1
      [M64 m, I8 i] -> pure $ set_opsize64 $ set_rm_ext_mem 0x1 m $ set_imm8 i $ primary 0xC1

      [R8  r, R8 R_CL] -> pure $                set_rm_ext_reg 0x1 r $ primary 0xD2
      [R16 r, R8 R_CL] -> pure $ set_opsize16 $ set_rm_ext_reg 0x1 r $ primary 0xD3
      [R32 r, R8 R_CL] -> pure $ set_opsize32 $ set_rm_ext_reg 0x1 r $ primary 0xD3
      [R64 r, R8 R_CL] -> pure $ set_opsize64 $ set_rm_ext_reg 0x1 r $ primary 0xD3

      [M8  m, R8 R_CL] -> pure $                set_rm_ext_mem 0x1 m $ primary 0xD2
      [M16 m, R8 R_CL] -> pure $ set_opsize16 $ set_rm_ext_mem 0x1 m $ primary 0xD3
      [M32 m, R8 R_CL] -> pure $ set_opsize32 $ set_rm_ext_mem 0x1 m $ primary 0xD3
      [M64 m, R8 R_CL] -> pure $ set_opsize64 $ set_rm_ext_mem 0x1 m $ primary 0xD3
      _ -> Nothing

    RCL -> case args of
      [R8  r, I8 1] -> pure $                set_rm_ext_reg 0x2 r $ primary 0xD0
      [R16 r, I8 1] -> pure $ set_opsize16 $ set_rm_ext_reg 0x2 r $ primary 0xD1
      [R32 r, I8 1] -> pure $ set_opsize32 $ set_rm_ext_reg 0x2 r $ primary 0xD1
      [R64 r, I8 1] -> pure $ set_opsize64 $ set_rm_ext_reg 0x2 r $ primary 0xD1

      [M8  m, I8 1] -> pure $                set_rm_ext_mem 0x2 m $ primary 0xD0
      [M16 m, I8 1] -> pure $ set_opsize16 $ set_rm_ext_mem 0x2 m $ primary 0xD1
      [M32 m, I8 1] -> pure $ set_opsize32 $ set_rm_ext_mem 0x2 m $ primary 0xD1
      [M64 m, I8 1] -> pure $ set_opsize64 $ set_rm_ext_mem 0x2 m $ primary 0xD1

      [R8  r, I8 i] -> pure $                set_rm_ext_reg 0x2 r $ set_imm8 i $ primary 0xC0
      [R16 r, I8 i] -> pure $ set_opsize16 $ set_rm_ext_reg 0x2 r $ set_imm8 i $ primary 0xC1
      [R32 r, I8 i] -> pure $ set_opsize32 $ set_rm_ext_reg 0x2 r $ set_imm8 i $ primary 0xC1
      [R64 r, I8 i] -> pure $ set_opsize64 $ set_rm_ext_reg 0x2 r $ set_imm8 i $ primary 0xC1

      [M8  m, I8 i] -> pure $                set_rm_ext_mem 0x2 m $ set_imm8 i $ primary 0xC0
      [M16 m, I8 i] -> pure $ set_opsize16 $ set_rm_ext_mem 0x2 m $ set_imm8 i $ primary 0xC1
      [M32 m, I8 i] -> pure $ set_opsize32 $ set_rm_ext_mem 0x2 m $ set_imm8 i $ primary 0xC1
      [M64 m, I8 i] -> pure $ set_opsize64 $ set_rm_ext_mem 0x2 m $ set_imm8 i $ primary 0xC1

      [R8  r, R8 R_CL] -> pure $                set_rm_ext_reg 0x2 r $ primary 0xD2
      [R16 r, R8 R_CL] -> pure $ set_opsize16 $ set_rm_ext_reg 0x2 r $ primary 0xD3
      [R32 r, R8 R_CL] -> pure $ set_opsize32 $ set_rm_ext_reg 0x2 r $ primary 0xD3
      [R64 r, R8 R_CL] -> pure $ set_opsize64 $ set_rm_ext_reg 0x2 r $ primary 0xD3

      [M8  m, R8 R_CL] -> pure $                set_rm_ext_mem 0x2 m $ primary 0xD2
      [M16 m, R8 R_CL] -> pure $ set_opsize16 $ set_rm_ext_mem 0x2 m $ primary 0xD3
      [M32 m, R8 R_CL] -> pure $ set_opsize32 $ set_rm_ext_mem 0x2 m $ primary 0xD3
      [M64 m, R8 R_CL] -> pure $ set_opsize64 $ set_rm_ext_mem 0x2 m $ primary 0xD3
      _ -> Nothing

    RCR -> case args of
      [R8  r, I8 1] -> pure $                set_rm_ext_reg 0x3 r $ primary 0xD0
      [R16 r, I8 1] -> pure $ set_opsize16 $ set_rm_ext_reg 0x3 r $ primary 0xD1
      [R32 r, I8 1] -> pure $ set_opsize32 $ set_rm_ext_reg 0x3 r $ primary 0xD1
      [R64 r, I8 1] -> pure $ set_opsize64 $ set_rm_ext_reg 0x3 r $ primary 0xD1

      [M8  m, I8 1] -> pure $                set_rm_ext_mem 0x3 m $ primary 0xD0
      [M16 m, I8 1] -> pure $ set_opsize16 $ set_rm_ext_mem 0x3 m $ primary 0xD1
      [M32 m, I8 1] -> pure $ set_opsize32 $ set_rm_ext_mem 0x3 m $ primary 0xD1
      [M64 m, I8 1] -> pure $ set_opsize64 $ set_rm_ext_mem 0x3 m $ primary 0xD1

      [R8  r, I8 i] -> pure $                set_rm_ext_reg 0x3 r $ set_imm8 i $ primary 0xC0
      [R16 r, I8 i] -> pure $ set_opsize16 $ set_rm_ext_reg 0x3 r $ set_imm8 i $ primary 0xC1
      [R32 r, I8 i] -> pure $ set_opsize32 $ set_rm_ext_reg 0x3 r $ set_imm8 i $ primary 0xC1
      [R64 r, I8 i] -> pure $ set_opsize64 $ set_rm_ext_reg 0x3 r $ set_imm8 i $ primary 0xC1

      [M8  m, I8 i] -> pure $                set_rm_ext_mem 0x3 m $ set_imm8 i $ primary 0xC0
      [M16 m, I8 i] -> pure $ set_opsize16 $ set_rm_ext_mem 0x3 m $ set_imm8 i $ primary 0xC1
      [M32 m, I8 i] -> pure $ set_opsize32 $ set_rm_ext_mem 0x3 m $ set_imm8 i $ primary 0xC1
      [M64 m, I8 i] -> pure $ set_opsize64 $ set_rm_ext_mem 0x3 m $ set_imm8 i $ primary 0xC1

      [R8  r, R8 R_CL] -> pure $                set_rm_ext_reg 0x3 r $ primary 0xD2
      [R16 r, R8 R_CL] -> pure $ set_opsize16 $ set_rm_ext_reg 0x3 r $ primary 0xD3
      [R32 r, R8 R_CL] -> pure $ set_opsize32 $ set_rm_ext_reg 0x3 r $ primary 0xD3
      [R64 r, R8 R_CL] -> pure $ set_opsize64 $ set_rm_ext_reg 0x3 r $ primary 0xD3

      [M8  m, R8 R_CL] -> pure $                set_rm_ext_mem 0x3 m $ primary 0xD2
      [M16 m, R8 R_CL] -> pure $ set_opsize16 $ set_rm_ext_mem 0x3 m $ primary 0xD3
      [M32 m, R8 R_CL] -> pure $ set_opsize32 $ set_rm_ext_mem 0x3 m $ primary 0xD3
      [M64 m, R8 R_CL] -> pure $ set_opsize64 $ set_rm_ext_mem 0x3 m $ primary 0xD3
      _ -> Nothing

    BSWAP -> do
      -- TODO: not allowed before 486
      case args of
        [R32 r] -> pure $ set_opsize32 $ set_oc_reg map_0F 0xC8 r
        [R64 r] -> pure $ set_opsize64 $ set_oc_reg map_0F 0xC8 r
        _       -> Nothing

    SUB -> alts
      [ handle_acc_imm  primary 0x2C
      , handle_rm_imm   primary 0x80 0x5
      , handle_rm_imm8  primary 0x83 0x5
      , handle_rm_reg   primary 0x28
      , handle_reg_rm   primary 0x2A
      ]

    SBB -> alts
      [ handle_acc_imm  primary 0x1C
      , handle_rm_imm   primary 0x80 0x3
      , handle_rm_imm8  primary 0x83 0x3
      , handle_rm_reg   primary 0x18
      , handle_reg_rm   primary 0x1A
      ]

    DEC -> alts
      [ case args of
          -- shorter forms, except in 64-bit mode (reused for REX prefixes)
          [R16 r]
            | not mode64
            , (False,c) <- regCodeX r
            -> pure $ set_opsize16 $ primary (0x48 + c)
          [R32 r]
            | not mode64
            , (False,c) <- regCodeX r
            -> pure $ set_opsize32 $ primary (0x48 + c)
          _ -> Nothing
      , handle_ext_rm primary 0xFE 0x1
      ]

    INC -> alts
      [ case args of
          -- shorter forms, except in 64-bit mode (reused for REX prefixes)
          [R16 r]
            | not mode64
            , (False,c) <- regCodeX r
            -> pure $ set_opsize16 $ primary (0x40 + c)
          [R32 r]
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
    IMUL -> alts
      [ handle_ext_rm     primary 0xF6 0x5
      , handle_reg_rm     map_0F  0xAF
      , handle_reg_rm_i8  primary 0x6B
      , handle_reg_rm_imm primary 0x69
      ]

    SXA -> case args of
      -- Intel uses CBW/CWDE/CDQE mnemonics to differentiate the operand size.
      -- This sucks. Let's use the destination register (AX,EAX,RAX) as an
      -- operand instead.
      [OpReg R_AX ] -> pure $ set_opsize16 $ primary 0x98
      [OpReg R_EAX] -> pure $ set_opsize32 $ primary 0x98
      [OpReg R_RAX] -> pure $ set_opsize64 $ primary 0x98
      _                  -> Nothing

    SXAD -> case args of
      -- Intel uses CWD/CDQ/CQO mnemonics to differentiate the operand size.
      -- This sucks. Let's use the source/destination register (AX,EAX,RAX) as an
      -- operand instead.
      [OpReg R_AX ] -> pure $ set_opsize16 $ primary 0x99
      [OpReg R_EAX] -> pure $ set_opsize32 $ primary 0x99
      [OpReg R_RAX] -> pure $ set_opsize64 $ primary 0x99
      _                  -> Nothing

    MOV -> alts
      [ handle_rm_reg primary 0x88
      , handle_reg_rm primary 0x8A
        -- shorter forms for reg,imm + imm64 form!
      , case args of
          [R8  r, I8  i] ->
            pure $ set_imm8 i $ set_oc_reg primary 0xB0 r
          [R16 r, I16 i] ->
            pure $ set_opsize16 $ set_imm16 i $ set_oc_reg primary 0xB8 r
          [R32 r, I32 i] ->
            pure $ set_opsize32 $ set_imm32 i $ set_oc_reg primary 0xB8 r
          [R64 r, I64 i] ->
            pure $ set_opsize64 $ set_imm64 i $ set_oc_reg primary 0xB8 r
          _ -> Nothing
      , handle_rm_imm primary 0xC6 0x0
      -- TODO: mov rm, sreg
      -- TODO: mov sreg, rm
      -- TODO: mov acc, moffs
      -- TODO: mov moffs, acc
      ]

    MOVSX -> case args of
      [R16 d, R8 s]  -> pure $ set_opsize16 $ set_rm_reg_reg d s $ map_0F 0xBE
      [R32 d, R8 s]  -> pure $ set_opsize32 $ set_rm_reg_reg d s $ map_0F 0xBE
      [R64 d, R8 s]  -> pure $ set_opsize64 $ set_rm_reg_reg d s $ map_0F 0xBE
      [R32 d, R16 s] -> pure $                set_rm_reg_reg d s $ map_0F 0xBF
      [R64 d, R16 s] -> pure $ set_opsize64 $ set_rm_reg_reg d s $ map_0F 0xBF
      [R64 d, R32 s] -> pure $ set_opsize64 $ set_rm_reg_reg d s $ primary 0x63
      [R16 d, M8 s]  -> pure $ set_opsize16 $ set_rm_reg_mem d s $ map_0F 0xBE
      [R32 d, M8 s]  -> pure $ set_opsize32 $ set_rm_reg_mem d s $ map_0F 0xBE
      [R64 d, M8 s]  -> pure $ set_opsize64 $ set_rm_reg_mem d s $ map_0F 0xBE
      [R32 d, M16 s] -> pure $                set_rm_reg_mem d s $ map_0F 0xBF
      [R64 d, M16 s] -> pure $ set_opsize64 $ set_rm_reg_mem d s $ map_0F 0xBF
      [R64 d, M32 s] -> pure $ set_opsize64 $ set_rm_reg_mem d s $ primary 0x63
      -- discouraged (use normal MOV instead) but still supported
      [R16 d, R16 s] -> pure $ set_opsize16 $ set_rm_reg_reg d s $ primary 0x63
      [R32 d, R32 s] -> pure $ set_opsize32 $ set_rm_reg_reg d s $ primary 0x63
      [R16 d, M16 s] -> pure $ set_opsize16 $ set_rm_reg_mem d s $ primary 0x63
      [R32 d, M32 s] -> pure $ set_opsize32 $ set_rm_reg_mem d s $ primary 0x63
      _ -> Nothing

    MOVZX -> case args of
      [R16 d, R8 s]  -> pure $ set_opsize16 $ set_rm_reg_reg d s $ map_0F 0xB6
      [R32 d, R8 s]  -> pure $ set_opsize32 $ set_rm_reg_reg d s $ map_0F 0xB6
      [R64 d, R8 s]  -> pure $ set_opsize64 $ set_rm_reg_reg d s $ map_0F 0xB6
      [R32 d, R16 s] -> pure $                set_rm_reg_reg d s $ map_0F 0xB7
      [R64 d, R16 s] -> pure $ set_opsize64 $ set_rm_reg_reg d s $ map_0F 0xB7
      [R16 d, M8 s]  -> pure $ set_opsize16 $ set_rm_reg_mem d s $ map_0F 0xB6
      [R32 d, M8 s]  -> pure $ set_opsize32 $ set_rm_reg_mem d s $ map_0F 0xB6
      [R64 d, M8 s]  -> pure $ set_opsize64 $ set_rm_reg_mem d s $ map_0F 0xB6
      [R32 d, M16 s] -> pure $                set_rm_reg_mem d s $ map_0F 0xB7
      [R64 d, M16 s] -> pure $ set_opsize64 $ set_rm_reg_mem d s $ map_0F 0xB7
      -- [R64 d, R32 s] -- not supported: a simple MOV does the same thing
      -- [R64 d, M32 s] -- not supported: a simple MOV does the same thing
      _ -> Nothing

    MOVBE -> case args of
      [R16 d, M16 s] -> pure $ set_opsize16 $ set_rm_reg_mem d s $ map_0F38 0xF0
      [R32 d, M32 s] -> pure $ set_opsize32 $ set_rm_reg_mem d s $ map_0F38 0xF0
      [R64 d, M64 s] -> pure $ set_opsize64 $ set_rm_reg_mem d s $ map_0F38 0xF0
      [M16 d, R16 s] -> pure $ set_opsize16 $ set_rm_reg_mem s d $ map_0F38 0xF1
      [M32 d, R32 s] -> pure $ set_opsize32 $ set_rm_reg_mem s d $ map_0F38 0xF1
      [M64 d, R64 s] -> pure $ set_opsize64 $ set_rm_reg_mem s d $ map_0F38 0xF1
      _ -> Nothing

    CMOV cc -> do
      let oc = map_0F (0x40 + condCode cc)
      case args of
        [R16 d, R16 s] -> pure $ set_opsize16 $ set_rm_reg_reg d s oc
        [R32 d, R32 s] -> pure $ set_opsize32 $ set_rm_reg_reg d s oc
        [R64 d, R64 s] -> pure $ set_opsize64 $ set_rm_reg_reg d s oc
        [R16 d, M16 s] -> pure $ set_opsize16 $ set_rm_reg_mem d s oc
        [R32 d, M32 s] -> pure $ set_opsize32 $ set_rm_reg_mem d s oc
        [R64 d, M64 s] -> pure $ set_opsize64 $ set_rm_reg_mem d s oc
        _              -> Nothing

    SETcc cc -> case args of
      [R8 r] -> pure $ set_rm_ext_reg 0x0 r $ map_0F (0x90 + condCode cc)
      [M8 m] -> pure $ set_rm_ext_mem 0x0 m $ map_0F (0x90 + condCode cc)
      _      -> Nothing

    XCHG -> case args of
      [R16 R_AX,  R16 r]     -> pure $ set_opsize16 $ set_oc_reg primary 0x90 r
      [R16 r,     R16 R_AX]  -> pure $ set_opsize16 $ set_oc_reg primary 0x90 r
      [R32 R_EAX, R32 r]     -> pure $ set_opsize32 $ set_oc_reg primary 0x90 r
      [R32 r,     R32 R_EAX] -> pure $ set_opsize32 $ set_oc_reg primary 0x90 r
      [R64 R_RAX, R64 r]     -> pure $ set_opsize64 $ set_oc_reg primary 0x90 r
      [R64 r,     R64 R_RAX] -> pure $ set_opsize64 $ set_oc_reg primary 0x90 r
      [R8 r1,     R8 r2]     -> pure $ set_rm_reg_reg r1 r2 $ primary 0x86
      [M8 m,      R8 r]      -> pure $ set_rm_reg_mem r m   $ primary 0x86
      [R8 r,      M8 m]      -> pure $ set_rm_reg_mem r m   $ primary 0x86
      [R16 r1,    R16 r2]    -> pure $ set_opsize16 $ set_rm_reg_reg r1 r2 $ primary 0x87
      [M16 m,     R16 r]     -> pure $ set_opsize16 $ set_rm_reg_mem r m   $ primary 0x87
      [R16 r,     M16 m]     -> pure $ set_opsize16 $ set_rm_reg_mem r m   $ primary 0x87
      [R32 r1,    R32 r2]    -> pure $ set_opsize32 $ set_rm_reg_reg r1 r2 $ primary 0x87
      [M32 m,     R32 r]     -> pure $ set_opsize32 $ set_rm_reg_mem r m   $ primary 0x87
      [R32 r,     M32 m]     -> pure $ set_opsize32 $ set_rm_reg_mem r m   $ primary 0x87
      [R64 r1,    R64 r2]    -> pure $ set_opsize64 $ set_rm_reg_reg r1 r2 $ primary 0x87
      [M64 m,     R64 r]     -> pure $ set_opsize64 $ set_rm_reg_mem r m   $ primary 0x87
      [R64 r,     M64 m]     -> pure $ set_opsize64 $ set_rm_reg_mem r m   $ primary 0x87
      _ -> Nothing

    XADD -> case args of
      [R8 d,  R8 s] -> pure $ set_rm_reg_reg s d $ map_0F 0xC0
      [M8 d,  R8 s] -> pure $ set_rm_reg_mem s d $ map_0F 0xC0
      [R16 d, R16 s] -> pure $ set_opsize16 $ set_rm_reg_reg s d $ map_0F 0xC1
      [M16 d, R16 s] -> pure $ set_opsize16 $ set_rm_reg_mem s d $ map_0F 0xC1
      [R32 d, R32 s] -> pure $ set_opsize32 $ set_rm_reg_reg s d $ map_0F 0xC1
      [M32 d, R32 s] -> pure $ set_opsize32 $ set_rm_reg_mem s d $ map_0F 0xC1
      [R64 d, R64 s] -> pure $ set_opsize64 $ set_rm_reg_reg s d $ map_0F 0xC1
      [M64 d, R64 s] -> pure $ set_opsize64 $ set_rm_reg_mem s d $ map_0F 0xC1
      _ -> Nothing

    LEA -> case args of
      -- we don't care about the size of the targetted memory...
      [R16 r, OpMem m] -> do
        pure $ set_opsize16 $ set_rm_reg_mem r m $ primary 0x8D
      [R32 r, OpMem m] -> do
        pure $ set_opsize32 $ set_rm_reg_mem r m $ primary 0x8D
      [R64 r, OpMem m] -> do
        assert_mode64
        pure $ set_opsize64 $ set_rm_reg_mem r m $ primary 0x8D
      _ -> Nothing

    IN -> case args of
      [R8  R_AL,  I8 i]     -> pure $ set_imm8 i $ primary 0xE4
      [R16 R_AX,  I8 i]     -> pure $ set_opsize16 $ set_imm8 i $ primary 0xE5
      [R32 R_EAX, I8 i]     -> pure $ set_opsize32 $ set_imm8 i $ primary 0xE5
      [R8  R_AL,  R16 R_DX] -> pure $ primary 0xEC
      [R16 R_AX,  R16 R_DX] -> pure $ set_opsize16 $ primary 0xED
      [R32 R_EAX, R16 R_DX] -> pure $ set_opsize32 $ primary 0xED
      _ -> Nothing

    OUT -> case args of
      [I8 i, R8  R_AL]      -> pure $ set_imm8 i $ primary 0xE6
      [I8 i, R16 R_AX]      -> pure $ set_opsize16 $ set_imm8 i $ primary 0xE7
      [I8 i, R32 R_EAX]     -> pure $ set_opsize32 $ set_imm8 i $ primary 0xE7
      [R16 R_DX, R8  R_AL]  -> pure $ primary 0xEE
      [R16 R_DX, R16 R_AX]  -> pure $ set_opsize16 $ primary 0xEF
      [R16 R_DX, R32 R_EAX] -> pure $ set_opsize32 $ primary 0xEF
      _ -> Nothing

    INS osz asz -> do
      assert_no_args
      e <- case osz of
        OpSize8  -> pure $ primary 0x6C
        OpSize16 -> pure $ set_opsize16 $ primary 0x6D
        OpSize32 -> pure $ set_opsize32 $ primary 0x6D
        _        -> Nothing
      set_addrsize asz e

    OUTS osz asz -> do
      assert_no_args
      e <- case osz of
        OpSize8  -> pure $ primary 0x6E
        OpSize16 -> pure $ set_opsize16 $ primary 0x6F
        OpSize32 -> pure $ set_opsize32 $ primary 0x6F
        _        -> Nothing
      set_addrsize asz e

    PUSH -> case args of
      -- smaller forms for registers
      [R16 r] -> pure $ set_opsize16 $ set_oc_reg primary 0x50 r
      [R32 r] -> pure $ set_opsize32 $ set_oc_reg primary 0x50 r
      [R64 r] -> pure $ set_opsize64 $ set_oc_reg primary 0x50 r

      -- dead code because of the smaller forms above
      -- [R16 r] -> pure $ set_opsize16 $ set_rm_ext_reg 0x6 r $ primary 0xFF
      -- [R32 r] -> pure $ set_opsize32 $ set_rm_ext_reg 0x6 r $ primary 0xFF
      -- [R64 r] -> pure $ set_opsize64 $ set_rm_ext_reg 0x6 r $ primary 0xFF

      [M16 m] -> pure $ set_opsize16 $ set_rm_ext_mem 0x6 m $ primary 0xFF
      [M32 m] -> pure $ set_opsize32 $ set_rm_ext_mem 0x6 m $ primary 0xFF
      [M64 m] -> pure $ set_opsize64 $ set_rm_ext_mem 0x6 m $ primary 0xFF
      [I8 i]  -> pure $                set_imm8 i           $ primary 0x6A
      [I16 i] -> pure $ set_opsize16 $ set_imm16 i          $ primary 0x68
      [I32 i] -> pure $ set_opsize32 $ set_imm32 i          $ primary 0x68
      [OpSeg CS] | not mode64 -> pure $ primary 0x0E
      [OpSeg SS] | not mode64 -> pure $ primary 0x16
      [OpSeg DS] | not mode64 -> pure $ primary 0x1E
      [OpSeg ES] | not mode64 -> pure $ primary 0x06
      [OpSeg FS]              -> pure $ map_0F 0xA0
      [OpSeg GS]              -> pure $ map_0F 0xA8
      _ -> Nothing

    POP -> case args of
      -- smaller forms for registers
      [R16 r] -> pure $ set_opsize16 $ set_oc_reg primary 0x58 r
      [R32 r] -> pure $ set_opsize32 $ set_oc_reg primary 0x58 r
      [R64 r] -> pure $ set_opsize64 $ set_oc_reg primary 0x58 r

      -- dead code because of the smaller forms above
      -- [R16 r] -> pure $ set_opsize16 $ set_rm_ext_reg 0x0 r $ primary 0x8F
      -- [R32 r] -> pure $ set_opsize32 $ set_rm_ext_reg 0x0 r $ primary 0x8F
      -- [R64 r] -> pure $ set_opsize64 $ set_rm_ext_reg 0x0 r $ primary 0x8F

      [M16 m] -> pure $ set_opsize16 $ set_rm_ext_mem 0x0 m $ primary 0x8F
      [M32 m] -> pure $ set_opsize32 $ set_rm_ext_mem 0x0 m $ primary 0x8F
      [M64 m] -> pure $ set_opsize64 $ set_rm_ext_mem 0x0 m $ primary 0x8F
      -- TODO: the operand size can be used to select the stack increment. How
      -- do we provide this to users?
      [OpSeg SS] | not mode64 -> pure $ primary 0x17
      [OpSeg DS] | not mode64 -> pure $ primary 0x1F
      [OpSeg ES] | not mode64 -> pure $ primary 0x07
      [OpSeg FS]              -> pure $ map_0F 0xA1
      [OpSeg GS]              -> pure $ map_0F 0xA9
      _ -> Nothing

    POPA sz -> do
      assert_not_mode64
      assert_no_args
      case sz of
        OpSize16 -> pure $ set_opsize16 $ primary 0x61
        OpSize32 -> pure $ set_opsize32 $ primary 0x61
        _        -> Nothing

    PUSHA sz -> do
      assert_not_mode64
      assert_no_args
      case sz of
        OpSize16 -> pure $ set_opsize16 $ primary 0x60
        OpSize32 -> pure $ set_opsize32 $ primary 0x60
        _        -> Nothing

    Jcc cc -> case args of
      [I8  i] -> pure $ set_imm8 i $ primary (0x70 + condCode cc)
      [I16 i]
        | not mode64 -- not supported in 64-bit mode
        -> pure $ set_opsize16 $ set_imm16 i $ map_0F (0x80 + condCode cc)
      [I32 i] -> pure $ set_opsize32 $ set_imm32 i $ map_0F (0x80 + condCode cc)
      _       -> Nothing

    JMP -> case args of
      [I8  i] -> pure $ set_imm8 i $ primary 0xEB
      [I16 i]
        | not mode64
        -> pure $ set_opsize16 $ set_imm16 i $ primary 0xE9
      [I32 i] -> pure $ set_opsize32 $ set_imm32 i $ primary 0xE9
      [R16 r]
        | not mode64
        -> pure $ set_opsize16 $ set_rm_ext_reg 0x4 r $ primary 0xFF
      [R32 r]
        | not mode64
        -> pure $ set_opsize32 $ set_rm_ext_reg 0x4 r $ primary 0xFF
      [R64 r]
        | mode64 -- default to 64-bit size
        -> pure $ set_rm_ext_reg 0x4 r $ primary 0xFF
      [M16 m]
        | not mode64
        -> pure $ set_opsize16 $ set_rm_ext_mem 0x4 m $ primary 0xFF
      [M32 m]
        | not mode64
        -> pure $ set_opsize32 $ set_rm_ext_mem 0x4 m $ primary 0xFF
      [M64 m]
        | mode64 -- default to 64-bit size
        -> pure $ set_rm_ext_mem 0x4 m $ primary 0xFF
      -- TODO: other forms: ptr k:n, m k:n
      _       -> Nothing

    INTO -> do
      assert_not_mode64
      assert_no_args
      pure (primary 0xCE)
    INT1 -> assert_no_args >> pure (primary 0xF1)
    INT3 -> assert_no_args >> pure (primary 0xCC)
    INT  -> do
      i <- imm8_arg
      pure $ set_imm8 i $ primary 0xCD

    SYSCALL -> do
      assert_mode64
      assert_no_args
      pure (map_0F 0x05)

    RET -> case args of
      []      -> pure $ primary 0xC3
      [I16 i] -> pure $ set_imm16 i $ primary 0xC2
      _       -> Nothing

    RET_FAR -> case args of
      []      -> pure $ primary 0xCB
      [I16 i] -> pure $ set_imm16 i $ primary 0xCA
      _       -> Nothing

    CALL -> case args of
      [I16 i] -> pure $ set_opsize16 $ set_imm16 i $ primary 0xE8
      [I32 i] -> pure $ set_opsize32 $ set_imm32 i $ primary 0xE8
      [R16 r] | not mode64 -> pure $ set_opsize16 $ set_rm_ext_reg 0x2 r $ primary 0xFF
      [R32 r] | not mode64 -> pure $ set_opsize32 $ set_rm_ext_reg 0x2 r $ primary 0xFF
      [M16 m] | not mode64 -> pure $ set_opsize16 $ set_rm_ext_mem 0x2 m $ primary 0xFF
      [M32 m] | not mode64 -> pure $ set_opsize32 $ set_rm_ext_mem 0x2 m $ primary 0xFF
      -- default to 64-bit operand size in 64-bit mode
      [R64 r]              -> pure $                set_rm_ext_reg 0x2 r $ primary 0xFF
      [M64 m]              -> pure $                set_rm_ext_mem 0x2 m $ primary 0xFF
      _ -> Nothing

    CALL_FAR -> Nothing -- TODO: far CALLs

    UD0 -> case args of
      [R32 r, R32 rm] -> pure $ set_rm_reg_reg r rm $ map_0F 0xFF
      [R32 r, M32 rm] -> pure $ set_rm_reg_mem r rm $ map_0F 0xFF
      _ -> Nothing

    UD1 -> case args of
      [R32 r, R32 rm] -> pure $ set_rm_reg_reg r rm $ map_0F 0xB9
      [R32 r, M32 rm] -> pure $ set_rm_reg_mem r rm $ map_0F 0xB9
      _ -> Nothing

    UD2 -> do
      assert_no_args
      pure $ map_0F 0x0B

    CMC -> assert_no_args >> pure (primary 0xF5)
    CLC -> assert_no_args >> pure (primary 0xF8)
    STC -> assert_no_args >> pure (primary 0xF9)
    CLI -> assert_no_args >> pure (primary 0xFA)
    STI -> assert_no_args >> pure (primary 0xFB)
    CLD -> assert_no_args >> pure (primary 0xFC)
    STD -> assert_no_args >> pure (primary 0xFD)

    LAHF -> assert_no_args >> pure (primary 0x9F)
    SAHF -> assert_no_args >> pure (primary 0x9E)

    POPF sz -> do
      assert_no_args
      case sz of
        OpSize8  -> Nothing
        OpSize16 -> pure $ set_opsize16 $ primary 0x9D
        OpSize32 -> pure $ set_opsize32 $ primary 0x9D
        OpSize64 -> pure $ set_opsize64 $ primary 0x9D

    PUSHF sz -> do
      assert_no_args
      case sz of
        OpSize8  -> Nothing
        OpSize16 -> pure $ set_opsize16 $ primary 0x9C
        OpSize32 -> pure $ set_opsize32 $ primary 0x9C
        OpSize64 -> pure $ set_opsize64 $ primary 0x9C

    CPUID -> assert_no_args >> pure (map_0F 0xA2)

    ADCX -> do
      has_extension Ext.ADX
      case args of
        [R32 r, M32 m] -> do
          pure $ set_rm_reg_mem r m $ map_66_0F38 0xF6
        [R64 r, M64 m] -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_mem r m $ map_66_0F38 0xF6
        [R32 r, R32 rm] -> do
          pure $ set_rm_reg_reg r rm $ map_66_0F38 0xF6
        [R64 r, R64 rm] -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_reg r rm $ map_66_0F38 0xF6
        _ -> Nothing

    ADOX -> do
      has_extension Ext.ADX
      case args of
        [R32 r, M32 m] -> do
          pure $ set_rm_reg_mem r m $ map_F3_0F38 0xF6
        [R64 r, M64 m] -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_mem r m $ map_F3_0F38 0xF6
        [R32 r, R32 rm] -> do
          pure $ set_rm_reg_reg r rm $ map_F3_0F38 0xF6
        [R64 r, R64 rm] -> do
          assert_mode64
          pure $ set_opsize64 $ set_rm_reg_reg r rm $ map_F3_0F38 0xF6
        _ -> Nothing

    ADDPD -> 
      case args of
        [V128 v, M128 m] -> do
          has_extension Ext.SSE2
          pure $ set_rm_vec_mem v m $ map_66_0F 0x58
        [V128 v1, V128 v2] -> do
          has_extension Ext.SSE2
          pure $ set_rm_vec_vec v1 v2 $ map_66_0F 0x58
        _ -> Nothing
