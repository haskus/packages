{-# LANGUAGE OverloadedLists #-}

module Haskus.Arch.X86_64.ISA.Encoder
  ( encodeInsn
  , EError (..)
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
import Haskus.Arch.X86_64.ISA.Encoding.SIB
import Haskus.Arch.X86_64.ISA.Encoding.Disp
import Haskus.Arch.X86_64.ISA.Encoding.Vec
import Haskus.Arch.X86_64.ISA.Encoding.Segment
import Haskus.Arch.X86_64.ISA.Context
import qualified Haskus.Arch.X86_64.ISA.Extension as Ext
import Haskus.Arch.X86_64.ISA.Size

import Control.Applicative
import Haskus.Utils.Monad

-- | Encoding error
data EError
  = EOnlyAvailableInMode64
  | ENotAvailableInMode64
  | ERequireExtension !Ext.Extension
  | ERequireNoArg  -- ^ Some arguments were provided, but the instruction doesn't support any
  | EInvalidArgs   -- ^ Arguments aren't valid
  | EInvalidOpSize8
  | EInvalidOpSize16
  | EInvalidOpSize32
  | EInvalidOpSize64
  | EInvalidAddrSize
  | EInvalidMemOperand
  | EIncompatibleRegs
  | ENoEncoding
  deriving (Show,Eq,Ord)

-- | Encoder monad
--
-- State monad for an Enc, with failure mode
newtype E a = E (Enc -> (# Enc, (# a | EError #) #))

pattern ESuccess :: Enc -> a -> (# Enc, (# a | EError #) #)
pattern ESuccess e a = (# e, (# a | #) #)

pattern EFailure :: Enc -> EError -> (# Enc, (# a | EError #) #)
pattern EFailure e r = (# e, (# | r #) #)
{-# COMPLETE ESuccess, EFailure #-}

instance Functor E where
  fmap f (E g) = E \e0 -> case g e0 of
    EFailure e1 r -> EFailure e1 r
    ESuccess e1 a -> ESuccess e1 (f a)

instance Applicative E where
  pure a      = E \e -> ESuccess e a
  E mf <*> E ma = E \e0 -> case mf e0 of
    EFailure e1 r -> EFailure e1 r
    ESuccess e1 f -> case ma e1 of
      EFailure e2 r -> EFailure e2 r
      ESuccess e2 a -> ESuccess e2 (f a)

instance Monad E where
  E f >>= mg = E \e0 -> case f e0 of
    EFailure e r -> EFailure e r
    ESuccess e a -> let !(E g) = mg a in g e

-- | Encoder failure
efail :: EError -> E a
efail r = E \e -> EFailure e r

-- | No encoding failure
noEncoding :: E ()
noEncoding = efail ENoEncoding

-- | Invalid args failure
invalidArgs :: E a
invalidArgs = efail EInvalidArgs

-- | Get current encoding
getEnc :: E Enc
getEnc = E \e -> ESuccess e e

-- | Set current encoding
setEnc :: Enc -> E ()
setEnc e = E \_ -> ESuccess e ()

-- | Modify current encoding
modifyEnc :: (Enc -> Enc) -> E ()
modifyEnc f = E \e -> ESuccess (f e) ()

-- | Run encoder monad
runE :: E () -> Either (Enc,EError) Enc
runE (E f) = case f emptyEnc of
  EFailure e r  -> Left (e,r)
  ESuccess e _a -> Right e
  

-- | Get the encoding specification of an instruction and its operands
encodeInsn :: Context -> Operation -> Operands -> Either (Enc,EError) Enc
encodeInsn !ctx !op !args = do
  let mode64              = is64bitMode (ctxMode ctx)
      assert_mode64       = unless mode64 $ efail EOnlyAvailableInMode64
      assert_not_mode64   = when   mode64 $ efail ENotAvailableInMode64
      require_extension x = unless (extensionAvailable ctx x) $ efail (ERequireExtension x)

      assert_no_args = case args of
        [] -> pure ()
        _  -> efail ERequireNoArg

      imm8_arg = case args of
        [I8 x] -> pure x
        _      -> invalidArgs
      
      primary   oc = modifyEnc \e -> e { encOpcode = Just (Op oc) }
      map_0F    oc = modifyEnc \e -> e { encOpcode = Just (Op_0F oc) }
      map_0F38  oc = modifyEnc \e -> e { encOpcode = Just (Op_0F38 oc) }

      prefix_67    = modifyEnc \e -> e { encPrefixes = P_67 : encPrefixes e }
      prefix_66    = modifyEnc \e -> e { encPrefixes = P_66 : encPrefixes e }
      prefix_F3    = modifyEnc \e -> e { encPrefixes = P_F3 : encPrefixes e }
      prefix_F2    = modifyEnc \e -> e { encPrefixes = P_F2 : encPrefixes e }

      set_imm8  i = set_imm (SizedValue8 i)
      set_imm16 i = set_imm (SizedValue16 i)
      set_imm32 i = set_imm (SizedValue32 i)
      set_imm64 i = set_imm (SizedValue64 i)

      set_imm imm = modifyEnc \e -> e { encImm = Just imm }

      set_modrm v = modifyEnc \e -> e { encModRM = Just (ModRM v) }

      set_segment ms = case ms of
        Nothing -> pure ()
        Just s  -> modifyEnc \e -> e { encPrefixes = segmentOverridePrefix s : encPrefixes e }

      set_opsize16 = case defaultOperationSize ctx of
        OpSize16 -> pure ()
        OpSize32 -> prefix_66
        OpSize8  -> efail EInvalidOpSize16
        OpSize64 -> efail EInvalidOpSize16

      set_opsize32 = case defaultOperationSize ctx of
        OpSize32 -> pure ()
        OpSize16 -> prefix_66
        OpSize8  -> efail EInvalidOpSize32
        OpSize64 -> efail EInvalidOpSize32

      set_opsize64 = modifyEnc \e -> e { encRex = encRex e <> Just rexW }

      set_addrsize asz
        | overriddenAddressSize False ctx == asz = pure ()
        | overriddenAddressSize True  ctx == asz = prefix_67
        | otherwise                              = efail EInvalidAddrSize

      -- store opcode extension in ModRM.reg
      set_r_ext ext = modifyEnc \e -> e { encModRM = encModRM e <> Just (mkModRM_reg ext) }

      -- store gpr in REX.R:ModRM.reg
      set_r_gpr r = set_r_reg (regREX r) xc c
        where !(xc,c) = regCodeX r

      -- store vec in REX.R:ModRM.reg
      set_r_vec r = set_r_reg False xc c
        where !(xc,c) = vecCodeX r

      -- store register code in REX.R:ModRM.reg
      set_r_reg force_rex xr r = modifyEnc \e ->
        let -- handle registers that require REX
            mrex1 = if force_rex then Just emptyRex else Nothing
            -- register code extension in REX.R
            mrex2 = if xr        then Just rexR     else Nothing
        in e { encRex   = encRex e <> mrex1 <> mrex2
             , encModRM = encModRM e <> Just (mkModRM_reg r)
             }

      -- store register code in REX.B:opcode
      set_oc_reg r = do
        let !(xc,c) = regCodeX r
            -- handle registers that require REX
            mrex1 = if regREX r then Just emptyRex else Nothing
            -- register code extension in REX.B
            mrex2 = if xc       then Just rexB     else Nothing
        modifyEnc \e ->
          let oc' = case encOpcode e of
                      Nothing -> error "set_oc_reg: expected an opcode"
                      Just o  -> case o of
                        Op      oc -> Op      (oc+c)
                        Op_0F   oc -> Op_0F   (oc+c)
                        Op_0F38 oc -> Op_0F38 (oc+c)
                        Op_0F3A oc -> Op_0F3A (oc+c)
                        Op_0F0F oc -> Op_0F0F (oc+c)
                        _ -> error $ "set_oc_reg: unexpected opcode: " ++ show o
          in e { encRex = mrex1 <> mrex2
               , encOpcode = Just oc'
               }

      -- store gpr in REX.B:ModRM.rm
      set_m_gpr r = set_m_reg (regREX r) xc c
        where !(xc,c) = regCodeX r

      -- store register code in REX.B:ModRM.rm
      set_m_reg force_rex xr r = modifyEnc \e ->
        let -- handle registers that require REX
            mrex1 = if force_rex then Just emptyRex else Nothing
            -- register code extension in REX.B
            mrex2 = if xr        then Just rexB     else Nothing
        in e { encRex   = encRex e <> mrex1 <> mrex2
             , encModRM = encModRM e <> Just (mkModRM_mod_rm 0b11 r)
             }

      -- encode memory operand
      set_m_mem mem = do
        e <- getEnc
        case encodeMem ctx mem e of
          Nothing -> efail EInvalidMemOperand
          Just e' -> setEnc e'

      -- store opcode extension in ModRM.reg and reg in ModRM.rm
      set_rm_ext_reg x r = set_r_ext x >> set_m_gpr r
      set_rm_reg_mem r m = set_r_gpr r >> set_m_mem m
      set_rm_ext_mem x m = set_r_ext x >> set_m_mem m
      set_rm_vec_mem v m = set_r_vec v >> set_m_mem m

      -- store v1 in ModRM.reg and v2 in ModRM.rm
      set_rm_vec_vec v1 v2 = do
        let
            -- ModRM.rm extension in REX.B; ModRM.reg extension in REX.R
            !(xr,r) = vecCodeX v1
            !(xm,m) = vecCodeX v2
            mrex1 = if xr then Just rexR else Nothing
            mrex2 = if xm then Just rexB else Nothing
        modifyEnc \e -> e { encRex   = encRex e <> mrex1 <> mrex2
                          , encModRM = Just (mkModRM 0b11 r m)
                          }

      -- store r1 in ModRM.reg and r2 in ModRM.rm
      set_rm_reg_reg r1 r2 = do
        unless (compatibleRegs r1 r2) do
          efail EIncompatibleRegs

        let
            -- handle registers that require REX
            mrex1 = if regREX r1 || regREX r2 then Just emptyRex else Nothing
            -- ModRM.rm extension in REX.B; ModRM.reg extension in REX.R
            !(xr,r) = regCodeX r1
            !(xm,m) = regCodeX r2
            mrex2 = if xr then Just rexR else Nothing
            mrex3 = if xm then Just rexB else Nothing
        modifyEnc \e -> e { encRex   = encRex e <> mrex1 <> mrex2 <> mrex3
                          , encModRM = Just (mkModRM 0b11 r m)
                          }

      -- store r1 in ModRM.rm and r2 in ModRM.reg
      set_mr_reg_reg r1 r2 = set_rm_reg_reg r2 r1

      -- several instructions have special encodings for the rAX, immN cases
      -- (ADD, ADC, etc.). We handle them here.
      handle_acc_imm mk_oc oc = case args of
        [R8 R_AL,  I8  i] -> Just do
          set_imm8 i << mk_oc oc
        [R16 R_AX,  I16 i] -> Just do
          set_opsize16 << set_imm16 i << mk_oc (oc+1)
        [R32 R_EAX, I32 i] -> Just do
          set_opsize32 << set_imm32 i << mk_oc (oc+1)
        [R64 R_RAX, I32 i] -> Just do
          assert_mode64
          set_opsize64 << set_imm32 i << mk_oc (oc+1)
        _ -> Nothing

      handle_rm_imm  mk_oc oc ext = case args of
        [R8  r, I8  i] -> Just do
          set_rm_ext_reg ext r << set_imm8 i << mk_oc oc
        [R16 r, I16 i] -> Just do
          set_opsize16 << set_rm_ext_reg ext r << set_imm16 i << mk_oc (oc+1)
        [R32 r, I32 i] -> Just do
          set_opsize32 << set_rm_ext_reg ext r << set_imm32 i << mk_oc (oc+1)
        [R64 r, I32 i] -> Just do
          assert_mode64
          set_opsize64 << set_rm_ext_reg ext r << set_imm32 i << mk_oc (oc+1)
        [M8  m, I8  i] -> Just do
          set_rm_ext_mem ext m << set_imm8 i << mk_oc oc
        [M16 m, I16 i] -> Just do
          set_opsize16 << set_rm_ext_mem ext m << set_imm16 i << mk_oc (oc+1)
        [M32 m, I32 i] -> Just do
          set_opsize32 << set_rm_ext_mem ext m << set_imm32 i << mk_oc (oc+1)
        [M64 m, I32 i] -> Just do
          assert_mode64
          set_opsize64 << set_rm_ext_mem ext m << set_imm32 i << mk_oc (oc+1)
        _ -> Nothing

      handle_rm_imm8 mk_oc oc ext = case args of
        [R16 r, I8 i] -> Just do
          set_opsize16 << set_rm_ext_reg ext r << set_imm8 i << mk_oc oc
        [R32 r, I8 i] -> Just do
          set_opsize32 << set_rm_ext_reg ext r << set_imm8 i << mk_oc oc
        [R64 r, I8 i] -> Just do
          assert_mode64
          set_opsize64 << set_rm_ext_reg ext r << set_imm8 i << mk_oc oc
        [M16 m, I8 i] -> Just do
          set_opsize16 << set_rm_ext_mem ext m << set_imm8 i << mk_oc oc
        [M32 m, I8 i] -> Just do
          set_opsize32 << set_rm_ext_mem ext m << set_imm8 i << mk_oc oc
        [M64 m, I8 i] -> Just do
          assert_mode64
          set_opsize64 << set_rm_ext_mem ext m << set_imm8 i << mk_oc oc
        _ -> Nothing

      handle_rm_reg  mk_oc oc     = case args of
        [R8  r1, R8  r2] -> Just do
          set_mr_reg_reg r1 r2 << mk_oc oc
        [R16 r1, R16 r2] -> Just do
          set_opsize16 << set_mr_reg_reg r1 r2 << mk_oc (oc+1)
        [R32 r1, R32 r2] -> Just do
          set_opsize32 << set_mr_reg_reg r1 r2 << mk_oc (oc+1)
        [R64 r1, R64 r2] -> Just do
          assert_mode64
          set_opsize64 << set_mr_reg_reg r1 r2 << mk_oc (oc+1)
        [M8 m,  R8   r] -> Just do
          set_rm_reg_mem r m << mk_oc oc
        [M16 m, R16 r] -> Just do
          set_opsize16 << set_rm_reg_mem r m << mk_oc (oc+1)
        [M32 m, R32 r] -> Just do
          set_opsize32 << set_rm_reg_mem r m << mk_oc (oc+1)
        [M64 m, R64 r] -> Just do
          assert_mode64
          set_opsize64 << set_rm_reg_mem r m << mk_oc (oc+1)
        _ -> Nothing

      handle_reg_rm  mk_oc oc     = case args of
        [R8 r1, R8 r2] -> Just do
          set_rm_reg_reg r1 r2 << mk_oc oc
        [R16 r1, R16 r2] -> Just do
          set_opsize16 << set_rm_reg_reg r1 r2 << mk_oc (oc+1)
        [R32 r1, R32 r2] -> Just do
          set_opsize32 << set_rm_reg_reg r1 r2 << mk_oc (oc+1)
        [R64 r1, R64 r2] -> Just do
          assert_mode64
          set_opsize64 << set_rm_reg_reg r1 r2 << mk_oc (oc+1)
        [R8  r, M8  m] -> Just do
          set_rm_reg_mem r m << mk_oc oc
        [R16 r, M16 m] -> Just do
          set_opsize16 << set_rm_reg_mem r m << mk_oc (oc+1)
        [R32 r, M32 m] -> Just do
          set_opsize32 << set_rm_reg_mem r m << mk_oc (oc+1)
        [R64 r, M64 m] -> Just do
          assert_mode64
          set_opsize64 << set_rm_reg_mem r m << mk_oc (oc+1)
        _ -> Nothing

      handle_reg_rm_i8 mk_oc oc   = case args of
        [R16 r1, R16 r2, I8 i] -> Just do
          set_imm8 i << set_opsize16 << set_rm_reg_reg r1 r2 << mk_oc oc
        [R32 r1, R32 r2, I8 i] -> Just do
          set_imm8 i << set_opsize32 << set_rm_reg_reg r1 r2 << mk_oc oc
        [R64 r1, R64 r2, I8 i] -> Just do
          assert_mode64
          set_imm8 i << set_opsize64 << set_rm_reg_reg r1 r2 << mk_oc oc
        [R16 r, M16 m, I8 i] -> Just do
          set_imm8 i << set_opsize16 << set_rm_reg_mem r m << mk_oc oc
        [R32 r, M32 m, I8 i] -> Just do
          set_imm8 i << set_opsize32 << set_rm_reg_mem r m << mk_oc oc
        [R64 r, M64 m, I8 i] -> Just do
          assert_mode64
          set_imm8 i << set_opsize64 << set_rm_reg_mem r m << mk_oc oc
        _ -> Nothing

      handle_reg_rm_imm mk_oc oc  = case args of
        [R16 r1, R16 r2, I16 i] -> Just do
          set_imm16 i << set_opsize16 << set_rm_reg_reg r1 r2 << mk_oc oc
        [R32 r1, R32 r2, I32 i] -> Just do
          set_imm32 i << set_opsize32 << set_rm_reg_reg r1 r2 << mk_oc oc
        [R64 r1, R64 r2, I32 i] -> Just do
          assert_mode64
          set_imm32 i << set_opsize64 << set_rm_reg_reg r1 r2 << mk_oc oc
        [R16 r, M16 m, I16 i] -> Just do
          set_imm16 i << set_opsize16 << set_rm_reg_mem r m << mk_oc oc
        [R32 r, M32 m, I32 i] -> Just do
          set_imm32 i << set_opsize32 << set_rm_reg_mem r m << mk_oc oc
        [R64 r, M64 m, I32 i] -> Just do
          assert_mode64
          set_imm32 i << set_opsize64 << set_rm_reg_mem r m << mk_oc oc
        _ -> Nothing

      alts :: [Maybe (E ())] -> E ()
      alts as = case asum as of
        Nothing -> noEncoding
        Just e  -> e
        

  runE case op of
    AAA -> do
      assert_not_mode64
      primary 0x37
      assert_no_args

    AAS -> do
      assert_not_mode64
      primary 0x3F
      assert_no_args

    AAD -> do
      assert_not_mode64
      primary 0xD5
      set_imm8 =<< imm8_arg

    AAM -> do
      assert_not_mode64
      primary 0xD4
      set_imm8 =<< imm8_arg

    DAA -> do
      assert_not_mode64
      primary 0x27
      assert_no_args

    DAS -> do
      assert_not_mode64
      primary 0x2F
      assert_no_args

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
      [R8  rm, R8  r] ->                 set_rm_reg_reg r rm << map_0F 0xB0
      [M8  rm, R8  r] ->                 set_rm_reg_mem r rm << map_0F 0xB0
      [R16 rm, R16 r] -> set_opsize16 << set_rm_reg_reg r rm << map_0F 0xB1
      [M16 rm, R16 r] -> set_opsize16 << set_rm_reg_mem r rm << map_0F 0xB1
      [R32 rm, R32 r] -> set_opsize32 << set_rm_reg_reg r rm << map_0F 0xB1
      [M32 rm, R32 r] -> set_opsize32 << set_rm_reg_mem r rm << map_0F 0xB1
      [R64 rm, R64 r] -> set_opsize64 << set_rm_reg_reg r rm << map_0F 0xB1
      [M64 rm, R64 r] -> set_opsize64 << set_rm_reg_mem r rm << map_0F 0xB1
      _ -> invalidArgs

    CMPXCHGB -> case args of
      [M64  m] ->                 set_rm_ext_mem 0x1 m << map_0F 0xC7
      [M128 m] -> set_opsize64 << set_rm_ext_mem 0x1 m << map_0F 0xC7
      _ -> invalidArgs

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
      [R8  r] ->                 set_rm_ext_reg 0x2 r << primary 0xF6
      [M8  m] ->                 set_rm_ext_mem 0x2 m << primary 0xF6
      [R16 r] -> set_opsize16 << set_rm_ext_reg 0x2 r << primary 0xF7
      [R32 r] -> set_opsize32 << set_rm_ext_reg 0x2 r << primary 0xF7
      [R64 r] -> set_opsize64 << set_rm_ext_reg 0x2 r << primary 0xF7
      [M16 m] -> set_opsize16 << set_rm_ext_mem 0x2 m << primary 0xF7
      [M32 m] -> set_opsize32 << set_rm_ext_mem 0x2 m << primary 0xF7
      [M64 m] -> set_opsize64 << set_rm_ext_mem 0x2 m << primary 0xF7
      _ -> invalidArgs

    SHL -> case args of
      [R8  r, I8 1] ->                 set_rm_ext_reg 0x4 r << primary 0xD0
      [R16 r, I8 1] -> set_opsize16 << set_rm_ext_reg 0x4 r << primary 0xD1
      [R32 r, I8 1] -> set_opsize32 << set_rm_ext_reg 0x4 r << primary 0xD1
      [R64 r, I8 1] -> set_opsize64 << set_rm_ext_reg 0x4 r << primary 0xD1

      [M8  m, I8 1] ->                 set_rm_ext_mem 0x4 m << primary 0xD0
      [M16 m, I8 1] -> set_opsize16 << set_rm_ext_mem 0x4 m << primary 0xD1
      [M32 m, I8 1] -> set_opsize32 << set_rm_ext_mem 0x4 m << primary 0xD1
      [M64 m, I8 1] -> set_opsize64 << set_rm_ext_mem 0x4 m << primary 0xD1

      [R8  r, I8 i] ->                 set_rm_ext_reg 0x4 r << set_imm8 i << primary 0xC0
      [R16 r, I8 i] -> set_opsize16 << set_rm_ext_reg 0x4 r << set_imm8 i << primary 0xC1
      [R32 r, I8 i] -> set_opsize32 << set_rm_ext_reg 0x4 r << set_imm8 i << primary 0xC1
      [R64 r, I8 i] -> set_opsize64 << set_rm_ext_reg 0x4 r << set_imm8 i << primary 0xC1

      [M8  m, I8 i] ->                 set_rm_ext_mem 0x4 m << set_imm8 i << primary 0xC0
      [M16 m, I8 i] -> set_opsize16 << set_rm_ext_mem 0x4 m << set_imm8 i << primary 0xC1
      [M32 m, I8 i] -> set_opsize32 << set_rm_ext_mem 0x4 m << set_imm8 i << primary 0xC1
      [M64 m, I8 i] -> set_opsize64 << set_rm_ext_mem 0x4 m << set_imm8 i << primary 0xC1

      [R8  r, R8 R_CL] ->                 set_rm_ext_reg 0x4 r << primary 0xD2
      [R16 r, R8 R_CL] -> set_opsize16 << set_rm_ext_reg 0x4 r << primary 0xD3
      [R32 r, R8 R_CL] -> set_opsize32 << set_rm_ext_reg 0x4 r << primary 0xD3
      [R64 r, R8 R_CL] -> set_opsize64 << set_rm_ext_reg 0x4 r << primary 0xD3

      [M8  m, R8 R_CL] ->                 set_rm_ext_mem 0x4 m << primary 0xD2
      [M16 m, R8 R_CL] -> set_opsize16 << set_rm_ext_mem 0x4 m << primary 0xD3
      [M32 m, R8 R_CL] -> set_opsize32 << set_rm_ext_mem 0x4 m << primary 0xD3
      [M64 m, R8 R_CL] -> set_opsize64 << set_rm_ext_mem 0x4 m << primary 0xD3
      _ -> invalidArgs

    SAR -> case args of
      [R8  r, I8 1] ->                 set_rm_ext_reg 0x7 r << primary 0xD0
      [R16 r, I8 1] -> set_opsize16 << set_rm_ext_reg 0x7 r << primary 0xD1
      [R32 r, I8 1] -> set_opsize32 << set_rm_ext_reg 0x7 r << primary 0xD1
      [R64 r, I8 1] -> set_opsize64 << set_rm_ext_reg 0x7 r << primary 0xD1

      [M8  m, I8 1] ->                 set_rm_ext_mem 0x7 m << primary 0xD0
      [M16 m, I8 1] -> set_opsize16 << set_rm_ext_mem 0x7 m << primary 0xD1
      [M32 m, I8 1] -> set_opsize32 << set_rm_ext_mem 0x7 m << primary 0xD1
      [M64 m, I8 1] -> set_opsize64 << set_rm_ext_mem 0x7 m << primary 0xD1

      [R8  r, I8 i] ->                 set_rm_ext_reg 0x7 r << set_imm8 i << primary 0xC0
      [R16 r, I8 i] -> set_opsize16 << set_rm_ext_reg 0x7 r << set_imm8 i << primary 0xC1
      [R32 r, I8 i] -> set_opsize32 << set_rm_ext_reg 0x7 r << set_imm8 i << primary 0xC1
      [R64 r, I8 i] -> set_opsize64 << set_rm_ext_reg 0x7 r << set_imm8 i << primary 0xC1

      [M8  m, I8 i] ->                 set_rm_ext_mem 0x7 m << set_imm8 i << primary 0xC0
      [M16 m, I8 i] -> set_opsize16 << set_rm_ext_mem 0x7 m << set_imm8 i << primary 0xC1
      [M32 m, I8 i] -> set_opsize32 << set_rm_ext_mem 0x7 m << set_imm8 i << primary 0xC1
      [M64 m, I8 i] -> set_opsize64 << set_rm_ext_mem 0x7 m << set_imm8 i << primary 0xC1

      [R8  r, R8 R_CL] ->                 set_rm_ext_reg 0x7 r << primary 0xD2
      [R16 r, R8 R_CL] -> set_opsize16 << set_rm_ext_reg 0x7 r << primary 0xD3
      [R32 r, R8 R_CL] -> set_opsize32 << set_rm_ext_reg 0x7 r << primary 0xD3
      [R64 r, R8 R_CL] -> set_opsize64 << set_rm_ext_reg 0x7 r << primary 0xD3

      [M8  m, R8 R_CL] ->                 set_rm_ext_mem 0x7 m << primary 0xD2
      [M16 m, R8 R_CL] -> set_opsize16 << set_rm_ext_mem 0x7 m << primary 0xD3
      [M32 m, R8 R_CL] -> set_opsize32 << set_rm_ext_mem 0x7 m << primary 0xD3
      [M64 m, R8 R_CL] -> set_opsize64 << set_rm_ext_mem 0x7 m << primary 0xD3
      _ -> invalidArgs

    SHR -> case args of
      [R8  r, I8 1] ->                 set_rm_ext_reg 0x5 r << primary 0xD0
      [R16 r, I8 1] -> set_opsize16 << set_rm_ext_reg 0x5 r << primary 0xD1
      [R32 r, I8 1] -> set_opsize32 << set_rm_ext_reg 0x5 r << primary 0xD1
      [R64 r, I8 1] -> set_opsize64 << set_rm_ext_reg 0x5 r << primary 0xD1

      [M8  m, I8 1] ->                 set_rm_ext_mem 0x5 m << primary 0xD0
      [M16 m, I8 1] -> set_opsize16 << set_rm_ext_mem 0x5 m << primary 0xD1
      [M32 m, I8 1] -> set_opsize32 << set_rm_ext_mem 0x5 m << primary 0xD1
      [M64 m, I8 1] -> set_opsize64 << set_rm_ext_mem 0x5 m << primary 0xD1

      [R8  r, I8 i] ->                 set_rm_ext_reg 0x5 r << set_imm8 i << primary 0xC0
      [R16 r, I8 i] -> set_opsize16 << set_rm_ext_reg 0x5 r << set_imm8 i << primary 0xC1
      [R32 r, I8 i] -> set_opsize32 << set_rm_ext_reg 0x5 r << set_imm8 i << primary 0xC1
      [R64 r, I8 i] -> set_opsize64 << set_rm_ext_reg 0x5 r << set_imm8 i << primary 0xC1

      [M8  m, I8 i] ->                 set_rm_ext_mem 0x5 m << set_imm8 i << primary 0xC0
      [M16 m, I8 i] -> set_opsize16 << set_rm_ext_mem 0x5 m << set_imm8 i << primary 0xC1
      [M32 m, I8 i] -> set_opsize32 << set_rm_ext_mem 0x5 m << set_imm8 i << primary 0xC1
      [M64 m, I8 i] -> set_opsize64 << set_rm_ext_mem 0x5 m << set_imm8 i << primary 0xC1

      [R8  r, R8 R_CL] ->                 set_rm_ext_reg 0x5 r << primary 0xD2
      [R16 r, R8 R_CL] -> set_opsize16 << set_rm_ext_reg 0x5 r << primary 0xD3
      [R32 r, R8 R_CL] -> set_opsize32 << set_rm_ext_reg 0x5 r << primary 0xD3
      [R64 r, R8 R_CL] -> set_opsize64 << set_rm_ext_reg 0x5 r << primary 0xD3

      [M8  m, R8 R_CL] ->                 set_rm_ext_mem 0x5 m << primary 0xD2
      [M16 m, R8 R_CL] -> set_opsize16 << set_rm_ext_mem 0x5 m << primary 0xD3
      [M32 m, R8 R_CL] -> set_opsize32 << set_rm_ext_mem 0x5 m << primary 0xD3
      [M64 m, R8 R_CL] -> set_opsize64 << set_rm_ext_mem 0x5 m << primary 0xD3
      _ -> invalidArgs

    ROL -> case args of
      [R8  r, I8 1] ->                 set_rm_ext_reg 0x0 r << primary 0xD0
      [R16 r, I8 1] -> set_opsize16 << set_rm_ext_reg 0x0 r << primary 0xD1
      [R32 r, I8 1] -> set_opsize32 << set_rm_ext_reg 0x0 r << primary 0xD1
      [R64 r, I8 1] -> set_opsize64 << set_rm_ext_reg 0x0 r << primary 0xD1

      [M8  m, I8 1] ->                 set_rm_ext_mem 0x0 m << primary 0xD0
      [M16 m, I8 1] -> set_opsize16 << set_rm_ext_mem 0x0 m << primary 0xD1
      [M32 m, I8 1] -> set_opsize32 << set_rm_ext_mem 0x0 m << primary 0xD1
      [M64 m, I8 1] -> set_opsize64 << set_rm_ext_mem 0x0 m << primary 0xD1

      [R8  r, I8 i] ->                 set_rm_ext_reg 0x0 r << set_imm8 i << primary 0xC0
      [R16 r, I8 i] -> set_opsize16 << set_rm_ext_reg 0x0 r << set_imm8 i << primary 0xC1
      [R32 r, I8 i] -> set_opsize32 << set_rm_ext_reg 0x0 r << set_imm8 i << primary 0xC1
      [R64 r, I8 i] -> set_opsize64 << set_rm_ext_reg 0x0 r << set_imm8 i << primary 0xC1

      [M8  m, I8 i] ->                 set_rm_ext_mem 0x0 m << set_imm8 i << primary 0xC0
      [M16 m, I8 i] -> set_opsize16 << set_rm_ext_mem 0x0 m << set_imm8 i << primary 0xC1
      [M32 m, I8 i] -> set_opsize32 << set_rm_ext_mem 0x0 m << set_imm8 i << primary 0xC1
      [M64 m, I8 i] -> set_opsize64 << set_rm_ext_mem 0x0 m << set_imm8 i << primary 0xC1

      [R8  r, R8 R_CL] ->                 set_rm_ext_reg 0x0 r << primary 0xD2
      [R16 r, R8 R_CL] -> set_opsize16 << set_rm_ext_reg 0x0 r << primary 0xD3
      [R32 r, R8 R_CL] -> set_opsize32 << set_rm_ext_reg 0x0 r << primary 0xD3
      [R64 r, R8 R_CL] -> set_opsize64 << set_rm_ext_reg 0x0 r << primary 0xD3

      [M8  m, R8 R_CL] ->                 set_rm_ext_mem 0x0 m << primary 0xD2
      [M16 m, R8 R_CL] -> set_opsize16 << set_rm_ext_mem 0x0 m << primary 0xD3
      [M32 m, R8 R_CL] -> set_opsize32 << set_rm_ext_mem 0x0 m << primary 0xD3
      [M64 m, R8 R_CL] -> set_opsize64 << set_rm_ext_mem 0x0 m << primary 0xD3
      _ -> invalidArgs

    ROR -> case args of
      [R8  r, I8 1] ->                 set_rm_ext_reg 0x1 r << primary 0xD0
      [R16 r, I8 1] -> set_opsize16 << set_rm_ext_reg 0x1 r << primary 0xD1
      [R32 r, I8 1] -> set_opsize32 << set_rm_ext_reg 0x1 r << primary 0xD1
      [R64 r, I8 1] -> set_opsize64 << set_rm_ext_reg 0x1 r << primary 0xD1

      [M8  m, I8 1] ->                 set_rm_ext_mem 0x1 m << primary 0xD0
      [M16 m, I8 1] -> set_opsize16 << set_rm_ext_mem 0x1 m << primary 0xD1
      [M32 m, I8 1] -> set_opsize32 << set_rm_ext_mem 0x1 m << primary 0xD1
      [M64 m, I8 1] -> set_opsize64 << set_rm_ext_mem 0x1 m << primary 0xD1

      [R8  r, I8 i] ->                 set_rm_ext_reg 0x1 r << set_imm8 i << primary 0xC0
      [R16 r, I8 i] -> set_opsize16 << set_rm_ext_reg 0x1 r << set_imm8 i << primary 0xC1
      [R32 r, I8 i] -> set_opsize32 << set_rm_ext_reg 0x1 r << set_imm8 i << primary 0xC1
      [R64 r, I8 i] -> set_opsize64 << set_rm_ext_reg 0x1 r << set_imm8 i << primary 0xC1

      [M8  m, I8 i] ->                 set_rm_ext_mem 0x1 m << set_imm8 i << primary 0xC0
      [M16 m, I8 i] -> set_opsize16 << set_rm_ext_mem 0x1 m << set_imm8 i << primary 0xC1
      [M32 m, I8 i] -> set_opsize32 << set_rm_ext_mem 0x1 m << set_imm8 i << primary 0xC1
      [M64 m, I8 i] -> set_opsize64 << set_rm_ext_mem 0x1 m << set_imm8 i << primary 0xC1

      [R8  r, R8 R_CL] ->                 set_rm_ext_reg 0x1 r << primary 0xD2
      [R16 r, R8 R_CL] -> set_opsize16 << set_rm_ext_reg 0x1 r << primary 0xD3
      [R32 r, R8 R_CL] -> set_opsize32 << set_rm_ext_reg 0x1 r << primary 0xD3
      [R64 r, R8 R_CL] -> set_opsize64 << set_rm_ext_reg 0x1 r << primary 0xD3

      [M8  m, R8 R_CL] ->                 set_rm_ext_mem 0x1 m << primary 0xD2
      [M16 m, R8 R_CL] -> set_opsize16 << set_rm_ext_mem 0x1 m << primary 0xD3
      [M32 m, R8 R_CL] -> set_opsize32 << set_rm_ext_mem 0x1 m << primary 0xD3
      [M64 m, R8 R_CL] -> set_opsize64 << set_rm_ext_mem 0x1 m << primary 0xD3
      _ -> invalidArgs

    RCL -> case args of
      [R8  r, I8 1] ->                 set_rm_ext_reg 0x2 r << primary 0xD0
      [R16 r, I8 1] -> set_opsize16 << set_rm_ext_reg 0x2 r << primary 0xD1
      [R32 r, I8 1] -> set_opsize32 << set_rm_ext_reg 0x2 r << primary 0xD1
      [R64 r, I8 1] -> set_opsize64 << set_rm_ext_reg 0x2 r << primary 0xD1

      [M8  m, I8 1] ->                 set_rm_ext_mem 0x2 m << primary 0xD0
      [M16 m, I8 1] -> set_opsize16 << set_rm_ext_mem 0x2 m << primary 0xD1
      [M32 m, I8 1] -> set_opsize32 << set_rm_ext_mem 0x2 m << primary 0xD1
      [M64 m, I8 1] -> set_opsize64 << set_rm_ext_mem 0x2 m << primary 0xD1

      [R8  r, I8 i] ->                 set_rm_ext_reg 0x2 r << set_imm8 i << primary 0xC0
      [R16 r, I8 i] -> set_opsize16 << set_rm_ext_reg 0x2 r << set_imm8 i << primary 0xC1
      [R32 r, I8 i] -> set_opsize32 << set_rm_ext_reg 0x2 r << set_imm8 i << primary 0xC1
      [R64 r, I8 i] -> set_opsize64 << set_rm_ext_reg 0x2 r << set_imm8 i << primary 0xC1

      [M8  m, I8 i] ->                 set_rm_ext_mem 0x2 m << set_imm8 i << primary 0xC0
      [M16 m, I8 i] -> set_opsize16 << set_rm_ext_mem 0x2 m << set_imm8 i << primary 0xC1
      [M32 m, I8 i] -> set_opsize32 << set_rm_ext_mem 0x2 m << set_imm8 i << primary 0xC1
      [M64 m, I8 i] -> set_opsize64 << set_rm_ext_mem 0x2 m << set_imm8 i << primary 0xC1

      [R8  r, R8 R_CL] ->                 set_rm_ext_reg 0x2 r << primary 0xD2
      [R16 r, R8 R_CL] -> set_opsize16 << set_rm_ext_reg 0x2 r << primary 0xD3
      [R32 r, R8 R_CL] -> set_opsize32 << set_rm_ext_reg 0x2 r << primary 0xD3
      [R64 r, R8 R_CL] -> set_opsize64 << set_rm_ext_reg 0x2 r << primary 0xD3

      [M8  m, R8 R_CL] ->                 set_rm_ext_mem 0x2 m << primary 0xD2
      [M16 m, R8 R_CL] -> set_opsize16 << set_rm_ext_mem 0x2 m << primary 0xD3
      [M32 m, R8 R_CL] -> set_opsize32 << set_rm_ext_mem 0x2 m << primary 0xD3
      [M64 m, R8 R_CL] -> set_opsize64 << set_rm_ext_mem 0x2 m << primary 0xD3
      _ -> invalidArgs

    RCR -> case args of
      [R8  r, I8 1] ->                 set_rm_ext_reg 0x3 r << primary 0xD0
      [R16 r, I8 1] -> set_opsize16 << set_rm_ext_reg 0x3 r << primary 0xD1
      [R32 r, I8 1] -> set_opsize32 << set_rm_ext_reg 0x3 r << primary 0xD1
      [R64 r, I8 1] -> set_opsize64 << set_rm_ext_reg 0x3 r << primary 0xD1

      [M8  m, I8 1] ->                 set_rm_ext_mem 0x3 m << primary 0xD0
      [M16 m, I8 1] -> set_opsize16 << set_rm_ext_mem 0x3 m << primary 0xD1
      [M32 m, I8 1] -> set_opsize32 << set_rm_ext_mem 0x3 m << primary 0xD1
      [M64 m, I8 1] -> set_opsize64 << set_rm_ext_mem 0x3 m << primary 0xD1

      [R8  r, I8 i] ->                 set_rm_ext_reg 0x3 r << set_imm8 i << primary 0xC0
      [R16 r, I8 i] -> set_opsize16 << set_rm_ext_reg 0x3 r << set_imm8 i << primary 0xC1
      [R32 r, I8 i] -> set_opsize32 << set_rm_ext_reg 0x3 r << set_imm8 i << primary 0xC1
      [R64 r, I8 i] -> set_opsize64 << set_rm_ext_reg 0x3 r << set_imm8 i << primary 0xC1

      [M8  m, I8 i] ->                 set_rm_ext_mem 0x3 m << set_imm8 i << primary 0xC0
      [M16 m, I8 i] -> set_opsize16 << set_rm_ext_mem 0x3 m << set_imm8 i << primary 0xC1
      [M32 m, I8 i] -> set_opsize32 << set_rm_ext_mem 0x3 m << set_imm8 i << primary 0xC1
      [M64 m, I8 i] -> set_opsize64 << set_rm_ext_mem 0x3 m << set_imm8 i << primary 0xC1

      [R8  r, R8 R_CL] ->                 set_rm_ext_reg 0x3 r << primary 0xD2
      [R16 r, R8 R_CL] -> set_opsize16 << set_rm_ext_reg 0x3 r << primary 0xD3
      [R32 r, R8 R_CL] -> set_opsize32 << set_rm_ext_reg 0x3 r << primary 0xD3
      [R64 r, R8 R_CL] -> set_opsize64 << set_rm_ext_reg 0x3 r << primary 0xD3

      [M8  m, R8 R_CL] ->                 set_rm_ext_mem 0x3 m << primary 0xD2
      [M16 m, R8 R_CL] -> set_opsize16 << set_rm_ext_mem 0x3 m << primary 0xD3
      [M32 m, R8 R_CL] -> set_opsize32 << set_rm_ext_mem 0x3 m << primary 0xD3
      [M64 m, R8 R_CL] -> set_opsize64 << set_rm_ext_mem 0x3 m << primary 0xD3
      _ -> invalidArgs

    BSWAP -> do
      -- TODO: not allowed before 486
      case args of
        [R32 r] -> set_opsize32 << set_oc_reg r << map_0F 0xC8
        [R64 r] -> set_opsize64 << set_oc_reg r << map_0F 0xC8
        _       -> invalidArgs

    POPCNT -> do
      require_extension Ext.POPCNT
      case args of
        [R16 d, R16 s] -> set_opsize16 << set_rm_reg_reg d s << prefix_F3 << map_0F 0xB8
        [R16 d, M16 s] -> set_opsize16 << set_rm_reg_mem d s << prefix_F3 << map_0F 0xB8
        [R32 d, R32 s] -> set_opsize32 << set_rm_reg_reg d s << prefix_F3 << map_0F 0xB8
        [R32 d, M32 s] -> set_opsize32 << set_rm_reg_mem d s << prefix_F3 << map_0F 0xB8
        [R64 d, R64 s] -> set_opsize64 << set_rm_reg_reg d s << prefix_F3 << map_0F 0xB8
        [R64 d, M64 s] -> set_opsize64 << set_rm_reg_mem d s << prefix_F3 << map_0F 0xB8
        _ -> invalidArgs

    LZCNT -> do
      require_extension Ext.LZCNT
      case args of
        [R16 d, R16 s] -> set_opsize16 << set_rm_reg_reg d s << prefix_F3 << map_0F 0xBD
        [R16 d, M16 s] -> set_opsize16 << set_rm_reg_mem d s << prefix_F3 << map_0F 0xBD
        [R32 d, R32 s] -> set_opsize32 << set_rm_reg_reg d s << prefix_F3 << map_0F 0xBD
        [R32 d, M32 s] -> set_opsize32 << set_rm_reg_mem d s << prefix_F3 << map_0F 0xBD
        [R64 d, R64 s] -> set_opsize64 << set_rm_reg_reg d s << prefix_F3 << map_0F 0xBD
        [R64 d, M64 s] -> set_opsize64 << set_rm_reg_mem d s << prefix_F3 << map_0F 0xBD
        _ -> invalidArgs

    TZCNT -> do
      require_extension Ext.BMI1
      case args of
        [R16 d, R16 s] -> set_opsize16 << set_rm_reg_reg d s << prefix_F3 << map_0F 0xBC
        [R16 d, M16 s] -> set_opsize16 << set_rm_reg_mem d s << prefix_F3 << map_0F 0xBC
        [R32 d, R32 s] -> set_opsize32 << set_rm_reg_reg d s << prefix_F3 << map_0F 0xBC
        [R32 d, M32 s] -> set_opsize32 << set_rm_reg_mem d s << prefix_F3 << map_0F 0xBC
        [R64 d, R64 s] -> set_opsize64 << set_rm_reg_reg d s << prefix_F3 << map_0F 0xBC
        [R64 d, M64 s] -> set_opsize64 << set_rm_reg_mem d s << prefix_F3 << map_0F 0xBC
        _ -> invalidArgs

    BSR -> do
      case args of
        [R16 d, R16 s] -> set_opsize16 << set_rm_reg_reg d s << map_0F 0xBD
        [R16 d, M16 s] -> set_opsize16 << set_rm_reg_mem d s << map_0F 0xBD
        [R32 d, R32 s] -> set_opsize32 << set_rm_reg_reg d s << map_0F 0xBD
        [R32 d, M32 s] -> set_opsize32 << set_rm_reg_mem d s << map_0F 0xBD
        [R64 d, R64 s] -> set_opsize64 << set_rm_reg_reg d s << map_0F 0xBD
        [R64 d, M64 s] -> set_opsize64 << set_rm_reg_mem d s << map_0F 0xBD
        _ -> invalidArgs

    BSF -> do
      case args of
        [R16 d, R16 s] -> set_opsize16 << set_rm_reg_reg d s << map_0F 0xBC
        [R16 d, M16 s] -> set_opsize16 << set_rm_reg_mem d s << map_0F 0xBC
        [R32 d, R32 s] -> set_opsize32 << set_rm_reg_reg d s << map_0F 0xBC
        [R32 d, M32 s] -> set_opsize32 << set_rm_reg_mem d s << map_0F 0xBC
        [R64 d, R64 s] -> set_opsize64 << set_rm_reg_reg d s << map_0F 0xBC
        [R64 d, M64 s] -> set_opsize64 << set_rm_reg_mem d s << map_0F 0xBC
        _ -> invalidArgs

    BT -> do
      case args of
        [R16 d, R16 s] -> set_opsize16 << set_rm_reg_reg s d << map_0F 0xA3
        [M16 d, R16 s] -> set_opsize16 << set_rm_reg_mem s d << map_0F 0xA3
        [R32 d, R32 s] -> set_opsize32 << set_rm_reg_reg s d << map_0F 0xA3
        [M32 d, R32 s] -> set_opsize32 << set_rm_reg_mem s d << map_0F 0xA3
        [R64 d, R64 s] -> set_opsize64 << set_rm_reg_reg s d << map_0F 0xA3
        [M64 d, R64 s] -> set_opsize64 << set_rm_reg_mem s d << map_0F 0xA3
        [R16 d, I8 i]  -> set_opsize16 << set_rm_ext_reg 0x4 d << set_imm8 i << map_0F 0xBA
        [M16 d, I8 i]  -> set_opsize16 << set_rm_ext_mem 0x4 d << set_imm8 i << map_0F 0xBA
        [R32 d, I8 i]  -> set_opsize32 << set_rm_ext_reg 0x4 d << set_imm8 i << map_0F 0xBA
        [M32 d, I8 i]  -> set_opsize32 << set_rm_ext_mem 0x4 d << set_imm8 i << map_0F 0xBA
        [R64 d, I8 i]  -> set_opsize64 << set_rm_ext_reg 0x4 d << set_imm8 i << map_0F 0xBA
        [M64 d, I8 i]  -> set_opsize64 << set_rm_ext_mem 0x4 d << set_imm8 i << map_0F 0xBA
        _ -> invalidArgs

    BTC -> do
      case args of
        [R16 d, R16 s] -> set_opsize16 << set_rm_reg_reg s d << map_0F 0xBB
        [M16 d, R16 s] -> set_opsize16 << set_rm_reg_mem s d << map_0F 0xBB
        [R32 d, R32 s] -> set_opsize32 << set_rm_reg_reg s d << map_0F 0xBB
        [M32 d, R32 s] -> set_opsize32 << set_rm_reg_mem s d << map_0F 0xBB
        [R64 d, R64 s] -> set_opsize64 << set_rm_reg_reg s d << map_0F 0xBB
        [M64 d, R64 s] -> set_opsize64 << set_rm_reg_mem s d << map_0F 0xBB
        [R16 d, I8 i]  -> set_opsize16 << set_rm_ext_reg 0x7 d << set_imm8 i << map_0F 0xBA
        [M16 d, I8 i]  -> set_opsize16 << set_rm_ext_mem 0x7 d << set_imm8 i << map_0F 0xBA
        [R32 d, I8 i]  -> set_opsize32 << set_rm_ext_reg 0x7 d << set_imm8 i << map_0F 0xBA
        [M32 d, I8 i]  -> set_opsize32 << set_rm_ext_mem 0x7 d << set_imm8 i << map_0F 0xBA
        [R64 d, I8 i]  -> set_opsize64 << set_rm_ext_reg 0x7 d << set_imm8 i << map_0F 0xBA
        [M64 d, I8 i]  -> set_opsize64 << set_rm_ext_mem 0x7 d << set_imm8 i << map_0F 0xBA
        _ -> invalidArgs

    BTR -> do
      case args of
        [R16 d, R16 s] -> set_opsize16 << set_rm_reg_reg s d << map_0F 0xB3
        [M16 d, R16 s] -> set_opsize16 << set_rm_reg_mem s d << map_0F 0xB3
        [R32 d, R32 s] -> set_opsize32 << set_rm_reg_reg s d << map_0F 0xB3
        [M32 d, R32 s] -> set_opsize32 << set_rm_reg_mem s d << map_0F 0xB3
        [R64 d, R64 s] -> set_opsize64 << set_rm_reg_reg s d << map_0F 0xB3
        [M64 d, R64 s] -> set_opsize64 << set_rm_reg_mem s d << map_0F 0xB3
        [R16 d, I8 i]  -> set_opsize16 << set_rm_ext_reg 0x6 d << set_imm8 i << map_0F 0xBA
        [M16 d, I8 i]  -> set_opsize16 << set_rm_ext_mem 0x6 d << set_imm8 i << map_0F 0xBA
        [R32 d, I8 i]  -> set_opsize32 << set_rm_ext_reg 0x6 d << set_imm8 i << map_0F 0xBA
        [M32 d, I8 i]  -> set_opsize32 << set_rm_ext_mem 0x6 d << set_imm8 i << map_0F 0xBA
        [R64 d, I8 i]  -> set_opsize64 << set_rm_ext_reg 0x6 d << set_imm8 i << map_0F 0xBA
        [M64 d, I8 i]  -> set_opsize64 << set_rm_ext_mem 0x6 d << set_imm8 i << map_0F 0xBA
        _ -> invalidArgs

    BTS -> do
      case args of
        [R16 d, R16 s] -> set_opsize16 << set_rm_reg_reg s d << map_0F 0xAB
        [M16 d, R16 s] -> set_opsize16 << set_rm_reg_mem s d << map_0F 0xAB
        [R32 d, R32 s] -> set_opsize32 << set_rm_reg_reg s d << map_0F 0xAB
        [M32 d, R32 s] -> set_opsize32 << set_rm_reg_mem s d << map_0F 0xAB
        [R64 d, R64 s] -> set_opsize64 << set_rm_reg_reg s d << map_0F 0xAB
        [M64 d, R64 s] -> set_opsize64 << set_rm_reg_mem s d << map_0F 0xAB
        [R16 d, I8 i]  -> set_opsize16 << set_rm_ext_reg 0x5 d << set_imm8 i << map_0F 0xBA
        [M16 d, I8 i]  -> set_opsize16 << set_rm_ext_mem 0x5 d << set_imm8 i << map_0F 0xBA
        [R32 d, I8 i]  -> set_opsize32 << set_rm_ext_reg 0x5 d << set_imm8 i << map_0F 0xBA
        [M32 d, I8 i]  -> set_opsize32 << set_rm_ext_mem 0x5 d << set_imm8 i << map_0F 0xBA
        [R64 d, I8 i]  -> set_opsize64 << set_rm_ext_reg 0x5 d << set_imm8 i << map_0F 0xBA
        [M64 d, I8 i]  -> set_opsize64 << set_rm_ext_mem 0x5 d << set_imm8 i << map_0F 0xBA
        _ -> invalidArgs

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

    DEC -> case args of
      [M8  m] ->                 set_rm_ext_mem 0x1 m << primary 0xFE
      [M16 m] -> set_opsize16 << set_rm_ext_mem 0x1 m << primary 0xFF
      [M32 m] -> set_opsize32 << set_rm_ext_mem 0x1 m << primary 0xFF
      [M64 m] -> set_opsize64 << set_rm_ext_mem 0x1 m << primary 0xFF
      -- shorter forms, except in 64-bit mode (reused for REX prefixes)
      [R16 r]
        | not mode64
        , (False,c) <- regCodeX r
        -> set_opsize16 << primary (0x48 + c)
      [R32 r]
        | not mode64
        , (False,c) <- regCodeX r
        -> set_opsize32 << primary (0x48 + c)
      [R8  r] ->                 set_rm_ext_reg 0x1 r << primary 0xFE
      [R16 r] -> set_opsize16 << set_rm_ext_reg 0x1 r << primary 0xFF
      [R32 r] -> set_opsize32 << set_rm_ext_reg 0x1 r << primary 0xFF
      [R64 r] -> set_opsize64 << set_rm_ext_reg 0x1 r << primary 0xFF
      _ -> invalidArgs

    INC -> case args of
      [M8  m] ->                 set_rm_ext_mem 0x0 m << primary 0xFE
      [M16 m] -> set_opsize16 << set_rm_ext_mem 0x0 m << primary 0xFF
      [M32 m] -> set_opsize32 << set_rm_ext_mem 0x0 m << primary 0xFF
      [M64 m] -> set_opsize64 << set_rm_ext_mem 0x0 m << primary 0xFF
      -- shorter forms, except in 64-bit mode (reused for REX prefixes)
      [R16 r]
        | not mode64
        , (False,c) <- regCodeX r
        -> set_opsize16 << primary (0x40 + c)
      [R32 r]
        | not mode64
        , (False,c) <- regCodeX r
        -> set_opsize32 << primary (0x40 + c)
      [R8  r] ->                 set_rm_ext_reg 0x0 r << primary 0xFE
      [R16 r] -> set_opsize16 << set_rm_ext_reg 0x0 r << primary 0xFF
      [R32 r] -> set_opsize32 << set_rm_ext_reg 0x0 r << primary 0xFF
      [R64 r] -> set_opsize64 << set_rm_ext_reg 0x0 r << primary 0xFF
      _ -> invalidArgs

    NEG  -> case args of
      [M8  m] ->                 set_rm_ext_mem 0x3 m << primary 0xF6
      [M16 m] -> set_opsize16 << set_rm_ext_mem 0x3 m << primary 0xF7
      [M32 m] -> set_opsize32 << set_rm_ext_mem 0x3 m << primary 0xF7
      [M64 m] -> set_opsize64 << set_rm_ext_mem 0x3 m << primary 0xF7
      _ -> invalidArgs

    DIV  -> case args of
      [M8  m] ->                 set_rm_ext_mem 0x6 m << primary 0xF6
      [M16 m] -> set_opsize16 << set_rm_ext_mem 0x6 m << primary 0xF7
      [M32 m] -> set_opsize32 << set_rm_ext_mem 0x6 m << primary 0xF7
      [M64 m] -> set_opsize64 << set_rm_ext_mem 0x6 m << primary 0xF7
      _ -> invalidArgs

    MUL  -> case args of
      [M8  m] ->                 set_rm_ext_mem 0x4 m << primary 0xF6
      [M16 m] -> set_opsize16 << set_rm_ext_mem 0x4 m << primary 0xF7
      [M32 m] -> set_opsize32 << set_rm_ext_mem 0x4 m << primary 0xF7
      [M64 m] -> set_opsize64 << set_rm_ext_mem 0x4 m << primary 0xF7
      _ -> invalidArgs

    IDIV -> case args of
      [M8  m] ->                 set_rm_ext_mem 0x7 m << primary 0xF6
      [M16 m] -> set_opsize16 << set_rm_ext_mem 0x7 m << primary 0xF7
      [M32 m] -> set_opsize32 << set_rm_ext_mem 0x7 m << primary 0xF7
      [M64 m] -> set_opsize64 << set_rm_ext_mem 0x7 m << primary 0xF7
      _ -> invalidArgs

    IMUL -> alts
      [ case args of
          [M8  m] -> Just $                 set_rm_ext_mem 0x5 m << primary 0xF6
          [M16 m] -> Just $ set_opsize16 << set_rm_ext_mem 0x5 m << primary 0xF7
          [M32 m] -> Just $ set_opsize32 << set_rm_ext_mem 0x5 m << primary 0xF7
          [M64 m] -> Just $ set_opsize64 << set_rm_ext_mem 0x5 m << primary 0xF7
          [R8  r] -> Just $                 set_rm_ext_reg 0x5 r << primary 0xF6
          [R16 r] -> Just $ set_opsize16 << set_rm_ext_reg 0x5 r << primary 0xF7
          [R32 r] -> Just $ set_opsize32 << set_rm_ext_reg 0x5 r << primary 0xF7
          [R64 r] -> Just $ set_opsize64 << set_rm_ext_reg 0x5 r << primary 0xF7
          _ -> Nothing

      , handle_reg_rm     map_0F  0xAF
      , handle_reg_rm_i8  primary 0x6B
      , handle_reg_rm_imm primary 0x69
      ]

    SXA -> case args of
      -- Intel uses CBW/CWDE/CDQE mnemonics to differentiate the operand size.
      -- This sucks. Let's use the destination register (AX,EAX,RAX) as an
      -- operand instead.
      [OpReg R_AX ] -> set_opsize16 << primary 0x98
      [OpReg R_EAX] -> set_opsize32 << primary 0x98
      [OpReg R_RAX] -> set_opsize64 << primary 0x98
      _             -> invalidArgs

    SXAD -> case args of
      -- Intel uses CWD/CDQ/CQO mnemonics to differentiate the operand size.
      -- This sucks. Let's use the source/destination register (AX,EAX,RAX) as an
      -- operand instead.
      [OpReg R_AX ] -> set_opsize16 << primary 0x99
      [OpReg R_EAX] -> set_opsize32 << primary 0x99
      [OpReg R_RAX] -> set_opsize64 << primary 0x99
      _             -> invalidArgs

    MOV -> alts
      [ handle_rm_reg primary 0x88
      , handle_reg_rm primary 0x8A
        -- shorter forms for reg,imm + imm64 form!
      , case args of
          [R8  r, I8  i] -> pure $                 set_imm8  i << set_oc_reg r << primary 0xB0
          [R16 r, I16 i] -> pure $ set_opsize16 << set_imm16 i << set_oc_reg r << primary 0xB8
          [R32 r, I32 i] -> pure $ set_opsize32 << set_imm32 i << set_oc_reg r << primary 0xB8
          [R64 r, I64 i] -> pure $ set_opsize64 << set_imm64 i << set_oc_reg r << primary 0xB8
          _ -> Nothing
      , case args of
        [R8  r, I8  i] -> Just $                 set_rm_ext_reg 0x0 r << set_imm8  i << primary 0xC6
        [R16 r, I16 i] -> Just $ set_opsize16 << set_rm_ext_reg 0x0 r << set_imm16 i << primary 0xC7
        [R32 r, I32 i] -> Just $ set_opsize32 << set_rm_ext_reg 0x0 r << set_imm32 i << primary 0xC7
        [M8  m, I8  i] -> Just $                 set_rm_ext_mem 0x0 m << set_imm8  i << primary 0xC6
        [M16 m, I16 i] -> Just $ set_opsize16 << set_rm_ext_mem 0x0 m << set_imm16 i << primary 0xC7
        [M32 m, I32 i] -> Just $ set_opsize32 << set_rm_ext_mem 0x0 m << set_imm32 i << primary 0xC7
        -- These are really MOVSX, so we implement them as such.
        -- [R64 r, I32 i] -> Just $ set_opsize64 << set_rm_ext_reg 0x0 r << set_imm32 i << primary 0xC7
        -- [M64 m, I32 i] -> Just $ set_opsize64 << set_rm_ext_mem 0x0 m << set_imm32 i << primary 0xC7
        _ -> Nothing
      -- TODO: mov rm, sreg
      -- TODO: mov sreg, rm
      -- TODO: mov acc, moffs
      -- TODO: mov moffs, acc
      ]

    MOVSX -> case args of
      [R16 d, R8 s]  -> set_opsize16 << set_rm_reg_reg d s << map_0F 0xBE
      [R32 d, R8 s]  -> set_opsize32 << set_rm_reg_reg d s << map_0F 0xBE
      [R64 d, R8 s]  -> set_opsize64 << set_rm_reg_reg d s << map_0F 0xBE
      [R32 d, R16 s] ->                 set_rm_reg_reg d s << map_0F 0xBF
      [R64 d, R16 s] -> set_opsize64 << set_rm_reg_reg d s << map_0F 0xBF
      [R64 d, R32 s] -> set_opsize64 << set_rm_reg_reg d s << primary 0x63
      [R16 d, M8 s]  -> set_opsize16 << set_rm_reg_mem d s << map_0F 0xBE
      [R32 d, M8 s]  -> set_opsize32 << set_rm_reg_mem d s << map_0F 0xBE
      [R64 d, M8 s]  -> set_opsize64 << set_rm_reg_mem d s << map_0F 0xBE
      [R32 d, M16 s] ->                 set_rm_reg_mem d s << map_0F 0xBF
      [R64 d, M16 s] -> set_opsize64 << set_rm_reg_mem d s << map_0F 0xBF
      [R64 d, M32 s] -> set_opsize64 << set_rm_reg_mem d s << primary 0x63
      -- discouraged (use normal MOV instead) but still supported
      [R16 d, R16 s] -> set_opsize16 << set_rm_reg_reg d s << primary 0x63
      [R32 d, R32 s] -> set_opsize32 << set_rm_reg_reg d s << primary 0x63
      [R16 d, M16 s] -> set_opsize16 << set_rm_reg_mem d s << primary 0x63
      [R32 d, M32 s] -> set_opsize32 << set_rm_reg_mem d s << primary 0x63
      -- these are documented as MOVs, but they're really MOVSXs!
      [R64 r, I32 i] -> set_opsize64 << set_rm_ext_reg 0x0 r << set_imm32 i << primary 0xC7
      [M64 m, I32 i] -> set_opsize64 << set_rm_ext_mem 0x0 m << set_imm32 i << primary 0xC7
      _ -> invalidArgs

    MOVZX -> case args of
      [R16 d, R8 s]  -> set_opsize16 << set_rm_reg_reg d s << map_0F 0xB6
      [R32 d, R8 s]  -> set_opsize32 << set_rm_reg_reg d s << map_0F 0xB6
      [R64 d, R8 s]  -> set_opsize64 << set_rm_reg_reg d s << map_0F 0xB6
      [R32 d, R16 s] ->                 set_rm_reg_reg d s << map_0F 0xB7
      [R64 d, R16 s] -> set_opsize64 << set_rm_reg_reg d s << map_0F 0xB7
      [R16 d, M8 s]  -> set_opsize16 << set_rm_reg_mem d s << map_0F 0xB6
      [R32 d, M8 s]  -> set_opsize32 << set_rm_reg_mem d s << map_0F 0xB6
      [R64 d, M8 s]  -> set_opsize64 << set_rm_reg_mem d s << map_0F 0xB6
      [R32 d, M16 s] ->                 set_rm_reg_mem d s << map_0F 0xB7
      [R64 d, M16 s] -> set_opsize64 << set_rm_reg_mem d s << map_0F 0xB7
      -- [R64 d, R32 s] -- not supported: a simple MOV does the same thing
      -- [R64 d, M32 s] -- not supported: a simple MOV does the same thing
      _ -> invalidArgs

    MOVBE -> do
      require_extension Ext.MOVBE
      case args of
        [R16 d, M16 s] -> set_opsize16 << set_rm_reg_mem d s << map_0F38 0xF0
        [R32 d, M32 s] -> set_opsize32 << set_rm_reg_mem d s << map_0F38 0xF0
        [R64 d, M64 s] -> set_opsize64 << set_rm_reg_mem d s << map_0F38 0xF0
        [M16 d, R16 s] -> set_opsize16 << set_rm_reg_mem s d << map_0F38 0xF1
        [M32 d, R32 s] -> set_opsize32 << set_rm_reg_mem s d << map_0F38 0xF1
        [M64 d, R64 s] -> set_opsize64 << set_rm_reg_mem s d << map_0F38 0xF1
        _ -> invalidArgs

    CMOV cc -> do
      require_extension Ext.CMOV
      map_0F (0x40 + condCode cc)
      case args of
        [R16 d, R16 s] -> set_opsize16 << set_rm_reg_reg d s
        [R32 d, R32 s] -> set_opsize32 << set_rm_reg_reg d s
        [R64 d, R64 s] -> set_opsize64 << set_rm_reg_reg d s
        [R16 d, M16 s] -> set_opsize16 << set_rm_reg_mem d s
        [R32 d, M32 s] -> set_opsize32 << set_rm_reg_mem d s
        [R64 d, M64 s] -> set_opsize64 << set_rm_reg_mem d s
        _              -> invalidArgs

    SETcc cc -> case args of
      [R8 r] -> set_rm_ext_reg 0x0 r << map_0F (0x90 + condCode cc)
      [M8 m] -> set_rm_ext_mem 0x0 m << map_0F (0x90 + condCode cc)
      _      -> invalidArgs

    XCHG -> case args of
      [R16 R_AX,  R16 r]     -> set_opsize16 << set_oc_reg r << primary 0x90
      [R16 r,     R16 R_AX]  -> set_opsize16 << set_oc_reg r << primary 0x90
      [R32 R_EAX, R32 r]     -> set_opsize32 << set_oc_reg r << primary 0x90
      [R32 r,     R32 R_EAX] -> set_opsize32 << set_oc_reg r << primary 0x90
      [R64 R_RAX, R64 r]     -> set_opsize64 << set_oc_reg r << primary 0x90
      [R64 r,     R64 R_RAX] -> set_opsize64 << set_oc_reg r << primary 0x90
      [R8 r1,     R8 r2]     -> set_rm_reg_reg r1 r2 << primary 0x86
      [M8 m,      R8 r]      -> set_rm_reg_mem r m   << primary 0x86
      [R8 r,      M8 m]      -> set_rm_reg_mem r m   << primary 0x86
      [R16 r1,    R16 r2]    -> set_opsize16 << set_rm_reg_reg r1 r2 << primary 0x87
      [M16 m,     R16 r]     -> set_opsize16 << set_rm_reg_mem r m   << primary 0x87
      [R16 r,     M16 m]     -> set_opsize16 << set_rm_reg_mem r m   << primary 0x87
      [R32 r1,    R32 r2]    -> set_opsize32 << set_rm_reg_reg r1 r2 << primary 0x87
      [M32 m,     R32 r]     -> set_opsize32 << set_rm_reg_mem r m   << primary 0x87
      [R32 r,     M32 m]     -> set_opsize32 << set_rm_reg_mem r m   << primary 0x87
      [R64 r1,    R64 r2]    -> set_opsize64 << set_rm_reg_reg r1 r2 << primary 0x87
      [M64 m,     R64 r]     -> set_opsize64 << set_rm_reg_mem r m   << primary 0x87
      [R64 r,     M64 m]     -> set_opsize64 << set_rm_reg_mem r m   << primary 0x87
      _ -> invalidArgs

    XADD -> case args of
      [R8 d,  R8 s]  ->                set_rm_reg_reg s d << map_0F 0xC0
      [M8 d,  R8 s]  ->                set_rm_reg_mem s d << map_0F 0xC0
      [R16 d, R16 s] -> set_opsize16 << set_rm_reg_reg s d << map_0F 0xC1
      [M16 d, R16 s] -> set_opsize16 << set_rm_reg_mem s d << map_0F 0xC1
      [R32 d, R32 s] -> set_opsize32 << set_rm_reg_reg s d << map_0F 0xC1
      [M32 d, R32 s] -> set_opsize32 << set_rm_reg_mem s d << map_0F 0xC1
      [R64 d, R64 s] -> set_opsize64 << set_rm_reg_reg s d << map_0F 0xC1
      [M64 d, R64 s] -> set_opsize64 << set_rm_reg_mem s d << map_0F 0xC1
      _ -> invalidArgs

    MOVNTI -> do
      require_extension Ext.SSE2
      case args of
        [M32 m, R32 r] ->                set_rm_reg_mem r m << map_0F 0xC3
        [M64 m, R64 r] -> set_opsize64 << set_rm_reg_mem r m << map_0F 0xC3
        _ -> invalidArgs

    XLAT -> do
      case args of
        -- expect [[rS:]rBX + AL] form
        -- Use it to infer segment and address size
        [OpMem (Mem mseg _msize NoLock (MemAbs (Just base) Scale1 (Just R_AL) NoDisp) Nothing)]
          -> case base of
              R_BX  -> set_addrsize AddrSize16 << set_segment mseg << primary 0xD7
              R_EBX -> set_addrsize AddrSize32 << set_segment mseg << primary 0xD7
              R_RBX -> set_addrsize AddrSize64 << set_segment mseg << primary 0xD7
              _ -> invalidArgs
        _ -> invalidArgs


    LEA -> case args of
      -- we don't care about the size of the targetted memory...
      [R16 r, OpMem m] -> do
        set_opsize16 << set_rm_reg_mem r m << primary 0x8D
      [R32 r, OpMem m] -> do
        set_opsize32 << set_rm_reg_mem r m << primary 0x8D
      [R64 r, OpMem m] -> do
        assert_mode64
        set_opsize64 << set_rm_reg_mem r m << primary 0x8D
      _ -> invalidArgs

    IN -> case args of
      [R8  R_AL,  I8 i]     -> set_imm8 i << primary 0xE4
      [R16 R_AX,  I8 i]     -> set_opsize16 << set_imm8 i << primary 0xE5
      [R32 R_EAX, I8 i]     -> set_opsize32 << set_imm8 i << primary 0xE5
      [R8  R_AL,  R16 R_DX] -> primary 0xEC
      [R16 R_AX,  R16 R_DX] -> set_opsize16 << primary 0xED
      [R32 R_EAX, R16 R_DX] -> set_opsize32 << primary 0xED
      _ -> invalidArgs

    OUT -> case args of
      [I8 i, R8  R_AL]      -> set_imm8 i << primary 0xE6
      [I8 i, R16 R_AX]      -> set_opsize16 << set_imm8 i << primary 0xE7
      [I8 i, R32 R_EAX]     -> set_opsize32 << set_imm8 i << primary 0xE7
      [R16 R_DX, R8  R_AL]  -> primary 0xEE
      [R16 R_DX, R16 R_AX]  -> set_opsize16 << primary 0xEF
      [R16 R_DX, R32 R_EAX] -> set_opsize32 << primary 0xEF
      _ -> invalidArgs

    INS osz asz -> do
      assert_no_args
      case osz of
        OpSize8  -> primary 0x6C
        OpSize16 -> set_opsize16 << primary 0x6D
        OpSize32 -> set_opsize32 << primary 0x6D
        OpSize64 -> efail EInvalidOpSize64
      set_addrsize asz

    OUTS osz asz -> do
      assert_no_args
      case osz of
        OpSize8  -> primary 0x6E
        OpSize16 -> set_opsize16 << primary 0x6F
        OpSize32 -> set_opsize32 << primary 0x6F
        OpSize64 -> efail EInvalidOpSize64
      set_addrsize asz

    MOVS osz asz -> do
      assert_no_args
      case osz of
        OpSize8  -> primary 0xA4
        OpSize16 -> set_opsize16 << primary 0xA5
        OpSize32 -> set_opsize32 << primary 0xA5
        OpSize64 -> set_opsize64 << primary 0xA5
      set_addrsize asz

    STOS osz asz -> do
      assert_no_args
      case osz of
        OpSize8  -> primary 0xAA
        OpSize16 -> set_opsize16 << primary 0xAB
        OpSize32 -> set_opsize32 << primary 0xAB
        OpSize64 -> set_opsize64 << primary 0xAB
      set_addrsize asz

    LODS osz asz -> do
      assert_no_args
      case osz of
        OpSize8  -> primary 0xAC
        OpSize16 -> set_opsize16 << primary 0xAD
        OpSize32 -> set_opsize32 << primary 0xAD
        OpSize64 -> set_opsize64 << primary 0xAD
      set_addrsize asz

    CMPS osz asz -> do
      assert_no_args
      case osz of
        OpSize8  -> primary 0xA6
        OpSize16 -> set_opsize16 << primary 0xA7
        OpSize32 -> set_opsize32 << primary 0xA7
        OpSize64 -> set_opsize64 << primary 0xA7
      set_addrsize asz

    SCAS osz asz -> do
      assert_no_args
      case osz of
        OpSize8  -> primary 0xAE
        OpSize16 -> set_opsize16 << primary 0xAF
        OpSize32 -> set_opsize32 << primary 0xAF
        OpSize64 -> set_opsize64 << primary 0xAF
      set_addrsize asz


    PUSH -> case args of
      -- smaller forms for registers
      [R16 r] -> set_opsize16 << set_oc_reg r << primary 0x50
      [R32 r] -> set_opsize32 << set_oc_reg r << primary 0x50
      [R64 r] -> set_opsize64 << set_oc_reg r << primary 0x50

      -- dead code because of the smaller forms above
      -- [R16 r] -> set_opsize16 << set_rm_ext_reg 0x6 r << primary 0xFF
      -- [R32 r] -> set_opsize32 << set_rm_ext_reg 0x6 r << primary 0xFF
      -- [R64 r] -> set_opsize64 << set_rm_ext_reg 0x6 r << primary 0xFF

      [M16 m] -> set_opsize16 << set_rm_ext_mem 0x6 m << primary 0xFF
      [M32 m] -> set_opsize32 << set_rm_ext_mem 0x6 m << primary 0xFF
      [M64 m] -> set_opsize64 << set_rm_ext_mem 0x6 m << primary 0xFF
      [I8 i]  ->                set_imm8 i           << primary 0x6A
      [I16 i] -> set_opsize16 << set_imm16 i          << primary 0x68
      [I32 i] -> set_opsize32 << set_imm32 i          << primary 0x68
      [OpSeg CS] | not mode64 -> primary 0x0E
      [OpSeg SS] | not mode64 -> primary 0x16
      [OpSeg DS] | not mode64 -> primary 0x1E
      [OpSeg ES] | not mode64 -> primary 0x06
      [OpSeg FS]              -> map_0F 0xA0
      [OpSeg GS]              -> map_0F 0xA8
      _ -> invalidArgs

    POP -> case args of
      -- smaller forms for registers
      [R16 r] -> set_opsize16 << set_oc_reg r << primary 0x58
      [R32 r] -> set_opsize32 << set_oc_reg r << primary 0x58
      [R64 r] -> set_opsize64 << set_oc_reg r << primary 0x58

      -- dead code because of the smaller forms above
      -- [R16 r] -> set_opsize16 << set_rm_ext_reg 0x0 r << primary 0x8F
      -- [R32 r] -> set_opsize32 << set_rm_ext_reg 0x0 r << primary 0x8F
      -- [R64 r] -> set_opsize64 << set_rm_ext_reg 0x0 r << primary 0x8F

      [M16 m] -> set_opsize16 << set_rm_ext_mem 0x0 m << primary 0x8F
      [M32 m] -> set_opsize32 << set_rm_ext_mem 0x0 m << primary 0x8F
      [M64 m] -> set_opsize64 << set_rm_ext_mem 0x0 m << primary 0x8F
      -- TODO: the operand size can be used to select the stack increment. How
      -- do we provide this to users?
      [OpSeg SS] | not mode64 -> primary 0x17
      [OpSeg DS] | not mode64 -> primary 0x1F
      [OpSeg ES] | not mode64 -> primary 0x07
      [OpSeg FS]              -> map_0F 0xA1
      [OpSeg GS]              -> map_0F 0xA9
      _ -> invalidArgs

    POPA sz -> do
      assert_not_mode64
      assert_no_args
      case sz of
        OpSize16 -> set_opsize16 << primary 0x61
        OpSize32 -> set_opsize32 << primary 0x61
        _        -> invalidArgs

    PUSHA sz -> do
      assert_not_mode64
      assert_no_args
      case sz of
        OpSize16 -> set_opsize16 << primary 0x60
        OpSize32 -> set_opsize32 << primary 0x60
        _        -> invalidArgs

    LDS -> do
      assert_not_mode64
      case args of
        [R16 r, OpMem m] -> set_opsize16 << set_rm_reg_mem r m << primary 0xC5
        [R32 r, OpMem m] -> set_opsize32 << set_rm_reg_mem r m << primary 0xC5
        _ -> invalidArgs

    LES -> do
      assert_not_mode64
      case args of
        [R16 r, OpMem m] -> set_opsize16 << set_rm_reg_mem r m << primary 0xC4
        [R32 r, OpMem m] -> set_opsize32 << set_rm_reg_mem r m << primary 0xC4
        _ -> invalidArgs

    LSS -> do
      case args of
        [R16 r, OpMem m] -> set_opsize16 << set_rm_reg_mem r m << map_0F 0xB2
        [R32 r, OpMem m] -> set_opsize32 << set_rm_reg_mem r m << map_0F 0xB2
        [R64 r, OpMem m] -> set_opsize64 << set_rm_reg_mem r m << map_0F 0xB2
        _ -> invalidArgs

    LFS -> do
      case args of
        [R16 r, OpMem m] -> set_opsize16 << set_rm_reg_mem r m << map_0F 0xB4
        [R32 r, OpMem m] -> set_opsize32 << set_rm_reg_mem r m << map_0F 0xB4
        [R64 r, OpMem m] -> set_opsize64 << set_rm_reg_mem r m << map_0F 0xB4
        _ -> invalidArgs

    LGS -> do
      case args of
        [R16 r, OpMem m] -> set_opsize16 << set_rm_reg_mem r m << map_0F 0xB5
        [R32 r, OpMem m] -> set_opsize32 << set_rm_reg_mem r m << map_0F 0xB5
        [R64 r, OpMem m] -> set_opsize64 << set_rm_reg_mem r m << map_0F 0xB5
        _ -> invalidArgs

    Jcc cc -> case args of
      [I8  i] -> set_imm8 i << primary (0x70 + condCode cc)
      [I16 i]
        | not mode64 -- not supported in 64-bit mode
        -> set_opsize16 << set_imm16 i << map_0F (0x80 + condCode cc)
      [I32 i] -> set_opsize32 << set_imm32 i << map_0F (0x80 + condCode cc)
      _       -> invalidArgs

    LOOP -> case args of
      [R16 R_CX,  I8 i] -> set_addrsize AddrSize16 << set_imm8 i << primary 0xE2
      [R32 R_ECX, I8 i] -> set_addrsize AddrSize32 << set_imm8 i << primary 0xE2
      [R64 R_RCX, I8 i] -> set_addrsize AddrSize64 << set_imm8 i << primary 0xE2
      _ -> invalidArgs

    LOOPE -> case args of
      [R16 R_CX,  I8 i] -> set_addrsize AddrSize16 << set_imm8 i << primary 0xE1
      [R32 R_ECX, I8 i] -> set_addrsize AddrSize32 << set_imm8 i << primary 0xE1
      [R64 R_RCX, I8 i] -> set_addrsize AddrSize64 << set_imm8 i << primary 0xE1
      _ -> invalidArgs

    LOOPNE -> case args of
      [R16 R_CX,  I8 i] -> set_addrsize AddrSize16 << set_imm8 i << primary 0xE0
      [R32 R_ECX, I8 i] -> set_addrsize AddrSize32 << set_imm8 i << primary 0xE0
      [R64 R_RCX, I8 i] -> set_addrsize AddrSize64 << set_imm8 i << primary 0xE0
      _ -> invalidArgs

    JMP -> case args of
      [I8  i] -> set_imm8 i << primary 0xEB
      [I16 i]
        | not mode64
        -> set_opsize16 << set_imm16 i << primary 0xE9
      [I32 i] -> set_opsize32 << set_imm32 i << primary 0xE9
      [R16 r]
        | not mode64
        -> set_opsize16 << set_rm_ext_reg 0x4 r << primary 0xFF
      [R32 r]
        | not mode64
        -> set_opsize32 << set_rm_ext_reg 0x4 r << primary 0xFF
      [R64 r]
        | mode64 -- default to 64-bit size
        -> set_rm_ext_reg 0x4 r << primary 0xFF
      [M16 m]
        | not mode64
        -> set_opsize16 << set_rm_ext_mem 0x4 m << primary 0xFF
      [M32 m]
        | not mode64
        -> set_opsize32 << set_rm_ext_mem 0x4 m << primary 0xFF
      [M64 m]
        | mode64 -- default to 64-bit size
        -> set_rm_ext_mem 0x4 m << primary 0xFF
      -- TODO: other forms: ptr k:n, m k:n
      _       -> invalidArgs

    INTO -> do
      assert_not_mode64
      assert_no_args
      primary 0xCE
    INT1 -> assert_no_args >> primary 0xF1
    INT3 -> assert_no_args >> primary 0xCC
    INT  -> do
      i <- imm8_arg
      set_imm8 i << primary 0xCD

    SYSCALL -> do
      assert_mode64
      assert_no_args
      map_0F 0x05

    SYSRET comp_mode -> do
      assert_mode64
      assert_no_args
      -- do we switch to 32-bit compatibility mode?
      if comp_mode
        then                 map_0F 0x07
        else set_opsize64 << map_0F 0x07

    SYSENTER -> do
      assert_no_args
      map_0F 0x34

    SYSEXIT comp_mode -> do
      assert_no_args
      -- do we switch to 32-bit compatibility mode?
      if comp_mode
        then                 map_0F 0x35
        else set_opsize64 << map_0F 0x35

    LEAVE osz -> do
      assert_no_args
      case osz of
        OpSize64 | mode64     -> primary 0xC9
        OpSize32 | not mode64 -> set_opsize32 << primary 0xC9
        OpSize16              -> set_opsize16 << primary 0xC9
        _ -> invalidArgs

    RET -> case args of
      []      -> primary 0xC3
      [I16 i] -> set_imm16 i << primary 0xC2
      _       -> invalidArgs

    RET_FAR -> case args of
      []      -> primary 0xCB
      [I16 i] -> set_imm16 i << primary 0xCA
      _       -> invalidArgs

    CALL -> case args of
      [I16 i] -> set_opsize16 << set_imm16 i << primary 0xE8
      [I32 i] -> set_opsize32 << set_imm32 i << primary 0xE8
      [R16 r] | not mode64 -> set_opsize16 << set_rm_ext_reg 0x2 r << primary 0xFF
      [R32 r] | not mode64 -> set_opsize32 << set_rm_ext_reg 0x2 r << primary 0xFF
      [M16 m] | not mode64 -> set_opsize16 << set_rm_ext_mem 0x2 m << primary 0xFF
      [M32 m] | not mode64 -> set_opsize32 << set_rm_ext_mem 0x2 m << primary 0xFF
      -- default to 64-bit operand size in 64-bit mode
      [R64 r]              ->                 set_rm_ext_reg 0x2 r << primary 0xFF
      [M64 m]              ->                 set_rm_ext_mem 0x2 m << primary 0xFF
      _ -> invalidArgs

    CALL_FAR -> noEncoding -- TODO: far CALLs

    UD0 -> case args of
      [R32 r, R32 rm] -> set_rm_reg_reg r rm << map_0F 0xFF
      [R32 r, M32 rm] -> set_rm_reg_mem r rm << map_0F 0xFF
      _ -> invalidArgs

    UD1 -> case args of
      [R32 r, R32 rm] -> set_rm_reg_reg r rm << map_0F 0xB9
      [R32 r, M32 rm] -> set_rm_reg_mem r rm << map_0F 0xB9
      _ -> invalidArgs

    UD2 -> do
      assert_no_args
      map_0F 0x0B

    CMC -> assert_no_args >> primary 0xF5
    CLC -> assert_no_args >> primary 0xF8
    STC -> assert_no_args >> primary 0xF9
    CLI -> assert_no_args >> primary 0xFA
    STI -> assert_no_args >> primary 0xFB
    CLD -> assert_no_args >> primary 0xFC
    STD -> assert_no_args >> primary 0xFD

    LAHF -> assert_no_args >> primary 0x9F
    SAHF -> assert_no_args >> primary 0x9E

    POPF sz -> do
      assert_no_args
      case sz of
        OpSize8  -> efail EInvalidOpSize8
        OpSize16 -> set_opsize16 << primary 0x9D
        OpSize32 -> set_opsize32 << primary 0x9D
        OpSize64 -> set_opsize64 << primary 0x9D

    PUSHF sz -> do
      assert_no_args
      case sz of
        OpSize8  -> efail EInvalidOpSize8
        OpSize16 -> set_opsize16 << primary 0x9C
        OpSize32 -> set_opsize32 << primary 0x9C
        OpSize64 -> set_opsize64 << primary 0x9C

    CPUID -> do
      assert_no_args
      map_0F 0xA2

    PAUSE -> do
      assert_no_args
      prefix_F3 << primary 0x90

    NOP sz -> do
      let mem a = Mem Nothing Nothing NoLock a Nothing
      let ra = if mode64 then Just R_RAX else Just R_EAX
      case sz of
        1 ->              primary 0x90
        2 -> prefix_66 << primary 0x90
        -- multi-byte NOPs as recommended in Intel manual
        -- Not supported on all models
        3 ->              set_rm_ext_mem 0x0 (mem (MemAbs ra Scale1 Nothing NoDisp))     << map_0F 0x1F
        4 ->              set_rm_ext_mem 0x0 (mem (MemAbs ra Scale1 Nothing (Disp8 0)))  << map_0F 0x1F
        5 ->              set_rm_ext_mem 0x0 (mem (MemAbs ra Scale1 ra      (Disp8 0)))  << map_0F 0x1F
        6 -> prefix_66 << set_rm_ext_mem 0x0 (mem (MemAbs ra Scale1 ra      (Disp8 0)))  << map_0F 0x1F
        7 ->              set_rm_ext_mem 0x0 (mem (MemAbs ra Scale1 Nothing (Disp32 0))) << map_0F 0x1F
        8 ->              set_rm_ext_mem 0x0 (mem (MemAbs ra Scale1 ra      (Disp32 0))) << map_0F 0x1F
        9 -> prefix_66 << set_rm_ext_mem 0x0 (mem (MemAbs ra Scale1 ra      (Disp32 0))) << map_0F 0x1F
        _ -> invalidArgs

    MWAIT -> do
      assert_no_args
      set_modrm 0xC9 << map_0F 0x01

    ADCX -> do
      require_extension Ext.ADX
      case args of
        [R32 r, M32 m] -> do
          set_rm_reg_mem r m << prefix_66 << map_0F38 0xF6
        [R64 r, M64 m] -> do
          assert_mode64
          set_opsize64 << set_rm_reg_mem r m << prefix_66 << map_0F38 0xF6
        [R32 r, R32 rm] -> do
          set_rm_reg_reg r rm << prefix_66 << map_0F38 0xF6
        [R64 r, R64 rm] -> do
          assert_mode64
          set_opsize64 << set_rm_reg_reg r rm << prefix_66 << map_0F38 0xF6
        _ -> invalidArgs

    ADOX -> do
      require_extension Ext.ADX
      case args of
        [R32 r, M32 m] -> do
          set_rm_reg_mem r m << prefix_F3 << map_0F38 0xF6
        [R64 r, M64 m] -> do
          assert_mode64
          set_opsize64 << set_rm_reg_mem r m << prefix_F3 << map_0F38 0xF6
        [R32 r, R32 rm] -> do
          set_rm_reg_reg r rm << prefix_F3 << map_0F38 0xF6
        [R64 r, R64 rm] -> do
          assert_mode64
          set_opsize64 << set_rm_reg_reg r rm << prefix_F3 << map_0F38 0xF6
        _ -> invalidArgs

    STR -> case args of
      [M16 m] -> set_rm_ext_mem 0x1 m << map_0F 0x00
      -- it is zero-extended to the whole register.
      [R16 r] -> set_rm_ext_reg 0x1 r << map_0F 0x00
      _ -> invalidArgs

    LTR -> case args of
      [M16 m] -> set_rm_ext_mem 0x3 m << map_0F 0x00
      -- it is zero-extended to the whole register.
      [R16 r] -> set_rm_ext_reg 0x3 r << map_0F 0x00
      _ -> invalidArgs

    SGDT -> do
      case args of
        [OpMem m] -> set_rm_ext_mem 0x0 m << map_0F 0x01
        _ -> invalidArgs

    LGDT -> do
      case args of
        [OpMem m] -> set_rm_ext_mem 0x2 m << map_0F 0x01
        _ -> invalidArgs

    SLDT -> do
      case args of
        [M16 m] -> set_rm_ext_mem 0x0 m << map_0F 0x00
        [R16 r] -> set_rm_ext_reg 0x0 r << map_0F 0x00
        _ -> invalidArgs

    LLDT -> do
      case args of
        [M16 m] -> set_rm_ext_mem 0x2 m << map_0F 0x00
        [R16 r] -> set_rm_ext_reg 0x2 r << map_0F 0x00
        _ -> invalidArgs

    SIDT -> do
      case args of
        [OpMem m] -> set_rm_ext_mem 0x1 m << map_0F 0x01
        _ -> invalidArgs

    LIDT -> do
      case args of
        [OpMem m] -> set_rm_ext_mem 0x3 m << map_0F 0x01
        _ -> invalidArgs

    RDFSBASE -> do
      assert_mode64
      require_extension Ext.FSGSBASE
      case args of
        [R32 r] ->                 set_rm_ext_reg 0x0 r << prefix_F3 << map_0F 0xAE
        [R64 r] -> set_opsize64 << set_rm_ext_reg 0x0 r << prefix_F3 << map_0F 0xAE
        _ -> invalidArgs

    RDGSBASE -> do
      assert_mode64
      require_extension Ext.FSGSBASE
      case args of
        [R32 r] ->                 set_rm_ext_reg 0x1 r << prefix_F3 << map_0F 0xAE
        [R64 r] -> set_opsize64 << set_rm_ext_reg 0x1 r << prefix_F3 << map_0F 0xAE
        _ -> invalidArgs

    WRFSBASE -> do
      assert_mode64
      require_extension Ext.FSGSBASE
      case args of
        [R32 r] ->                 set_rm_ext_reg 0x2 r << prefix_F3 << map_0F 0xAE
        [R64 r] -> set_opsize64 << set_rm_ext_reg 0x2 r << prefix_F3 << map_0F 0xAE
        _ -> invalidArgs

    WRGSBASE -> do
      assert_mode64
      require_extension Ext.FSGSBASE
      case args of
        [R32 r] ->                 set_rm_ext_reg 0x3 r << prefix_F3 <<  map_0F 0xAE
        [R64 r] -> set_opsize64 << set_rm_ext_reg 0x3 r << prefix_F3 <<  map_0F 0xAE
        _ -> invalidArgs

    SWAPGS -> do
      assert_mode64
      assert_no_args
      set_modrm 0xF8 << map_0F 0x01

    RDPID -> do
      require_extension Ext.RDPID
      case args of
        [R32 r] | not mode64 -> set_rm_ext_reg 0x7 r << prefix_F3 << map_0F 0xC7
        [R64 r] |     mode64 -> set_rm_ext_reg 0x7 r << prefix_F3 << map_0F 0xC7
        _ -> invalidArgs

    RDTSC -> do
      assert_no_args
      map_0F 0x31

    RDTSCP -> do
      assert_no_args
      set_modrm 0xF9 << map_0F 0x01

    LSL -> case args of
      [R16 d, R16 s] -> set_opsize16 << set_rm_reg_reg d s << map_0F 0x03
      [R16 d, M16 s] -> set_opsize16 << set_rm_reg_mem d s << map_0F 0x03
      [R32 d, R32 s] -> set_opsize32 << set_rm_reg_reg d s << map_0F 0x03
      [R32 d, M16 s] -> set_opsize32 << set_rm_reg_mem d s << map_0F 0x03
      [R64 d, R32 s] -> set_opsize64 << set_rm_reg_reg d s << map_0F 0x03
      [R64 d, M16 s] -> set_opsize64 << set_rm_reg_mem d s << map_0F 0x03
      _ -> invalidArgs

    PREFETCHW -> do
      require_extension Ext.PREFETCHW
      case args of
        [OpMem m] -> set_rm_ext_mem 0x1 m << map_0F 0x0D
        _ -> invalidArgs

    MFENCE -> do
      require_extension Ext.SSE2
      assert_no_args
      -- Can be encoded as 0F AE Fx, where x in the range 0-7 (i.e. ModRM.rm
      -- field is ignored). We use 0F AE F0.
      set_modrm 0xF0 << map_0F 0xAE

    SFENCE -> do
      require_extension Ext.SSE
      assert_no_args
      -- Can be encoded as 0F AE Fx, where x in the range 8-F (i.e. ModRM.rm
      -- field is ignored). We use 0F AE F8.
      set_modrm 0xF8 << map_0F 0xAE

    LFENCE -> do
      require_extension Ext.SSE2
      assert_no_args
      -- Can be encoded as 0F AE Ex, where x in the range 8-F (i.e. ModRM.rm
      -- field is ignored). We use 0F AE E8.
      set_modrm 0xE8 << map_0F 0xAE


    ADDPS ->
      case args of
        [V128 v, M128 m] -> do
          require_extension Ext.SSE
          set_rm_vec_mem v m << map_0F 0x58
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE
          set_rm_vec_vec v1 v2 << map_0F 0x58
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs

    ADDPD ->
      case args of
        [V128 v, M128 m] -> do
          require_extension Ext.SSE2
          set_rm_vec_mem v m << prefix_66 << map_0F 0x58
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE2
          set_rm_vec_vec v1 v2 << prefix_66 << map_0F 0x58
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs

    ADDSS ->
      case args of
        [V128 v, M32 m] -> do
          require_extension Ext.SSE
          set_rm_vec_mem v m << prefix_F3 << map_0F 0x58
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE
          set_rm_vec_vec v1 v2 << prefix_F3 << map_0F 0x58
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs

    ADDSD ->
      case args of
        [V128 v, M32 m] -> do
          require_extension Ext.SSE2
          set_rm_vec_mem v m << prefix_F2 << map_0F 0x58
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE2
          set_rm_vec_vec v1 v2 << prefix_F2 << map_0F 0x58
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs

    SUBPS ->
      case args of
        [V128 v, M128 m] -> do
          require_extension Ext.SSE
          set_rm_vec_mem v m << map_0F 0x5C
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE
          set_rm_vec_vec v1 v2 << map_0F 0x5C
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs

    SUBPD ->
      case args of
        [V128 v, M128 m] -> do
          require_extension Ext.SSE2
          set_rm_vec_mem v m << prefix_66 << map_0F 0x5C
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE2
          set_rm_vec_vec v1 v2 << prefix_66 << map_0F 0x5C
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs

    SUBSS ->
      case args of
        [V128 v, M32 m] -> do
          require_extension Ext.SSE
          set_rm_vec_mem v m << prefix_F3 << map_0F 0x5C
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE
          set_rm_vec_vec v1 v2 << prefix_F3 << map_0F 0x5C
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs

    SUBSD ->
      case args of
        [V128 v, M32 m] -> do
          require_extension Ext.SSE2
          set_rm_vec_mem v m << prefix_F2 << map_0F 0x5C
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE2
          set_rm_vec_vec v1 v2 << prefix_F2 << map_0F 0x5C
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs
