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

      oc       o = modifyEnc \e -> e { encOpcode = Just (Op o) }
      oc_0F    o = modifyEnc \e -> e { encOpcode = Just (Op_0F o) }
      oc_0F38  o = modifyEnc \e -> e { encOpcode = Just (Op_0F38 o) }

      p_67 = modifyEnc \e -> e { encPrefixes = P_67 : encPrefixes e }
      p_66 = modifyEnc \e -> e { encPrefixes = P_66 : encPrefixes e }
      p_F3 = modifyEnc \e -> e { encPrefixes = P_F3 : encPrefixes e }
      p_F2 = modifyEnc \e -> e { encPrefixes = P_F2 : encPrefixes e }

      imm8  i = set_imm (SizedValue8 i)
      imm16 i = set_imm (SizedValue16 i)
      imm32 i = set_imm (SizedValue32 i)
      imm64 i = set_imm (SizedValue64 i)

      set_imm imm = modifyEnc \e -> e { encImm = Just imm }

      set_modrm v = modifyEnc \e -> e { encModRM = Just (ModRM v) }

      set_segment ms = case ms of
        Nothing -> pure ()
        Just s  -> modifyEnc \e -> e { encPrefixes = segmentOverridePrefix s : encPrefixes e }

      osz16 = case defaultOperationSize ctx of
        OpSize16 -> pure ()
        OpSize32 -> p_66
        OpSize8  -> efail EInvalidOpSize16
        OpSize64 -> efail EInvalidOpSize16

      osz32 = case defaultOperationSize ctx of
        OpSize32 -> pure ()
        OpSize16 -> p_66
        OpSize8  -> efail EInvalidOpSize32
        OpSize64 -> efail EInvalidOpSize32

      osz64 = modifyEnc \e -> e { encRex = encRex e <> Just rexW }

      asz16 = set_addrsize AddrSize16
      asz32 = set_addrsize AddrSize32
      asz64 = set_addrsize AddrSize64

      set_addrsize asz
        | overriddenAddressSize False ctx == asz = pure ()
        | overriddenAddressSize True  ctx == asz = p_67
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
      oc_reg r = do
        let !(xc,c) = regCodeX r
            -- handle registers that require REX
            mrex1 = if regREX r then Just emptyRex else Nothing
            -- register code extension in REX.B
            mrex2 = if xc       then Just rexB     else Nothing
        modifyEnc \e ->
          let opc = case encOpcode e of
                      Nothing -> error "oc_reg: expected an opcode"
                      Just o0 -> case o0 of
                        Op      o -> Op      (o+c)
                        Op_0F   o -> Op_0F   (o+c)
                        Op_0F38 o -> Op_0F38 (o+c)
                        Op_0F3A o -> Op_0F3A (o+c)
                        Op_0F0F o -> Op_0F0F (o+c)
                        _ -> error $ "oc_reg: unexpected opcode: " ++ show o0
          in e { encRex = mrex1 <> mrex2
               , encOpcode = Just opc
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
      rm_ext_reg x r = set_r_ext x >> set_m_gpr r
      rm_reg_mem r m = set_r_gpr r >> set_m_mem m
      rm_ext_mem x m = set_r_ext x >> set_m_mem m
      rm_vec_mem v m = set_r_vec v >> set_m_mem m

      -- store v1 in ModRM.reg and v2 in ModRM.rm
      rm_vec_vec v1 v2 = do
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
      rm_reg_reg r1 r2 = do
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
      mr_reg_reg r1 r2 = rm_reg_reg r2 r1

  runE case op of
    AAA -> do
      assert_not_mode64
      oc 0x37
      assert_no_args

    AAS -> do
      assert_not_mode64
      oc 0x3F
      assert_no_args

    AAD -> do
      assert_not_mode64
      case args of
        [I8 i] -> imm8 i << oc 0xD5
        _ -> invalidArgs

    AAM -> do
      assert_not_mode64
      case args of
        [I8 i] -> imm8 i << oc 0xD4
        _ -> invalidArgs

    DAA -> do
      assert_not_mode64
      oc 0x27
      assert_no_args

    DAS -> do
      assert_not_mode64
      oc 0x2F
      assert_no_args

    ADC -> case args of
      [R8 R_AL,   I8  i] ->          imm8  i << oc 0x14
      [R16 R_AX,  I16 i] -> osz16 << imm16 i << oc 0x15
      [R32 R_EAX, I32 i] -> osz32 << imm32 i << oc 0x15
      [R64 R_RAX, I32 i] -> osz64 << imm32 i << oc 0x15
      [R8  r,     I8  i] ->          rm_ext_reg 0x2 r << imm8  i << oc 0x80
      [R16 r,     I16 i] -> osz16 << rm_ext_reg 0x2 r << imm16 i << oc 0x81
      [R32 r,     I32 i] -> osz32 << rm_ext_reg 0x2 r << imm32 i << oc 0x81
      [R64 r,     I32 i] -> osz64 << rm_ext_reg 0x2 r << imm32 i << oc 0x81
      [M8  m,     I8  i] ->          rm_ext_mem 0x2 m << imm8  i << oc 0x80
      [M16 m,     I16 i] -> osz16 << rm_ext_mem 0x2 m << imm16 i << oc 0x81
      [M32 m,     I32 i] -> osz32 << rm_ext_mem 0x2 m << imm32 i << oc 0x81
      [M64 m,     I32 i] -> osz64 << rm_ext_mem 0x2 m << imm32 i << oc 0x81
      [R16 r,     I8  i] -> osz16 << rm_ext_reg 0x2 r << imm8 i << oc 0x83
      [R32 r,     I8  i] -> osz32 << rm_ext_reg 0x2 r << imm8 i << oc 0x83
      [R64 r,     I8  i] -> osz64 << rm_ext_reg 0x2 r << imm8 i << oc 0x83
      [M16 m,     I8  i] -> osz16 << rm_ext_mem 0x2 m << imm8 i << oc 0x83
      [M32 m,     I8  i] -> osz32 << rm_ext_mem 0x2 m << imm8 i << oc 0x83
      [M64 m,     I8  i] -> osz64 << rm_ext_mem 0x2 m << imm8 i << oc 0x83
      [R8  r1, R8  r2] ->          mr_reg_reg r1 r2 << oc 0x10
      [R16 r1, R16 r2] -> osz16 << mr_reg_reg r1 r2 << oc 0x11
      [R32 r1, R32 r2] -> osz32 << mr_reg_reg r1 r2 << oc 0x11
      [R64 r1, R64 r2] -> osz64 << mr_reg_reg r1 r2 << oc 0x11
      [M8 m,   R8   r] ->          rm_reg_mem r m   << oc 0x10
      [M16 m,  R16  r] -> osz16 << rm_reg_mem r m   << oc 0x11
      [M32 m,  R32  r] -> osz32 << rm_reg_mem r m   << oc 0x11
      [M64 m,  R64  r] -> osz64 << rm_reg_mem r m   << oc 0x11
      [R8  r1, R8  r2] ->          rm_reg_reg r1 r2 << oc 0x12
      [R16 r1, R16 r2] -> osz16 << rm_reg_reg r1 r2 << oc 0x13
      [R32 r1, R32 r2] -> osz32 << rm_reg_reg r1 r2 << oc 0x13
      [R64 r1, R64 r2] -> osz64 << rm_reg_reg r1 r2 << oc 0x13
      [R8  r,  M8   m] ->          rm_reg_mem r m   << oc 0x12
      [R16 r,  M16  m] -> osz16 << rm_reg_mem r m   << oc 0x13
      [R32 r,  M32  m] -> osz32 << rm_reg_mem r m   << oc 0x13
      [R64 r,  M64  m] -> osz64 << rm_reg_mem r m   << oc 0x13
      _ -> invalidArgs

    ADD -> case args of
        [R8 R_AL,   I8  i] ->          imm8  i << oc 0x04
        [R16 R_AX,  I16 i] -> osz16 << imm16 i << oc 0x05
        [R32 R_EAX, I32 i] -> osz32 << imm32 i << oc 0x05
        [R64 R_RAX, I32 i] -> osz64 << imm32 i << oc 0x05
        [R8  r,     I8  i] ->          rm_ext_reg 0x0 r << imm8  i << oc 0x80
        [R16 r,     I16 i] -> osz16 << rm_ext_reg 0x0 r << imm16 i << oc 0x81
        [R32 r,     I32 i] -> osz32 << rm_ext_reg 0x0 r << imm32 i << oc 0x81
        [R64 r,     I32 i] -> osz64 << rm_ext_reg 0x0 r << imm32 i << oc 0x81
        [M8  m,     I8  i] ->          rm_ext_mem 0x0 m << imm8  i << oc 0x80
        [M16 m,     I16 i] -> osz16 << rm_ext_mem 0x0 m << imm16 i << oc 0x81
        [M32 m,     I32 i] -> osz32 << rm_ext_mem 0x0 m << imm32 i << oc 0x81
        [M64 m,     I32 i] -> osz64 << rm_ext_mem 0x0 m << imm32 i << oc 0x81
        [R16 r,     I8  i] -> osz16 << rm_ext_reg 0x0 r << imm8 i << oc 0x83
        [R32 r,     I8  i] -> osz32 << rm_ext_reg 0x0 r << imm8 i << oc 0x83
        [R64 r,     I8  i] -> osz64 << rm_ext_reg 0x0 r << imm8 i << oc 0x83
        [M16 m,     I8  i] -> osz16 << rm_ext_mem 0x0 m << imm8 i << oc 0x83
        [M32 m,     I8  i] -> osz32 << rm_ext_mem 0x0 m << imm8 i << oc 0x83
        [M64 m,     I8  i] -> osz64 << rm_ext_mem 0x0 m << imm8 i << oc 0x83
        [R8  r1, R8  r2] ->          mr_reg_reg r1 r2 << oc 0x00
        [R16 r1, R16 r2] -> osz16 << mr_reg_reg r1 r2 << oc 0x01
        [R32 r1, R32 r2] -> osz32 << mr_reg_reg r1 r2 << oc 0x01
        [R64 r1, R64 r2] -> osz64 << mr_reg_reg r1 r2 << oc 0x01
        [M8 m,   R8   r] ->          rm_reg_mem r m   << oc 0x00
        [M16 m,  R16  r] -> osz16 << rm_reg_mem r m   << oc 0x01
        [M32 m,  R32  r] -> osz32 << rm_reg_mem r m   << oc 0x01
        [M64 m,  R64  r] -> osz64 << rm_reg_mem r m   << oc 0x01
        [R8  r1, R8  r2] ->          rm_reg_reg r1 r2 << oc 0x02
        [R16 r1, R16 r2] -> osz16 << rm_reg_reg r1 r2 << oc 0x03
        [R32 r1, R32 r2] -> osz32 << rm_reg_reg r1 r2 << oc 0x03
        [R64 r1, R64 r2] -> osz64 << rm_reg_reg r1 r2 << oc 0x03
        [R8  r,  M8   m] ->          rm_reg_mem r m   << oc 0x02
        [R16 r,  M16  m] -> osz16 << rm_reg_mem r m   << oc 0x03
        [R32 r,  M32  m] -> osz32 << rm_reg_mem r m   << oc 0x03
        [R64 r,  M64  m] -> osz64 << rm_reg_mem r m   << oc 0x03
        _ -> invalidArgs

    TEST -> case args of
        [R8 R_AL,   I8  i] ->          imm8  i << oc 0xA8
        [R16 R_AX,  I16 i] -> osz16 << imm16 i << oc 0xA9
        [R32 R_EAX, I32 i] -> osz32 << imm32 i << oc 0xA9
        [R64 R_RAX, I32 i] -> osz64 << imm32 i << oc 0xA9
        [R8  r,     I8  i] ->          rm_ext_reg 0x0 r << imm8  i << oc 0xF6
        [R16 r,     I16 i] -> osz16 << rm_ext_reg 0x0 r << imm16 i << oc 0xF7
        [R32 r,     I32 i] -> osz32 << rm_ext_reg 0x0 r << imm32 i << oc 0xF7
        [R64 r,     I32 i] -> osz64 << rm_ext_reg 0x0 r << imm32 i << oc 0xF7
        [M8  m,     I8  i] ->          rm_ext_mem 0x0 m << imm8  i << oc 0xF6
        [M16 m,     I16 i] -> osz16 << rm_ext_mem 0x0 m << imm16 i << oc 0xF7
        [M32 m,     I32 i] -> osz32 << rm_ext_mem 0x0 m << imm32 i << oc 0xF7
        [M64 m,     I32 i] -> osz64 << rm_ext_mem 0x0 m << imm32 i << oc 0xF7
        [R8  r1,   R8  r2] ->          mr_reg_reg r1 r2 << oc 0x84
        [R16 r1,   R16 r2] -> osz16 << mr_reg_reg r1 r2 << oc 0x85
        [R32 r1,   R32 r2] -> osz32 << mr_reg_reg r1 r2 << oc 0x85
        [R64 r1,   R64 r2] -> osz64 << mr_reg_reg r1 r2 << oc 0x85
        [M8 m,     R8   r] ->          rm_reg_mem r m   << oc 0x84
        [M16 m,    R16  r] -> osz16 << rm_reg_mem r m   << oc 0x85
        [M32 m,    R32  r] -> osz32 << rm_reg_mem r m   << oc 0x85
        [M64 m,    R64  r] -> osz64 << rm_reg_mem r m   << oc 0x85
        _ -> invalidArgs

    CMP -> case args of
        [R8 R_AL,   I8  i] ->          imm8  i << oc 0x3C
        [R16 R_AX,  I16 i] -> osz16 << imm16 i << oc 0x3D
        [R32 R_EAX, I32 i] -> osz32 << imm32 i << oc 0x3D
        [R64 R_RAX, I32 i] -> osz64 << imm32 i << oc 0x3D
        [R8  r,     I8  i] ->          rm_ext_reg 0x7 r << imm8  i << oc 0x80
        [R16 r,     I16 i] -> osz16 << rm_ext_reg 0x7 r << imm16 i << oc 0x81
        [R32 r,     I32 i] -> osz32 << rm_ext_reg 0x7 r << imm32 i << oc 0x81
        [R64 r,     I32 i] -> osz64 << rm_ext_reg 0x7 r << imm32 i << oc 0x81
        [M8  m,     I8  i] ->          rm_ext_mem 0x7 m << imm8  i << oc 0x80
        [M16 m,     I16 i] -> osz16 << rm_ext_mem 0x7 m << imm16 i << oc 0x81
        [M32 m,     I32 i] -> osz32 << rm_ext_mem 0x7 m << imm32 i << oc 0x81
        [M64 m,     I32 i] -> osz64 << rm_ext_mem 0x7 m << imm32 i << oc 0x81
        [R16 r,     I8  i] -> osz16 << rm_ext_reg 0x7 r << imm8 i << oc 0x83
        [R32 r,     I8  i] -> osz32 << rm_ext_reg 0x7 r << imm8 i << oc 0x83
        [R64 r,     I8  i] -> osz64 << rm_ext_reg 0x7 r << imm8 i << oc 0x83
        [M16 m,     I8  i] -> osz16 << rm_ext_mem 0x7 m << imm8 i << oc 0x83
        [M32 m,     I8  i] -> osz32 << rm_ext_mem 0x7 m << imm8 i << oc 0x83
        [M64 m,     I8  i] -> osz64 << rm_ext_mem 0x7 m << imm8 i << oc 0x83
        [R8  r1, R8  r2] ->          mr_reg_reg r1 r2 << oc 0x38
        [R16 r1, R16 r2] -> osz16 << mr_reg_reg r1 r2 << oc 0x39
        [R32 r1, R32 r2] -> osz32 << mr_reg_reg r1 r2 << oc 0x39
        [R64 r1, R64 r2] -> osz64 << mr_reg_reg r1 r2 << oc 0x39
        [M8 m,   R8   r] ->          rm_reg_mem r m   << oc 0x38
        [M16 m,  R16  r] -> osz16 << rm_reg_mem r m   << oc 0x39
        [M32 m,  R32  r] -> osz32 << rm_reg_mem r m   << oc 0x39
        [M64 m,  R64  r] -> osz64 << rm_reg_mem r m   << oc 0x39
        [R8  r1, R8  r2] ->          rm_reg_reg r1 r2 << oc 0x3A
        [R16 r1, R16 r2] -> osz16 << rm_reg_reg r1 r2 << oc 0x3B
        [R32 r1, R32 r2] -> osz32 << rm_reg_reg r1 r2 << oc 0x3B
        [R64 r1, R64 r2] -> osz64 << rm_reg_reg r1 r2 << oc 0x3B
        [R8  r,  M8   m] ->          rm_reg_mem r m   << oc 0x3A
        [R16 r,  M16  m] -> osz16 << rm_reg_mem r m   << oc 0x3B
        [R32 r,  M32  m] -> osz32 << rm_reg_mem r m   << oc 0x3B
        [R64 r,  M64  m] -> osz64 << rm_reg_mem r m   << oc 0x3B
        _ -> invalidArgs

    CMPXCHG -> case args of
      [R8  rm, R8  r] ->          rm_reg_reg r rm << oc_0F 0xB0
      [M8  rm, R8  r] ->          rm_reg_mem r rm << oc_0F 0xB0
      [R16 rm, R16 r] -> osz16 << rm_reg_reg r rm << oc_0F 0xB1
      [M16 rm, R16 r] -> osz16 << rm_reg_mem r rm << oc_0F 0xB1
      [R32 rm, R32 r] -> osz32 << rm_reg_reg r rm << oc_0F 0xB1
      [M32 rm, R32 r] -> osz32 << rm_reg_mem r rm << oc_0F 0xB1
      [R64 rm, R64 r] -> osz64 << rm_reg_reg r rm << oc_0F 0xB1
      [M64 rm, R64 r] -> osz64 << rm_reg_mem r rm << oc_0F 0xB1
      _ -> invalidArgs

    CMPXCHGB -> case args of
      [M64  m] ->          rm_ext_mem 0x1 m << oc_0F 0xC7
      [M128 m] -> osz64 << rm_ext_mem 0x1 m << oc_0F 0xC7
      _ -> invalidArgs

    AND -> case args of
        [R8 R_AL,   I8  i] ->          imm8  i << oc 0x24
        [R16 R_AX,  I16 i] -> osz16 << imm16 i << oc 0x25
        [R32 R_EAX, I32 i] -> osz32 << imm32 i << oc 0x25
        [R64 R_RAX, I32 i] -> osz64 << imm32 i << oc 0x25
        [R8  r,     I8  i] ->          rm_ext_reg 0x4 r << imm8  i << oc 0x80
        [R16 r,     I16 i] -> osz16 << rm_ext_reg 0x4 r << imm16 i << oc 0x81
        [R32 r,     I32 i] -> osz32 << rm_ext_reg 0x4 r << imm32 i << oc 0x81
        [R64 r,     I32 i] -> osz64 << rm_ext_reg 0x4 r << imm32 i << oc 0x81
        [M8  m,     I8  i] ->          rm_ext_mem 0x4 m << imm8  i << oc 0x80
        [M16 m,     I16 i] -> osz16 << rm_ext_mem 0x4 m << imm16 i << oc 0x81
        [M32 m,     I32 i] -> osz32 << rm_ext_mem 0x4 m << imm32 i << oc 0x81
        [M64 m,     I32 i] -> osz64 << rm_ext_mem 0x4 m << imm32 i << oc 0x81
        [R16 r,     I8  i] -> osz16 << rm_ext_reg 0x4 r << imm8 i << oc 0x83
        [R32 r,     I8  i] -> osz32 << rm_ext_reg 0x4 r << imm8 i << oc 0x83
        [R64 r,     I8  i] -> osz64 << rm_ext_reg 0x4 r << imm8 i << oc 0x83
        [M16 m,     I8  i] -> osz16 << rm_ext_mem 0x4 m << imm8 i << oc 0x83
        [M32 m,     I8  i] -> osz32 << rm_ext_mem 0x4 m << imm8 i << oc 0x83
        [M64 m,     I8  i] -> osz64 << rm_ext_mem 0x4 m << imm8 i << oc 0x83
        [R8  r1, R8  r2] ->          mr_reg_reg r1 r2 << oc 0x20
        [R16 r1, R16 r2] -> osz16 << mr_reg_reg r1 r2 << oc 0x21
        [R32 r1, R32 r2] -> osz32 << mr_reg_reg r1 r2 << oc 0x21
        [R64 r1, R64 r2] -> osz64 << mr_reg_reg r1 r2 << oc 0x21
        [M8 m,   R8   r] ->          rm_reg_mem r m   << oc 0x20
        [M16 m,  R16  r] -> osz16 << rm_reg_mem r m   << oc 0x21
        [M32 m,  R32  r] -> osz32 << rm_reg_mem r m   << oc 0x21
        [M64 m,  R64  r] -> osz64 << rm_reg_mem r m   << oc 0x21
        [R8  r1, R8  r2] ->          rm_reg_reg r1 r2 << oc 0x22
        [R16 r1, R16 r2] -> osz16 << rm_reg_reg r1 r2 << oc 0x23
        [R32 r1, R32 r2] -> osz32 << rm_reg_reg r1 r2 << oc 0x23
        [R64 r1, R64 r2] -> osz64 << rm_reg_reg r1 r2 << oc 0x23
        [R8  r,  M8   m] ->          rm_reg_mem r m   << oc 0x22
        [R16 r,  M16  m] -> osz16 << rm_reg_mem r m   << oc 0x23
        [R32 r,  M32  m] -> osz32 << rm_reg_mem r m   << oc 0x23
        [R64 r,  M64  m] -> osz64 << rm_reg_mem r m   << oc 0x23
        _ -> invalidArgs

    OR -> case args of
        [R8 R_AL,   I8  i] ->          imm8  i << oc 0x0C
        [R16 R_AX,  I16 i] -> osz16 << imm16 i << oc 0x0D
        [R32 R_EAX, I32 i] -> osz32 << imm32 i << oc 0x0D
        [R64 R_RAX, I32 i] -> osz64 << imm32 i << oc 0x0D
        [R8  r,     I8  i] ->          rm_ext_reg 0x1 r << imm8  i << oc 0x80
        [R16 r,     I16 i] -> osz16 << rm_ext_reg 0x1 r << imm16 i << oc 0x81
        [R32 r,     I32 i] -> osz32 << rm_ext_reg 0x1 r << imm32 i << oc 0x81
        [R64 r,     I32 i] -> osz64 << rm_ext_reg 0x1 r << imm32 i << oc 0x81
        [M8  m,     I8  i] ->          rm_ext_mem 0x1 m << imm8  i << oc 0x80
        [M16 m,     I16 i] -> osz16 << rm_ext_mem 0x1 m << imm16 i << oc 0x81
        [M32 m,     I32 i] -> osz32 << rm_ext_mem 0x1 m << imm32 i << oc 0x81
        [M64 m,     I32 i] -> osz64 << rm_ext_mem 0x1 m << imm32 i << oc 0x81
        [R16 r,     I8  i] -> osz16 << rm_ext_reg 0x1 r << imm8 i << oc 0x83
        [R32 r,     I8  i] -> osz32 << rm_ext_reg 0x1 r << imm8 i << oc 0x83
        [R64 r,     I8  i] -> osz64 << rm_ext_reg 0x1 r << imm8 i << oc 0x83
        [M16 m,     I8  i] -> osz16 << rm_ext_mem 0x1 m << imm8 i << oc 0x83
        [M32 m,     I8  i] -> osz32 << rm_ext_mem 0x1 m << imm8 i << oc 0x83
        [M64 m,     I8  i] -> osz64 << rm_ext_mem 0x1 m << imm8 i << oc 0x83
        [R8  r1, R8  r2] ->          mr_reg_reg r1 r2 << oc 0x08
        [R16 r1, R16 r2] -> osz16 << mr_reg_reg r1 r2 << oc 0x09
        [R32 r1, R32 r2] -> osz32 << mr_reg_reg r1 r2 << oc 0x09
        [R64 r1, R64 r2] -> osz64 << mr_reg_reg r1 r2 << oc 0x09
        [M8 m,   R8   r] ->          rm_reg_mem r m   << oc 0x08
        [M16 m,  R16  r] -> osz16 << rm_reg_mem r m   << oc 0x09
        [M32 m,  R32  r] -> osz32 << rm_reg_mem r m   << oc 0x09
        [M64 m,  R64  r] -> osz64 << rm_reg_mem r m   << oc 0x09
        [R8  r1, R8  r2] ->          rm_reg_reg r1 r2 << oc 0x0A
        [R16 r1, R16 r2] -> osz16 << rm_reg_reg r1 r2 << oc 0x0B
        [R32 r1, R32 r2] -> osz32 << rm_reg_reg r1 r2 << oc 0x0B
        [R64 r1, R64 r2] -> osz64 << rm_reg_reg r1 r2 << oc 0x0B
        [R8  r,  M8   m] ->          rm_reg_mem r m   << oc 0x0A
        [R16 r,  M16  m] -> osz16 << rm_reg_mem r m   << oc 0x0B
        [R32 r,  M32  m] -> osz32 << rm_reg_mem r m   << oc 0x0B
        [R64 r,  M64  m] -> osz64 << rm_reg_mem r m   << oc 0x0B
        _ -> invalidArgs

    XOR -> case args of
        [R8 R_AL,   I8  i] ->          imm8  i << oc 0x34
        [R16 R_AX,  I16 i] -> osz16 << imm16 i << oc 0x35
        [R32 R_EAX, I32 i] -> osz32 << imm32 i << oc 0x35
        [R64 R_RAX, I32 i] -> osz64 << imm32 i << oc 0x35
        [R8  r,     I8  i] ->          rm_ext_reg 0x6 r << imm8  i << oc 0x80
        [R16 r,     I16 i] -> osz16 << rm_ext_reg 0x6 r << imm16 i << oc 0x81
        [R32 r,     I32 i] -> osz32 << rm_ext_reg 0x6 r << imm32 i << oc 0x81
        [R64 r,     I32 i] -> osz64 << rm_ext_reg 0x6 r << imm32 i << oc 0x81
        [M8  m,     I8  i] ->          rm_ext_mem 0x6 m << imm8  i << oc 0x80
        [M16 m,     I16 i] -> osz16 << rm_ext_mem 0x6 m << imm16 i << oc 0x81
        [M32 m,     I32 i] -> osz32 << rm_ext_mem 0x6 m << imm32 i << oc 0x81
        [M64 m,     I32 i] -> osz64 << rm_ext_mem 0x6 m << imm32 i << oc 0x81
        [R16 r,     I8  i] -> osz16 << rm_ext_reg 0x6 r << imm8 i << oc 0x83
        [R32 r,     I8  i] -> osz32 << rm_ext_reg 0x6 r << imm8 i << oc 0x83
        [R64 r,     I8  i] -> osz64 << rm_ext_reg 0x6 r << imm8 i << oc 0x83
        [M16 m,     I8  i] -> osz16 << rm_ext_mem 0x6 m << imm8 i << oc 0x83
        [M32 m,     I8  i] -> osz32 << rm_ext_mem 0x6 m << imm8 i << oc 0x83
        [M64 m,     I8  i] -> osz64 << rm_ext_mem 0x6 m << imm8 i << oc 0x83
        [R8  r1, R8  r2] ->          mr_reg_reg r1 r2 << oc 0x30
        [R16 r1, R16 r2] -> osz16 << mr_reg_reg r1 r2 << oc 0x31
        [R32 r1, R32 r2] -> osz32 << mr_reg_reg r1 r2 << oc 0x31
        [R64 r1, R64 r2] -> osz64 << mr_reg_reg r1 r2 << oc 0x31
        [M8 m,   R8   r] ->          rm_reg_mem r m   << oc 0x30
        [M16 m,  R16  r] -> osz16 << rm_reg_mem r m   << oc 0x31
        [M32 m,  R32  r] -> osz32 << rm_reg_mem r m   << oc 0x31
        [M64 m,  R64  r] -> osz64 << rm_reg_mem r m   << oc 0x31
        [R8  r1, R8  r2] ->          rm_reg_reg r1 r2 << oc 0x32
        [R16 r1, R16 r2] -> osz16 << rm_reg_reg r1 r2 << oc 0x33
        [R32 r1, R32 r2] -> osz32 << rm_reg_reg r1 r2 << oc 0x33
        [R64 r1, R64 r2] -> osz64 << rm_reg_reg r1 r2 << oc 0x33
        [R8  r,  M8   m] ->          rm_reg_mem r m   << oc 0x32
        [R16 r,  M16  m] -> osz16 << rm_reg_mem r m   << oc 0x33
        [R32 r,  M32  m] -> osz32 << rm_reg_mem r m   << oc 0x33
        [R64 r,  M64  m] -> osz64 << rm_reg_mem r m   << oc 0x33
        _ -> invalidArgs

    NOT -> case args of
      [R8  r] ->          rm_ext_reg 0x2 r << oc 0xF6
      [M8  m] ->          rm_ext_mem 0x2 m << oc 0xF6
      [R16 r] -> osz16 << rm_ext_reg 0x2 r << oc 0xF7
      [R32 r] -> osz32 << rm_ext_reg 0x2 r << oc 0xF7
      [R64 r] -> osz64 << rm_ext_reg 0x2 r << oc 0xF7
      [M16 m] -> osz16 << rm_ext_mem 0x2 m << oc 0xF7
      [M32 m] -> osz32 << rm_ext_mem 0x2 m << oc 0xF7
      [M64 m] -> osz64 << rm_ext_mem 0x2 m << oc 0xF7
      _ -> invalidArgs

    SHL -> case args of
      [R8  r, I8 1] ->          rm_ext_reg 0x4 r << oc 0xD0
      [R16 r, I8 1] -> osz16 << rm_ext_reg 0x4 r << oc 0xD1
      [R32 r, I8 1] -> osz32 << rm_ext_reg 0x4 r << oc 0xD1
      [R64 r, I8 1] -> osz64 << rm_ext_reg 0x4 r << oc 0xD1

      [M8  m, I8 1] ->          rm_ext_mem 0x4 m << oc 0xD0
      [M16 m, I8 1] -> osz16 << rm_ext_mem 0x4 m << oc 0xD1
      [M32 m, I8 1] -> osz32 << rm_ext_mem 0x4 m << oc 0xD1
      [M64 m, I8 1] -> osz64 << rm_ext_mem 0x4 m << oc 0xD1

      [R8  r, I8 i] ->          rm_ext_reg 0x4 r << imm8 i << oc 0xC0
      [R16 r, I8 i] -> osz16 << rm_ext_reg 0x4 r << imm8 i << oc 0xC1
      [R32 r, I8 i] -> osz32 << rm_ext_reg 0x4 r << imm8 i << oc 0xC1
      [R64 r, I8 i] -> osz64 << rm_ext_reg 0x4 r << imm8 i << oc 0xC1

      [M8  m, I8 i] ->          rm_ext_mem 0x4 m << imm8 i << oc 0xC0
      [M16 m, I8 i] -> osz16 << rm_ext_mem 0x4 m << imm8 i << oc 0xC1
      [M32 m, I8 i] -> osz32 << rm_ext_mem 0x4 m << imm8 i << oc 0xC1
      [M64 m, I8 i] -> osz64 << rm_ext_mem 0x4 m << imm8 i << oc 0xC1

      [R8  r, R8 R_CL] ->          rm_ext_reg 0x4 r << oc 0xD2
      [R16 r, R8 R_CL] -> osz16 << rm_ext_reg 0x4 r << oc 0xD3
      [R32 r, R8 R_CL] -> osz32 << rm_ext_reg 0x4 r << oc 0xD3
      [R64 r, R8 R_CL] -> osz64 << rm_ext_reg 0x4 r << oc 0xD3

      [M8  m, R8 R_CL] ->          rm_ext_mem 0x4 m << oc 0xD2
      [M16 m, R8 R_CL] -> osz16 << rm_ext_mem 0x4 m << oc 0xD3
      [M32 m, R8 R_CL] -> osz32 << rm_ext_mem 0x4 m << oc 0xD3
      [M64 m, R8 R_CL] -> osz64 << rm_ext_mem 0x4 m << oc 0xD3
      _ -> invalidArgs

    SAR -> case args of
      [R8  r, I8 1] ->          rm_ext_reg 0x7 r << oc 0xD0
      [R16 r, I8 1] -> osz16 << rm_ext_reg 0x7 r << oc 0xD1
      [R32 r, I8 1] -> osz32 << rm_ext_reg 0x7 r << oc 0xD1
      [R64 r, I8 1] -> osz64 << rm_ext_reg 0x7 r << oc 0xD1

      [M8  m, I8 1] ->          rm_ext_mem 0x7 m << oc 0xD0
      [M16 m, I8 1] -> osz16 << rm_ext_mem 0x7 m << oc 0xD1
      [M32 m, I8 1] -> osz32 << rm_ext_mem 0x7 m << oc 0xD1
      [M64 m, I8 1] -> osz64 << rm_ext_mem 0x7 m << oc 0xD1

      [R8  r, I8 i] ->          rm_ext_reg 0x7 r << imm8 i << oc 0xC0
      [R16 r, I8 i] -> osz16 << rm_ext_reg 0x7 r << imm8 i << oc 0xC1
      [R32 r, I8 i] -> osz32 << rm_ext_reg 0x7 r << imm8 i << oc 0xC1
      [R64 r, I8 i] -> osz64 << rm_ext_reg 0x7 r << imm8 i << oc 0xC1

      [M8  m, I8 i] ->          rm_ext_mem 0x7 m << imm8 i << oc 0xC0
      [M16 m, I8 i] -> osz16 << rm_ext_mem 0x7 m << imm8 i << oc 0xC1
      [M32 m, I8 i] -> osz32 << rm_ext_mem 0x7 m << imm8 i << oc 0xC1
      [M64 m, I8 i] -> osz64 << rm_ext_mem 0x7 m << imm8 i << oc 0xC1

      [R8  r, R8 R_CL] ->          rm_ext_reg 0x7 r << oc 0xD2
      [R16 r, R8 R_CL] -> osz16 << rm_ext_reg 0x7 r << oc 0xD3
      [R32 r, R8 R_CL] -> osz32 << rm_ext_reg 0x7 r << oc 0xD3
      [R64 r, R8 R_CL] -> osz64 << rm_ext_reg 0x7 r << oc 0xD3

      [M8  m, R8 R_CL] ->          rm_ext_mem 0x7 m << oc 0xD2
      [M16 m, R8 R_CL] -> osz16 << rm_ext_mem 0x7 m << oc 0xD3
      [M32 m, R8 R_CL] -> osz32 << rm_ext_mem 0x7 m << oc 0xD3
      [M64 m, R8 R_CL] -> osz64 << rm_ext_mem 0x7 m << oc 0xD3
      _ -> invalidArgs

    SHR -> case args of
      [R8  r, I8 1] ->          rm_ext_reg 0x5 r << oc 0xD0
      [R16 r, I8 1] -> osz16 << rm_ext_reg 0x5 r << oc 0xD1
      [R32 r, I8 1] -> osz32 << rm_ext_reg 0x5 r << oc 0xD1
      [R64 r, I8 1] -> osz64 << rm_ext_reg 0x5 r << oc 0xD1

      [M8  m, I8 1] ->          rm_ext_mem 0x5 m << oc 0xD0
      [M16 m, I8 1] -> osz16 << rm_ext_mem 0x5 m << oc 0xD1
      [M32 m, I8 1] -> osz32 << rm_ext_mem 0x5 m << oc 0xD1
      [M64 m, I8 1] -> osz64 << rm_ext_mem 0x5 m << oc 0xD1

      [R8  r, I8 i] ->          rm_ext_reg 0x5 r << imm8 i << oc 0xC0
      [R16 r, I8 i] -> osz16 << rm_ext_reg 0x5 r << imm8 i << oc 0xC1
      [R32 r, I8 i] -> osz32 << rm_ext_reg 0x5 r << imm8 i << oc 0xC1
      [R64 r, I8 i] -> osz64 << rm_ext_reg 0x5 r << imm8 i << oc 0xC1

      [M8  m, I8 i] ->          rm_ext_mem 0x5 m << imm8 i << oc 0xC0
      [M16 m, I8 i] -> osz16 << rm_ext_mem 0x5 m << imm8 i << oc 0xC1
      [M32 m, I8 i] -> osz32 << rm_ext_mem 0x5 m << imm8 i << oc 0xC1
      [M64 m, I8 i] -> osz64 << rm_ext_mem 0x5 m << imm8 i << oc 0xC1

      [R8  r, R8 R_CL] ->          rm_ext_reg 0x5 r << oc 0xD2
      [R16 r, R8 R_CL] -> osz16 << rm_ext_reg 0x5 r << oc 0xD3
      [R32 r, R8 R_CL] -> osz32 << rm_ext_reg 0x5 r << oc 0xD3
      [R64 r, R8 R_CL] -> osz64 << rm_ext_reg 0x5 r << oc 0xD3

      [M8  m, R8 R_CL] ->          rm_ext_mem 0x5 m << oc 0xD2
      [M16 m, R8 R_CL] -> osz16 << rm_ext_mem 0x5 m << oc 0xD3
      [M32 m, R8 R_CL] -> osz32 << rm_ext_mem 0x5 m << oc 0xD3
      [M64 m, R8 R_CL] -> osz64 << rm_ext_mem 0x5 m << oc 0xD3
      _ -> invalidArgs

    ROL -> case args of
      [R8  r, I8 1] ->          rm_ext_reg 0x0 r << oc 0xD0
      [R16 r, I8 1] -> osz16 << rm_ext_reg 0x0 r << oc 0xD1
      [R32 r, I8 1] -> osz32 << rm_ext_reg 0x0 r << oc 0xD1
      [R64 r, I8 1] -> osz64 << rm_ext_reg 0x0 r << oc 0xD1

      [M8  m, I8 1] ->          rm_ext_mem 0x0 m << oc 0xD0
      [M16 m, I8 1] -> osz16 << rm_ext_mem 0x0 m << oc 0xD1
      [M32 m, I8 1] -> osz32 << rm_ext_mem 0x0 m << oc 0xD1
      [M64 m, I8 1] -> osz64 << rm_ext_mem 0x0 m << oc 0xD1

      [R8  r, I8 i] ->          rm_ext_reg 0x0 r << imm8 i << oc 0xC0
      [R16 r, I8 i] -> osz16 << rm_ext_reg 0x0 r << imm8 i << oc 0xC1
      [R32 r, I8 i] -> osz32 << rm_ext_reg 0x0 r << imm8 i << oc 0xC1
      [R64 r, I8 i] -> osz64 << rm_ext_reg 0x0 r << imm8 i << oc 0xC1

      [M8  m, I8 i] ->          rm_ext_mem 0x0 m << imm8 i << oc 0xC0
      [M16 m, I8 i] -> osz16 << rm_ext_mem 0x0 m << imm8 i << oc 0xC1
      [M32 m, I8 i] -> osz32 << rm_ext_mem 0x0 m << imm8 i << oc 0xC1
      [M64 m, I8 i] -> osz64 << rm_ext_mem 0x0 m << imm8 i << oc 0xC1

      [R8  r, R8 R_CL] ->          rm_ext_reg 0x0 r << oc 0xD2
      [R16 r, R8 R_CL] -> osz16 << rm_ext_reg 0x0 r << oc 0xD3
      [R32 r, R8 R_CL] -> osz32 << rm_ext_reg 0x0 r << oc 0xD3
      [R64 r, R8 R_CL] -> osz64 << rm_ext_reg 0x0 r << oc 0xD3

      [M8  m, R8 R_CL] ->          rm_ext_mem 0x0 m << oc 0xD2
      [M16 m, R8 R_CL] -> osz16 << rm_ext_mem 0x0 m << oc 0xD3
      [M32 m, R8 R_CL] -> osz32 << rm_ext_mem 0x0 m << oc 0xD3
      [M64 m, R8 R_CL] -> osz64 << rm_ext_mem 0x0 m << oc 0xD3
      _ -> invalidArgs

    ROR -> case args of
      [R8  r, I8 1] ->          rm_ext_reg 0x1 r << oc 0xD0
      [R16 r, I8 1] -> osz16 << rm_ext_reg 0x1 r << oc 0xD1
      [R32 r, I8 1] -> osz32 << rm_ext_reg 0x1 r << oc 0xD1
      [R64 r, I8 1] -> osz64 << rm_ext_reg 0x1 r << oc 0xD1

      [M8  m, I8 1] ->          rm_ext_mem 0x1 m << oc 0xD0
      [M16 m, I8 1] -> osz16 << rm_ext_mem 0x1 m << oc 0xD1
      [M32 m, I8 1] -> osz32 << rm_ext_mem 0x1 m << oc 0xD1
      [M64 m, I8 1] -> osz64 << rm_ext_mem 0x1 m << oc 0xD1

      [R8  r, I8 i] ->          rm_ext_reg 0x1 r << imm8 i << oc 0xC0
      [R16 r, I8 i] -> osz16 << rm_ext_reg 0x1 r << imm8 i << oc 0xC1
      [R32 r, I8 i] -> osz32 << rm_ext_reg 0x1 r << imm8 i << oc 0xC1
      [R64 r, I8 i] -> osz64 << rm_ext_reg 0x1 r << imm8 i << oc 0xC1

      [M8  m, I8 i] ->          rm_ext_mem 0x1 m << imm8 i << oc 0xC0
      [M16 m, I8 i] -> osz16 << rm_ext_mem 0x1 m << imm8 i << oc 0xC1
      [M32 m, I8 i] -> osz32 << rm_ext_mem 0x1 m << imm8 i << oc 0xC1
      [M64 m, I8 i] -> osz64 << rm_ext_mem 0x1 m << imm8 i << oc 0xC1

      [R8  r, R8 R_CL] ->          rm_ext_reg 0x1 r << oc 0xD2
      [R16 r, R8 R_CL] -> osz16 << rm_ext_reg 0x1 r << oc 0xD3
      [R32 r, R8 R_CL] -> osz32 << rm_ext_reg 0x1 r << oc 0xD3
      [R64 r, R8 R_CL] -> osz64 << rm_ext_reg 0x1 r << oc 0xD3

      [M8  m, R8 R_CL] ->          rm_ext_mem 0x1 m << oc 0xD2
      [M16 m, R8 R_CL] -> osz16 << rm_ext_mem 0x1 m << oc 0xD3
      [M32 m, R8 R_CL] -> osz32 << rm_ext_mem 0x1 m << oc 0xD3
      [M64 m, R8 R_CL] -> osz64 << rm_ext_mem 0x1 m << oc 0xD3
      _ -> invalidArgs

    RCL -> case args of
      [R8  r, I8 1] ->          rm_ext_reg 0x2 r << oc 0xD0
      [R16 r, I8 1] -> osz16 << rm_ext_reg 0x2 r << oc 0xD1
      [R32 r, I8 1] -> osz32 << rm_ext_reg 0x2 r << oc 0xD1
      [R64 r, I8 1] -> osz64 << rm_ext_reg 0x2 r << oc 0xD1

      [M8  m, I8 1] ->          rm_ext_mem 0x2 m << oc 0xD0
      [M16 m, I8 1] -> osz16 << rm_ext_mem 0x2 m << oc 0xD1
      [M32 m, I8 1] -> osz32 << rm_ext_mem 0x2 m << oc 0xD1
      [M64 m, I8 1] -> osz64 << rm_ext_mem 0x2 m << oc 0xD1

      [R8  r, I8 i] ->          rm_ext_reg 0x2 r << imm8 i << oc 0xC0
      [R16 r, I8 i] -> osz16 << rm_ext_reg 0x2 r << imm8 i << oc 0xC1
      [R32 r, I8 i] -> osz32 << rm_ext_reg 0x2 r << imm8 i << oc 0xC1
      [R64 r, I8 i] -> osz64 << rm_ext_reg 0x2 r << imm8 i << oc 0xC1

      [M8  m, I8 i] ->          rm_ext_mem 0x2 m << imm8 i << oc 0xC0
      [M16 m, I8 i] -> osz16 << rm_ext_mem 0x2 m << imm8 i << oc 0xC1
      [M32 m, I8 i] -> osz32 << rm_ext_mem 0x2 m << imm8 i << oc 0xC1
      [M64 m, I8 i] -> osz64 << rm_ext_mem 0x2 m << imm8 i << oc 0xC1

      [R8  r, R8 R_CL] ->          rm_ext_reg 0x2 r << oc 0xD2
      [R16 r, R8 R_CL] -> osz16 << rm_ext_reg 0x2 r << oc 0xD3
      [R32 r, R8 R_CL] -> osz32 << rm_ext_reg 0x2 r << oc 0xD3
      [R64 r, R8 R_CL] -> osz64 << rm_ext_reg 0x2 r << oc 0xD3

      [M8  m, R8 R_CL] ->          rm_ext_mem 0x2 m << oc 0xD2
      [M16 m, R8 R_CL] -> osz16 << rm_ext_mem 0x2 m << oc 0xD3
      [M32 m, R8 R_CL] -> osz32 << rm_ext_mem 0x2 m << oc 0xD3
      [M64 m, R8 R_CL] -> osz64 << rm_ext_mem 0x2 m << oc 0xD3
      _ -> invalidArgs

    RCR -> case args of
      [R8  r, I8 1] ->          rm_ext_reg 0x3 r << oc 0xD0
      [R16 r, I8 1] -> osz16 << rm_ext_reg 0x3 r << oc 0xD1
      [R32 r, I8 1] -> osz32 << rm_ext_reg 0x3 r << oc 0xD1
      [R64 r, I8 1] -> osz64 << rm_ext_reg 0x3 r << oc 0xD1

      [M8  m, I8 1] ->          rm_ext_mem 0x3 m << oc 0xD0
      [M16 m, I8 1] -> osz16 << rm_ext_mem 0x3 m << oc 0xD1
      [M32 m, I8 1] -> osz32 << rm_ext_mem 0x3 m << oc 0xD1
      [M64 m, I8 1] -> osz64 << rm_ext_mem 0x3 m << oc 0xD1

      [R8  r, I8 i] ->          rm_ext_reg 0x3 r << imm8 i << oc 0xC0
      [R16 r, I8 i] -> osz16 << rm_ext_reg 0x3 r << imm8 i << oc 0xC1
      [R32 r, I8 i] -> osz32 << rm_ext_reg 0x3 r << imm8 i << oc 0xC1
      [R64 r, I8 i] -> osz64 << rm_ext_reg 0x3 r << imm8 i << oc 0xC1

      [M8  m, I8 i] ->          rm_ext_mem 0x3 m << imm8 i << oc 0xC0
      [M16 m, I8 i] -> osz16 << rm_ext_mem 0x3 m << imm8 i << oc 0xC1
      [M32 m, I8 i] -> osz32 << rm_ext_mem 0x3 m << imm8 i << oc 0xC1
      [M64 m, I8 i] -> osz64 << rm_ext_mem 0x3 m << imm8 i << oc 0xC1

      [R8  r, R8 R_CL] ->          rm_ext_reg 0x3 r << oc 0xD2
      [R16 r, R8 R_CL] -> osz16 << rm_ext_reg 0x3 r << oc 0xD3
      [R32 r, R8 R_CL] -> osz32 << rm_ext_reg 0x3 r << oc 0xD3
      [R64 r, R8 R_CL] -> osz64 << rm_ext_reg 0x3 r << oc 0xD3

      [M8  m, R8 R_CL] ->          rm_ext_mem 0x3 m << oc 0xD2
      [M16 m, R8 R_CL] -> osz16 << rm_ext_mem 0x3 m << oc 0xD3
      [M32 m, R8 R_CL] -> osz32 << rm_ext_mem 0x3 m << oc 0xD3
      [M64 m, R8 R_CL] -> osz64 << rm_ext_mem 0x3 m << oc 0xD3
      _ -> invalidArgs

    BSWAP -> do
      -- TODO: not allowed before 486
      case args of
        [R32 r] -> osz32 << oc_reg r << oc_0F 0xC8
        [R64 r] -> osz64 << oc_reg r << oc_0F 0xC8
        _       -> invalidArgs

    POPCNT -> do
      require_extension Ext.POPCNT
      case args of
        [R16 d, R16 s] -> osz16 << rm_reg_reg d s << p_F3 << oc_0F 0xB8
        [R16 d, M16 s] -> osz16 << rm_reg_mem d s << p_F3 << oc_0F 0xB8
        [R32 d, R32 s] -> osz32 << rm_reg_reg d s << p_F3 << oc_0F 0xB8
        [R32 d, M32 s] -> osz32 << rm_reg_mem d s << p_F3 << oc_0F 0xB8
        [R64 d, R64 s] -> osz64 << rm_reg_reg d s << p_F3 << oc_0F 0xB8
        [R64 d, M64 s] -> osz64 << rm_reg_mem d s << p_F3 << oc_0F 0xB8
        _ -> invalidArgs

    LZCNT -> do
      require_extension Ext.LZCNT
      case args of
        [R16 d, R16 s] -> osz16 << rm_reg_reg d s << p_F3 << oc_0F 0xBD
        [R16 d, M16 s] -> osz16 << rm_reg_mem d s << p_F3 << oc_0F 0xBD
        [R32 d, R32 s] -> osz32 << rm_reg_reg d s << p_F3 << oc_0F 0xBD
        [R32 d, M32 s] -> osz32 << rm_reg_mem d s << p_F3 << oc_0F 0xBD
        [R64 d, R64 s] -> osz64 << rm_reg_reg d s << p_F3 << oc_0F 0xBD
        [R64 d, M64 s] -> osz64 << rm_reg_mem d s << p_F3 << oc_0F 0xBD
        _ -> invalidArgs

    TZCNT -> do
      require_extension Ext.BMI1
      case args of
        [R16 d, R16 s] -> osz16 << rm_reg_reg d s << p_F3 << oc_0F 0xBC
        [R16 d, M16 s] -> osz16 << rm_reg_mem d s << p_F3 << oc_0F 0xBC
        [R32 d, R32 s] -> osz32 << rm_reg_reg d s << p_F3 << oc_0F 0xBC
        [R32 d, M32 s] -> osz32 << rm_reg_mem d s << p_F3 << oc_0F 0xBC
        [R64 d, R64 s] -> osz64 << rm_reg_reg d s << p_F3 << oc_0F 0xBC
        [R64 d, M64 s] -> osz64 << rm_reg_mem d s << p_F3 << oc_0F 0xBC
        _ -> invalidArgs

    BSR -> do
      case args of
        [R16 d, R16 s] -> osz16 << rm_reg_reg d s << oc_0F 0xBD
        [R16 d, M16 s] -> osz16 << rm_reg_mem d s << oc_0F 0xBD
        [R32 d, R32 s] -> osz32 << rm_reg_reg d s << oc_0F 0xBD
        [R32 d, M32 s] -> osz32 << rm_reg_mem d s << oc_0F 0xBD
        [R64 d, R64 s] -> osz64 << rm_reg_reg d s << oc_0F 0xBD
        [R64 d, M64 s] -> osz64 << rm_reg_mem d s << oc_0F 0xBD
        _ -> invalidArgs

    BSF -> do
      case args of
        [R16 d, R16 s] -> osz16 << rm_reg_reg d s << oc_0F 0xBC
        [R16 d, M16 s] -> osz16 << rm_reg_mem d s << oc_0F 0xBC
        [R32 d, R32 s] -> osz32 << rm_reg_reg d s << oc_0F 0xBC
        [R32 d, M32 s] -> osz32 << rm_reg_mem d s << oc_0F 0xBC
        [R64 d, R64 s] -> osz64 << rm_reg_reg d s << oc_0F 0xBC
        [R64 d, M64 s] -> osz64 << rm_reg_mem d s << oc_0F 0xBC
        _ -> invalidArgs

    BT -> do
      case args of
        [R16 d, R16 s] -> osz16 << rm_reg_reg s d << oc_0F 0xA3
        [M16 d, R16 s] -> osz16 << rm_reg_mem s d << oc_0F 0xA3
        [R32 d, R32 s] -> osz32 << rm_reg_reg s d << oc_0F 0xA3
        [M32 d, R32 s] -> osz32 << rm_reg_mem s d << oc_0F 0xA3
        [R64 d, R64 s] -> osz64 << rm_reg_reg s d << oc_0F 0xA3
        [M64 d, R64 s] -> osz64 << rm_reg_mem s d << oc_0F 0xA3
        [R16 d, I8 i]  -> osz16 << rm_ext_reg 0x4 d << imm8 i << oc_0F 0xBA
        [M16 d, I8 i]  -> osz16 << rm_ext_mem 0x4 d << imm8 i << oc_0F 0xBA
        [R32 d, I8 i]  -> osz32 << rm_ext_reg 0x4 d << imm8 i << oc_0F 0xBA
        [M32 d, I8 i]  -> osz32 << rm_ext_mem 0x4 d << imm8 i << oc_0F 0xBA
        [R64 d, I8 i]  -> osz64 << rm_ext_reg 0x4 d << imm8 i << oc_0F 0xBA
        [M64 d, I8 i]  -> osz64 << rm_ext_mem 0x4 d << imm8 i << oc_0F 0xBA
        _ -> invalidArgs

    BTC -> do
      case args of
        [R16 d, R16 s] -> osz16 << rm_reg_reg s d << oc_0F 0xBB
        [M16 d, R16 s] -> osz16 << rm_reg_mem s d << oc_0F 0xBB
        [R32 d, R32 s] -> osz32 << rm_reg_reg s d << oc_0F 0xBB
        [M32 d, R32 s] -> osz32 << rm_reg_mem s d << oc_0F 0xBB
        [R64 d, R64 s] -> osz64 << rm_reg_reg s d << oc_0F 0xBB
        [M64 d, R64 s] -> osz64 << rm_reg_mem s d << oc_0F 0xBB
        [R16 d, I8 i]  -> osz16 << rm_ext_reg 0x7 d << imm8 i << oc_0F 0xBA
        [M16 d, I8 i]  -> osz16 << rm_ext_mem 0x7 d << imm8 i << oc_0F 0xBA
        [R32 d, I8 i]  -> osz32 << rm_ext_reg 0x7 d << imm8 i << oc_0F 0xBA
        [M32 d, I8 i]  -> osz32 << rm_ext_mem 0x7 d << imm8 i << oc_0F 0xBA
        [R64 d, I8 i]  -> osz64 << rm_ext_reg 0x7 d << imm8 i << oc_0F 0xBA
        [M64 d, I8 i]  -> osz64 << rm_ext_mem 0x7 d << imm8 i << oc_0F 0xBA
        _ -> invalidArgs

    BTR -> do
      case args of
        [R16 d, R16 s] -> osz16 << rm_reg_reg s d << oc_0F 0xB3
        [M16 d, R16 s] -> osz16 << rm_reg_mem s d << oc_0F 0xB3
        [R32 d, R32 s] -> osz32 << rm_reg_reg s d << oc_0F 0xB3
        [M32 d, R32 s] -> osz32 << rm_reg_mem s d << oc_0F 0xB3
        [R64 d, R64 s] -> osz64 << rm_reg_reg s d << oc_0F 0xB3
        [M64 d, R64 s] -> osz64 << rm_reg_mem s d << oc_0F 0xB3
        [R16 d, I8 i]  -> osz16 << rm_ext_reg 0x6 d << imm8 i << oc_0F 0xBA
        [M16 d, I8 i]  -> osz16 << rm_ext_mem 0x6 d << imm8 i << oc_0F 0xBA
        [R32 d, I8 i]  -> osz32 << rm_ext_reg 0x6 d << imm8 i << oc_0F 0xBA
        [M32 d, I8 i]  -> osz32 << rm_ext_mem 0x6 d << imm8 i << oc_0F 0xBA
        [R64 d, I8 i]  -> osz64 << rm_ext_reg 0x6 d << imm8 i << oc_0F 0xBA
        [M64 d, I8 i]  -> osz64 << rm_ext_mem 0x6 d << imm8 i << oc_0F 0xBA
        _ -> invalidArgs

    BTS -> do
      case args of
        [R16 d, R16 s] -> osz16 << rm_reg_reg s d << oc_0F 0xAB
        [M16 d, R16 s] -> osz16 << rm_reg_mem s d << oc_0F 0xAB
        [R32 d, R32 s] -> osz32 << rm_reg_reg s d << oc_0F 0xAB
        [M32 d, R32 s] -> osz32 << rm_reg_mem s d << oc_0F 0xAB
        [R64 d, R64 s] -> osz64 << rm_reg_reg s d << oc_0F 0xAB
        [M64 d, R64 s] -> osz64 << rm_reg_mem s d << oc_0F 0xAB
        [R16 d, I8 i]  -> osz16 << rm_ext_reg 0x5 d << imm8 i << oc_0F 0xBA
        [M16 d, I8 i]  -> osz16 << rm_ext_mem 0x5 d << imm8 i << oc_0F 0xBA
        [R32 d, I8 i]  -> osz32 << rm_ext_reg 0x5 d << imm8 i << oc_0F 0xBA
        [M32 d, I8 i]  -> osz32 << rm_ext_mem 0x5 d << imm8 i << oc_0F 0xBA
        [R64 d, I8 i]  -> osz64 << rm_ext_reg 0x5 d << imm8 i << oc_0F 0xBA
        [M64 d, I8 i]  -> osz64 << rm_ext_mem 0x5 d << imm8 i << oc_0F 0xBA
        _ -> invalidArgs

    SUB -> case args of
        [R8 R_AL,   I8  i] ->          imm8  i << oc 0x2C
        [R16 R_AX,  I16 i] -> osz16 << imm16 i << oc 0x2D
        [R32 R_EAX, I32 i] -> osz32 << imm32 i << oc 0x2D
        [R64 R_RAX, I32 i] -> osz64 << imm32 i << oc 0x2D
        [R8  r, I8  i] ->          rm_ext_reg 0x5 r << imm8  i << oc 0x80
        [R16 r, I16 i] -> osz16 << rm_ext_reg 0x5 r << imm16 i << oc 0x81
        [R32 r, I32 i] -> osz32 << rm_ext_reg 0x5 r << imm32 i << oc 0x81
        [R64 r, I32 i] -> osz64 << rm_ext_reg 0x5 r << imm32 i << oc 0x81
        [M8  m, I8  i] ->          rm_ext_mem 0x5 m << imm8  i << oc 0x80
        [M16 m, I16 i] -> osz16 << rm_ext_mem 0x5 m << imm16 i << oc 0x81
        [M32 m, I32 i] -> osz32 << rm_ext_mem 0x5 m << imm32 i << oc 0x81
        [M64 m, I32 i] -> osz64 << rm_ext_mem 0x5 m << imm32 i << oc 0x81
        [R16 r, I8 i] -> osz16 << rm_ext_reg 0x5 r << imm8 i << oc 0x83
        [R32 r, I8 i] -> osz32 << rm_ext_reg 0x5 r << imm8 i << oc 0x83
        [R64 r, I8 i] -> osz64 << rm_ext_reg 0x5 r << imm8 i << oc 0x83
        [M16 m, I8 i] -> osz16 << rm_ext_mem 0x5 m << imm8 i << oc 0x83
        [M32 m, I8 i] -> osz32 << rm_ext_mem 0x5 m << imm8 i << oc 0x83
        [M64 m, I8 i] -> osz64 << rm_ext_mem 0x5 m << imm8 i << oc 0x83
        [R8  r1, R8  r2] ->          mr_reg_reg r1 r2 << oc 0x28
        [R16 r1, R16 r2] -> osz16 << mr_reg_reg r1 r2 << oc 0x29
        [R32 r1, R32 r2] -> osz32 << mr_reg_reg r1 r2 << oc 0x29
        [R64 r1, R64 r2] -> osz64 << mr_reg_reg r1 r2 << oc 0x29
        [M8 m,   R8   r] ->          rm_reg_mem r m   << oc 0x28
        [M16 m,  R16  r] -> osz16 << rm_reg_mem r m   << oc 0x29
        [M32 m,  R32  r] -> osz32 << rm_reg_mem r m   << oc 0x29
        [M64 m,  R64  r] -> osz64 << rm_reg_mem r m   << oc 0x29
        [R8  r1, R8  r2] ->          rm_reg_reg r1 r2 << oc 0x2A
        [R16 r1, R16 r2] -> osz16 << rm_reg_reg r1 r2 << oc 0x2B
        [R32 r1, R32 r2] -> osz32 << rm_reg_reg r1 r2 << oc 0x2B
        [R64 r1, R64 r2] -> osz64 << rm_reg_reg r1 r2 << oc 0x2B
        [R8  r,  M8   m] ->          rm_reg_mem r m   << oc 0x2A
        [R16 r,  M16  m] -> osz16 << rm_reg_mem r m   << oc 0x2B
        [R32 r,  M32  m] -> osz32 << rm_reg_mem r m   << oc 0x2B
        [R64 r,  M64  m] -> osz64 << rm_reg_mem r m   << oc 0x2B
        _ -> invalidArgs

    SBB -> case args of
        [R8 R_AL,   I8  i] ->          imm8  i << oc 0x1C
        [R16 R_AX,  I16 i] -> osz16 << imm16 i << oc 0x1D
        [R32 R_EAX, I32 i] -> osz32 << imm32 i << oc 0x1D
        [R64 R_RAX, I32 i] -> osz64 << imm32 i << oc 0x1D
        [R8  r,     I8  i] ->          rm_ext_reg 0x3 r << imm8  i << oc 0x80
        [R16 r,     I16 i] -> osz16 << rm_ext_reg 0x3 r << imm16 i << oc 0x81
        [R32 r,     I32 i] -> osz32 << rm_ext_reg 0x3 r << imm32 i << oc 0x81
        [R64 r,     I32 i] -> osz64 << rm_ext_reg 0x3 r << imm32 i << oc 0x81
        [M8  m,     I8  i] ->          rm_ext_mem 0x3 m << imm8  i << oc 0x80
        [M16 m,     I16 i] -> osz16 << rm_ext_mem 0x3 m << imm16 i << oc 0x81
        [M32 m,     I32 i] -> osz32 << rm_ext_mem 0x3 m << imm32 i << oc 0x81
        [M64 m,     I32 i] -> osz64 << rm_ext_mem 0x3 m << imm32 i << oc 0x81
        [R16 r,     I8  i] -> osz16 << rm_ext_reg 0x3 r << imm8 i << oc 0x83
        [R32 r,     I8  i] -> osz32 << rm_ext_reg 0x3 r << imm8 i << oc 0x83
        [R64 r,     I8  i] -> osz64 << rm_ext_reg 0x3 r << imm8 i << oc 0x83
        [M16 m,     I8  i] -> osz16 << rm_ext_mem 0x3 m << imm8 i << oc 0x83
        [M32 m,     I8  i] -> osz32 << rm_ext_mem 0x3 m << imm8 i << oc 0x83
        [M64 m,     I8  i] -> osz64 << rm_ext_mem 0x3 m << imm8 i << oc 0x83
        [R8  r1, R8  r2] ->          mr_reg_reg r1 r2 << oc 0x18
        [R16 r1, R16 r2] -> osz16 << mr_reg_reg r1 r2 << oc 0x19
        [R32 r1, R32 r2] -> osz32 << mr_reg_reg r1 r2 << oc 0x19
        [R64 r1, R64 r2] -> osz64 << mr_reg_reg r1 r2 << oc 0x19
        [M8 m,   R8   r] ->          rm_reg_mem r m   << oc 0x18
        [M16 m,  R16  r] -> osz16 << rm_reg_mem r m   << oc 0x19
        [M32 m,  R32  r] -> osz32 << rm_reg_mem r m   << oc 0x19
        [M64 m,  R64  r] -> osz64 << rm_reg_mem r m   << oc 0x19
        [R8  r1, R8  r2] ->          rm_reg_reg r1 r2 << oc 0x1A
        [R16 r1, R16 r2] -> osz16 << rm_reg_reg r1 r2 << oc 0x1B
        [R32 r1, R32 r2] -> osz32 << rm_reg_reg r1 r2 << oc 0x1B
        [R64 r1, R64 r2] -> osz64 << rm_reg_reg r1 r2 << oc 0x1B
        [R8  r,  M8   m] ->          rm_reg_mem r m   << oc 0x1A
        [R16 r,  M16  m] -> osz16 << rm_reg_mem r m   << oc 0x1B
        [R32 r,  M32  m] -> osz32 << rm_reg_mem r m   << oc 0x1B
        [R64 r,  M64  m] -> osz64 << rm_reg_mem r m   << oc 0x1B
        _ -> invalidArgs

    DEC -> case args of
      [M8  m] ->          rm_ext_mem 0x1 m << oc 0xFE
      [M16 m] -> osz16 << rm_ext_mem 0x1 m << oc 0xFF
      [M32 m] -> osz32 << rm_ext_mem 0x1 m << oc 0xFF
      [M64 m] -> osz64 << rm_ext_mem 0x1 m << oc 0xFF
      -- shorter forms, except in 64-bit mode (reused for REX prefixes)
      [R16 r]
        | not mode64
        , (False,c) <- regCodeX r
        -> osz16 << oc (0x48 + c)
      [R32 r]
        | not mode64
        , (False,c) <- regCodeX r
        -> osz32 << oc (0x48 + c)
      [R8  r] ->          rm_ext_reg 0x1 r << oc 0xFE
      [R16 r] -> osz16 << rm_ext_reg 0x1 r << oc 0xFF
      [R32 r] -> osz32 << rm_ext_reg 0x1 r << oc 0xFF
      [R64 r] -> osz64 << rm_ext_reg 0x1 r << oc 0xFF
      _ -> invalidArgs

    INC -> case args of
      [M8  m] ->          rm_ext_mem 0x0 m << oc 0xFE
      [M16 m] -> osz16 << rm_ext_mem 0x0 m << oc 0xFF
      [M32 m] -> osz32 << rm_ext_mem 0x0 m << oc 0xFF
      [M64 m] -> osz64 << rm_ext_mem 0x0 m << oc 0xFF
      -- shorter forms, except in 64-bit mode (reused for REX prefixes)
      [R16 r]
        | not mode64
        , (False,c) <- regCodeX r
        -> osz16 << oc (0x40 + c)
      [R32 r]
        | not mode64
        , (False,c) <- regCodeX r
        -> osz32 << oc (0x40 + c)
      [R8  r] ->          rm_ext_reg 0x0 r << oc 0xFE
      [R16 r] -> osz16 << rm_ext_reg 0x0 r << oc 0xFF
      [R32 r] -> osz32 << rm_ext_reg 0x0 r << oc 0xFF
      [R64 r] -> osz64 << rm_ext_reg 0x0 r << oc 0xFF
      _ -> invalidArgs

    NEG  -> case args of
      [M8  m] ->          rm_ext_mem 0x3 m << oc 0xF6
      [M16 m] -> osz16 << rm_ext_mem 0x3 m << oc 0xF7
      [M32 m] -> osz32 << rm_ext_mem 0x3 m << oc 0xF7
      [M64 m] -> osz64 << rm_ext_mem 0x3 m << oc 0xF7
      _ -> invalidArgs

    DIV  -> case args of
      [M8  m] ->          rm_ext_mem 0x6 m << oc 0xF6
      [M16 m] -> osz16 << rm_ext_mem 0x6 m << oc 0xF7
      [M32 m] -> osz32 << rm_ext_mem 0x6 m << oc 0xF7
      [M64 m] -> osz64 << rm_ext_mem 0x6 m << oc 0xF7
      _ -> invalidArgs

    MUL  -> case args of
      [M8  m] ->          rm_ext_mem 0x4 m << oc 0xF6
      [M16 m] -> osz16 << rm_ext_mem 0x4 m << oc 0xF7
      [M32 m] -> osz32 << rm_ext_mem 0x4 m << oc 0xF7
      [M64 m] -> osz64 << rm_ext_mem 0x4 m << oc 0xF7
      _ -> invalidArgs

    IDIV -> case args of
      [M8  m] ->          rm_ext_mem 0x7 m << oc 0xF6
      [M16 m] -> osz16 << rm_ext_mem 0x7 m << oc 0xF7
      [M32 m] -> osz32 << rm_ext_mem 0x7 m << oc 0xF7
      [M64 m] -> osz64 << rm_ext_mem 0x7 m << oc 0xF7
      _ -> invalidArgs

    IMUL -> case args of
      [M8  m] ->          rm_ext_mem 0x5 m << oc 0xF6
      [M16 m] -> osz16 << rm_ext_mem 0x5 m << oc 0xF7
      [M32 m] -> osz32 << rm_ext_mem 0x5 m << oc 0xF7
      [M64 m] -> osz64 << rm_ext_mem 0x5 m << oc 0xF7
      [R8  r] ->          rm_ext_reg 0x5 r << oc 0xF6
      [R16 r] -> osz16 << rm_ext_reg 0x5 r << oc 0xF7
      [R32 r] -> osz32 << rm_ext_reg 0x5 r << oc 0xF7
      [R64 r] -> osz64 << rm_ext_reg 0x5 r << oc 0xF7
      [R16 r1, R16 r2, I16 i] -> imm16 i << osz16 << rm_reg_reg r1 r2 << oc 0x69
      [R32 r1, R32 r2, I32 i] -> imm32 i << osz32 << rm_reg_reg r1 r2 << oc 0x69
      [R64 r1, R64 r2, I32 i] -> imm32 i << osz64 << rm_reg_reg r1 r2 << oc 0x69
      [R16 r,  M16 m,  I16 i] -> imm16 i << osz16 << rm_reg_mem r m << oc 0x69
      [R32 r,  M32 m,  I32 i] -> imm32 i << osz32 << rm_reg_mem r m << oc 0x69
      [R64 r,  M64 m,  I32 i] -> imm32 i << osz64 << rm_reg_mem r m << oc 0x69
      [R16 r1, R16 r2, I8  i] -> imm8 i << osz16 << rm_reg_reg r1 r2 << oc 0x6B
      [R32 r1, R32 r2, I8  i] -> imm8 i << osz32 << rm_reg_reg r1 r2 << oc 0x6B
      [R64 r1, R64 r2, I8  i] -> imm8 i << osz64 << rm_reg_reg r1 r2 << oc 0x6B
      [R16 r,  M16 m,  I8  i] -> imm8 i << osz16 << rm_reg_mem r m << oc 0x6B
      [R32 r,  M32 m,  I8  i] -> imm8 i << osz32 << rm_reg_mem r m << oc 0x6B
      [R64 r,  M64 m,  I8  i] -> imm8 i << osz64 << rm_reg_mem r m << oc 0x6B
      [R16 r1, R16 r2] -> osz16 << rm_reg_reg r1 r2 << oc_0F 0xAF
      [R32 r1, R32 r2] -> osz32 << rm_reg_reg r1 r2 << oc_0F 0xAF
      [R64 r1, R64 r2] -> osz64 << rm_reg_reg r1 r2 << oc_0F 0xAF
      [R16 r,  M16  m] -> osz16 << rm_reg_mem r m   << oc_0F 0xAF
      [R32 r,  M32  m] -> osz32 << rm_reg_mem r m   << oc_0F 0xAF
      [R64 r,  M64  m] -> osz64 << rm_reg_mem r m   << oc_0F 0xAF
      _ -> invalidArgs

    SXA -> case args of
      -- Intel uses CBW/CWDE/CDQE mnemonics to differentiate the operand size.
      -- This sucks. Let's use the destination register (AX,EAX,RAX) as an
      -- operand instead.
      [OpReg R_AX ] -> osz16 << oc 0x98
      [OpReg R_EAX] -> osz32 << oc 0x98
      [OpReg R_RAX] -> osz64 << oc 0x98
      _             -> invalidArgs

    SXAD -> case args of
      -- Intel uses CWD/CDQ/CQO mnemonics to differentiate the operand size.
      -- This sucks. Let's use the source/destination register (AX,EAX,RAX) as an
      -- operand instead.
      [OpReg R_AX ] -> osz16 << oc 0x99
      [OpReg R_EAX] -> osz32 << oc 0x99
      [OpReg R_RAX] -> osz64 << oc 0x99
      _             -> invalidArgs

    MOV -> case args of
      [R8  r1, R8  r2] ->          mr_reg_reg r1 r2 << oc 0x88
      [R16 r1, R16 r2] -> osz16 << mr_reg_reg r1 r2 << oc 0x89
      [R32 r1, R32 r2] -> osz32 << mr_reg_reg r1 r2 << oc 0x89
      [R64 r1, R64 r2] -> osz64 << mr_reg_reg r1 r2 << oc 0x89
      [M8 m,   R8   r] ->          rm_reg_mem r m   << oc 0x88
      [M16 m,  R16  r] -> osz16 << rm_reg_mem r m   << oc 0x89
      [M32 m,  R32  r] -> osz32 << rm_reg_mem r m   << oc 0x89
      [M64 m,  R64  r] -> osz64 << rm_reg_mem r m   << oc 0x89
      -- redundant encodings
      -- [R8  r1, R8  r2] ->          rm_reg_reg r1 r2 << oc 0x8A
      -- [R16 r1, R16 r2] -> osz16 << rm_reg_reg r1 r2 << oc 0x8B
      -- [R32 r1, R32 r2] -> osz32 << rm_reg_reg r1 r2 << oc 0x8B
      -- [R64 r1, R64 r2] -> osz64 << rm_reg_reg r1 r2 << oc 0x8B
      [R8  r,  M8   m] ->          rm_reg_mem r m   << oc 0x8A
      [R16 r,  M16  m] -> osz16 << rm_reg_mem r m   << oc 0x8B
      [R32 r,  M32  m] -> osz32 << rm_reg_mem r m   << oc 0x8B
      [R64 r,  M64  m] -> osz64 << rm_reg_mem r m   << oc 0x8B
      -- shorter forms for reg,imm + imm64 form!
      [R8  r, I8  i] ->          imm8  i << oc_reg r << oc 0xB0
      [R16 r, I16 i] -> osz16 << imm16 i << oc_reg r << oc 0xB8
      [R32 r, I32 i] -> osz32 << imm32 i << oc_reg r << oc 0xB8
      [R64 r, I64 i] -> osz64 << imm64 i << oc_reg r << oc 0xB8
      [R8  r, I8  i] ->          rm_ext_reg 0x0 r << imm8  i << oc 0xC6
      [R16 r, I16 i] -> osz16 << rm_ext_reg 0x0 r << imm16 i << oc 0xC7
      [R32 r, I32 i] -> osz32 << rm_ext_reg 0x0 r << imm32 i << oc 0xC7
      [M8  m, I8  i] ->          rm_ext_mem 0x0 m << imm8  i << oc 0xC6
      [M16 m, I16 i] -> osz16 << rm_ext_mem 0x0 m << imm16 i << oc 0xC7
      [M32 m, I32 i] -> osz32 << rm_ext_mem 0x0 m << imm32 i << oc 0xC7
      -- These are really MOVSX, so we implement them as such.
      -- [R64 r, I32 i] -> Just $ osz64 << rm_ext_reg 0x0 r << imm32 i << oc 0xC7
      -- [M64 m, I32 i] -> Just $ osz64 << rm_ext_mem 0x0 m << imm32 i << oc 0xC7
      -- TODO: mov rm, sreg
      -- TODO: mov sreg, rm
      -- TODO: mov acc, moffs
      -- TODO: mov moffs, acc
      _ -> invalidArgs

    MOVSX -> case args of
      [R16 d, R8 s]  -> osz16 << rm_reg_reg d s << oc_0F 0xBE
      [R32 d, R8 s]  -> osz32 << rm_reg_reg d s << oc_0F 0xBE
      [R64 d, R8 s]  -> osz64 << rm_reg_reg d s << oc_0F 0xBE
      [R32 d, R16 s] ->          rm_reg_reg d s << oc_0F 0xBF
      [R64 d, R16 s] -> osz64 << rm_reg_reg d s << oc_0F 0xBF
      [R64 d, R32 s] -> osz64 << rm_reg_reg d s << oc 0x63
      [R16 d, M8 s]  -> osz16 << rm_reg_mem d s << oc_0F 0xBE
      [R32 d, M8 s]  -> osz32 << rm_reg_mem d s << oc_0F 0xBE
      [R64 d, M8 s]  -> osz64 << rm_reg_mem d s << oc_0F 0xBE
      [R32 d, M16 s] ->          rm_reg_mem d s << oc_0F 0xBF
      [R64 d, M16 s] -> osz64 << rm_reg_mem d s << oc_0F 0xBF
      [R64 d, M32 s] -> osz64 << rm_reg_mem d s << oc 0x63
      -- discouraged (use normal MOV instead) but still supported
      [R16 d, R16 s] -> osz16 << rm_reg_reg d s << oc 0x63
      [R32 d, R32 s] -> osz32 << rm_reg_reg d s << oc 0x63
      [R16 d, M16 s] -> osz16 << rm_reg_mem d s << oc 0x63
      [R32 d, M32 s] -> osz32 << rm_reg_mem d s << oc 0x63
      -- these are documented as MOVs, but they're really MOVSXs!
      [R64 r, I32 i] -> osz64 << rm_ext_reg 0x0 r << imm32 i << oc 0xC7
      [M64 m, I32 i] -> osz64 << rm_ext_mem 0x0 m << imm32 i << oc 0xC7
      _ -> invalidArgs

    MOVZX -> case args of
      [R16 d, R8 s]  -> osz16 << rm_reg_reg d s << oc_0F 0xB6
      [R32 d, R8 s]  -> osz32 << rm_reg_reg d s << oc_0F 0xB6
      [R64 d, R8 s]  -> osz64 << rm_reg_reg d s << oc_0F 0xB6
      [R32 d, R16 s] ->          rm_reg_reg d s << oc_0F 0xB7
      [R64 d, R16 s] -> osz64 << rm_reg_reg d s << oc_0F 0xB7
      [R16 d, M8 s]  -> osz16 << rm_reg_mem d s << oc_0F 0xB6
      [R32 d, M8 s]  -> osz32 << rm_reg_mem d s << oc_0F 0xB6
      [R64 d, M8 s]  -> osz64 << rm_reg_mem d s << oc_0F 0xB6
      [R32 d, M16 s] ->          rm_reg_mem d s << oc_0F 0xB7
      [R64 d, M16 s] -> osz64 << rm_reg_mem d s << oc_0F 0xB7
      -- [R64 d, R32 s] -- not supported: a simple MOV does the same thing
      -- [R64 d, M32 s] -- not supported: a simple MOV does the same thing
      _ -> invalidArgs

    MOVBE -> do
      require_extension Ext.MOVBE
      case args of
        [R16 d, M16 s] -> osz16 << rm_reg_mem d s << oc_0F38 0xF0
        [R32 d, M32 s] -> osz32 << rm_reg_mem d s << oc_0F38 0xF0
        [R64 d, M64 s] -> osz64 << rm_reg_mem d s << oc_0F38 0xF0
        [M16 d, R16 s] -> osz16 << rm_reg_mem s d << oc_0F38 0xF1
        [M32 d, R32 s] -> osz32 << rm_reg_mem s d << oc_0F38 0xF1
        [M64 d, R64 s] -> osz64 << rm_reg_mem s d << oc_0F38 0xF1
        _ -> invalidArgs

    CMOV cc -> do
      require_extension Ext.CMOV
      oc_0F (0x40 + condCode cc)
      case args of
        [R16 d, R16 s] -> osz16 << rm_reg_reg d s
        [R32 d, R32 s] -> osz32 << rm_reg_reg d s
        [R64 d, R64 s] -> osz64 << rm_reg_reg d s
        [R16 d, M16 s] -> osz16 << rm_reg_mem d s
        [R32 d, M32 s] -> osz32 << rm_reg_mem d s
        [R64 d, M64 s] -> osz64 << rm_reg_mem d s
        _              -> invalidArgs

    SETcc cc -> case args of
      [R8 r] -> rm_ext_reg 0x0 r << oc_0F (0x90 + condCode cc)
      [M8 m] -> rm_ext_mem 0x0 m << oc_0F (0x90 + condCode cc)
      _      -> invalidArgs

    XCHG -> case args of
      [R16 R_AX,  R16 r]     -> osz16 << oc_reg r << oc 0x90
      [R16 r,     R16 R_AX]  -> osz16 << oc_reg r << oc 0x90
      [R32 R_EAX, R32 r]     -> osz32 << oc_reg r << oc 0x90
      [R32 r,     R32 R_EAX] -> osz32 << oc_reg r << oc 0x90
      [R64 R_RAX, R64 r]     -> osz64 << oc_reg r << oc 0x90
      [R64 r,     R64 R_RAX] -> osz64 << oc_reg r << oc 0x90
      [R8 r1,     R8 r2]     -> rm_reg_reg r1 r2 << oc 0x86
      [M8 m,      R8 r]      -> rm_reg_mem r m   << oc 0x86
      [R8 r,      M8 m]      -> rm_reg_mem r m   << oc 0x86
      [R16 r1,    R16 r2]    -> osz16 << rm_reg_reg r1 r2 << oc 0x87
      [M16 m,     R16 r]     -> osz16 << rm_reg_mem r m   << oc 0x87
      [R16 r,     M16 m]     -> osz16 << rm_reg_mem r m   << oc 0x87
      [R32 r1,    R32 r2]    -> osz32 << rm_reg_reg r1 r2 << oc 0x87
      [M32 m,     R32 r]     -> osz32 << rm_reg_mem r m   << oc 0x87
      [R32 r,     M32 m]     -> osz32 << rm_reg_mem r m   << oc 0x87
      [R64 r1,    R64 r2]    -> osz64 << rm_reg_reg r1 r2 << oc 0x87
      [M64 m,     R64 r]     -> osz64 << rm_reg_mem r m   << oc 0x87
      [R64 r,     M64 m]     -> osz64 << rm_reg_mem r m   << oc 0x87
      _ -> invalidArgs

    XADD -> case args of
      [R8 d,  R8 s]  ->          rm_reg_reg s d << oc_0F 0xC0
      [M8 d,  R8 s]  ->          rm_reg_mem s d << oc_0F 0xC0
      [R16 d, R16 s] -> osz16 << rm_reg_reg s d << oc_0F 0xC1
      [M16 d, R16 s] -> osz16 << rm_reg_mem s d << oc_0F 0xC1
      [R32 d, R32 s] -> osz32 << rm_reg_reg s d << oc_0F 0xC1
      [M32 d, R32 s] -> osz32 << rm_reg_mem s d << oc_0F 0xC1
      [R64 d, R64 s] -> osz64 << rm_reg_reg s d << oc_0F 0xC1
      [M64 d, R64 s] -> osz64 << rm_reg_mem s d << oc_0F 0xC1
      _ -> invalidArgs

    MOVNTI -> do
      require_extension Ext.SSE2
      case args of
        [M32 m, R32 r] ->          rm_reg_mem r m << oc_0F 0xC3
        [M64 m, R64 r] -> osz64 << rm_reg_mem r m << oc_0F 0xC3
        _ -> invalidArgs

    XLAT -> do
      case args of
        -- expect [[rS:]rBX + AL] form
        -- Use it to infer segment and address size
        [OpMem (Mem mseg _msize NoLock (MemAbs (Just base) Scale1 (Just R_AL) NoDisp) Nothing)]
          -> case base of
              R_BX  -> asz16 << set_segment mseg << oc 0xD7
              R_EBX -> asz32 << set_segment mseg << oc 0xD7
              R_RBX -> asz64 << set_segment mseg << oc 0xD7
              _ -> invalidArgs
        _ -> invalidArgs


    LEA -> case args of
      -- we don't care about the size of the targetted memory...
      [R16 r, OpMem m] -> osz16 << rm_reg_mem r m << oc 0x8D
      [R32 r, OpMem m] -> osz32 << rm_reg_mem r m << oc 0x8D
      [R64 r, OpMem m] -> osz64 << rm_reg_mem r m << oc 0x8D
      _ -> invalidArgs

    IN -> case args of
      [R8  R_AL,  I8 i]     ->          imm8 i << oc 0xE4
      [R16 R_AX,  I8 i]     -> osz16 << imm8 i << oc 0xE5
      [R32 R_EAX, I8 i]     -> osz32 << imm8 i << oc 0xE5
      [R8  R_AL,  R16 R_DX] ->          oc 0xEC
      [R16 R_AX,  R16 R_DX] -> osz16 << oc 0xED
      [R32 R_EAX, R16 R_DX] -> osz32 << oc 0xED
      _ -> invalidArgs

    OUT -> case args of
      [I8 i, R8  R_AL]      ->          imm8 i << oc 0xE6
      [I8 i, R16 R_AX]      -> osz16 << imm8 i << oc 0xE7
      [I8 i, R32 R_EAX]     -> osz32 << imm8 i << oc 0xE7
      [R16 R_DX, R8  R_AL]  ->          oc 0xEE
      [R16 R_DX, R16 R_AX]  -> osz16 << oc 0xEF
      [R16 R_DX, R32 R_EAX] -> osz32 << oc 0xEF
      _ -> invalidArgs

    INS osz asz -> do
      assert_no_args
      case osz of
        OpSize8  ->          oc 0x6C
        OpSize16 -> osz16 << oc 0x6D
        OpSize32 -> osz32 << oc 0x6D
        OpSize64 -> efail EInvalidOpSize64
      set_addrsize asz

    OUTS osz asz -> do
      assert_no_args
      case osz of
        OpSize8  ->          oc 0x6E
        OpSize16 -> osz16 << oc 0x6F
        OpSize32 -> osz32 << oc 0x6F
        OpSize64 -> efail EInvalidOpSize64
      set_addrsize asz

    MOVS osz asz -> do
      assert_no_args
      case osz of
        OpSize8  ->          oc 0xA4
        OpSize16 -> osz16 << oc 0xA5
        OpSize32 -> osz32 << oc 0xA5
        OpSize64 -> osz64 << oc 0xA5
      set_addrsize asz

    STOS osz asz -> do
      assert_no_args
      case osz of
        OpSize8  ->          oc 0xAA
        OpSize16 -> osz16 << oc 0xAB
        OpSize32 -> osz32 << oc 0xAB
        OpSize64 -> osz64 << oc 0xAB
      set_addrsize asz

    LODS osz asz -> do
      assert_no_args
      case osz of
        OpSize8  ->          oc 0xAC
        OpSize16 -> osz16 << oc 0xAD
        OpSize32 -> osz32 << oc 0xAD
        OpSize64 -> osz64 << oc 0xAD
      set_addrsize asz

    CMPS osz asz -> do
      assert_no_args
      case osz of
        OpSize8  ->          oc 0xA6
        OpSize16 -> osz16 << oc 0xA7
        OpSize32 -> osz32 << oc 0xA7
        OpSize64 -> osz64 << oc 0xA7
      set_addrsize asz

    SCAS osz asz -> do
      assert_no_args
      case osz of
        OpSize8  ->          oc 0xAE
        OpSize16 -> osz16 << oc 0xAF
        OpSize32 -> osz32 << oc 0xAF
        OpSize64 -> osz64 << oc 0xAF
      set_addrsize asz


    PUSH -> case args of
      -- smaller forms for registers
      [R16 r] -> osz16 << oc_reg r << oc 0x50
      [R32 r] -> osz32 << oc_reg r << oc 0x50
      [R64 r] -> osz64 << oc_reg r << oc 0x50

      -- dead code because of the smaller forms above
      -- [R16 r] -> osz16 << rm_ext_reg 0x6 r << oc 0xFF
      -- [R32 r] -> osz32 << rm_ext_reg 0x6 r << oc 0xFF
      -- [R64 r] -> osz64 << rm_ext_reg 0x6 r << oc 0xFF

      [M16 m] -> osz16 << rm_ext_mem 0x6 m << oc 0xFF
      [M32 m] -> osz32 << rm_ext_mem 0x6 m << oc 0xFF
      [M64 m] -> osz64 << rm_ext_mem 0x6 m << oc 0xFF
      [I8 i]  ->          imm8  i          << oc 0x6A
      [I16 i] -> osz16 << imm16 i          << oc 0x68
      [I32 i] -> osz32 << imm32 i          << oc 0x68
      [OpSeg CS] | not mode64 -> oc 0x0E
      [OpSeg SS] | not mode64 -> oc 0x16
      [OpSeg DS] | not mode64 -> oc 0x1E
      [OpSeg ES] | not mode64 -> oc 0x06
      [OpSeg FS]              -> oc_0F 0xA0
      [OpSeg GS]              -> oc_0F 0xA8
      _ -> invalidArgs

    POP -> case args of
      -- smaller forms for registers
      [R16 r] -> osz16 << oc_reg r << oc 0x58
      [R32 r] -> osz32 << oc_reg r << oc 0x58
      [R64 r] -> osz64 << oc_reg r << oc 0x58

      -- dead code because of the smaller forms above
      -- [R16 r] -> osz16 << rm_ext_reg 0x0 r << oc 0x8F
      -- [R32 r] -> osz32 << rm_ext_reg 0x0 r << oc 0x8F
      -- [R64 r] -> osz64 << rm_ext_reg 0x0 r << oc 0x8F

      [M16 m] -> osz16 << rm_ext_mem 0x0 m << oc 0x8F
      [M32 m] -> osz32 << rm_ext_mem 0x0 m << oc 0x8F
      [M64 m] -> osz64 << rm_ext_mem 0x0 m << oc 0x8F
      -- TODO: the operand size can be used to select the stack increment. How
      -- do we provide this to users?
      [OpSeg SS] | not mode64 -> oc 0x17
      [OpSeg DS] | not mode64 -> oc 0x1F
      [OpSeg ES] | not mode64 -> oc 0x07
      [OpSeg FS]              -> oc_0F 0xA1
      [OpSeg GS]              -> oc_0F 0xA9
      _ -> invalidArgs

    POPA sz -> do
      assert_not_mode64
      assert_no_args
      case sz of
        OpSize16 -> osz16 << oc 0x61
        OpSize32 -> osz32 << oc 0x61
        _        -> invalidArgs

    PUSHA sz -> do
      assert_not_mode64
      assert_no_args
      case sz of
        OpSize16 -> osz16 << oc 0x60
        OpSize32 -> osz32 << oc 0x60
        _        -> invalidArgs

    LDS -> do
      assert_not_mode64
      case args of
        [R16 r, OpMem m] -> osz16 << rm_reg_mem r m << oc 0xC5
        [R32 r, OpMem m] -> osz32 << rm_reg_mem r m << oc 0xC5
        _ -> invalidArgs

    LES -> do
      assert_not_mode64
      case args of
        [R16 r, OpMem m] -> osz16 << rm_reg_mem r m << oc 0xC4
        [R32 r, OpMem m] -> osz32 << rm_reg_mem r m << oc 0xC4
        _ -> invalidArgs

    LSS -> do
      case args of
        [R16 r, OpMem m] -> osz16 << rm_reg_mem r m << oc_0F 0xB2
        [R32 r, OpMem m] -> osz32 << rm_reg_mem r m << oc_0F 0xB2
        [R64 r, OpMem m] -> osz64 << rm_reg_mem r m << oc_0F 0xB2
        _ -> invalidArgs

    LFS -> do
      case args of
        [R16 r, OpMem m] -> osz16 << rm_reg_mem r m << oc_0F 0xB4
        [R32 r, OpMem m] -> osz32 << rm_reg_mem r m << oc_0F 0xB4
        [R64 r, OpMem m] -> osz64 << rm_reg_mem r m << oc_0F 0xB4
        _ -> invalidArgs

    LGS -> do
      case args of
        [R16 r, OpMem m] -> osz16 << rm_reg_mem r m << oc_0F 0xB5
        [R32 r, OpMem m] -> osz32 << rm_reg_mem r m << oc_0F 0xB5
        [R64 r, OpMem m] -> osz64 << rm_reg_mem r m << oc_0F 0xB5
        _ -> invalidArgs

    Jcc cc -> case args of
      [I8  i] -> imm8 i << oc (0x70 + condCode cc)
      [I16 i] -> do
        assert_not_mode64
        osz16 << imm16 i << oc_0F (0x80 + condCode cc)
      [I32 i] -> osz32 << imm32 i << oc_0F (0x80 + condCode cc)
      _       -> invalidArgs

    LOOP -> case args of
      [R16 R_CX,  I8 i] -> set_addrsize AddrSize16 << imm8 i << oc 0xE2
      [R32 R_ECX, I8 i] -> set_addrsize AddrSize32 << imm8 i << oc 0xE2
      [R64 R_RCX, I8 i] -> set_addrsize AddrSize64 << imm8 i << oc 0xE2
      _ -> invalidArgs

    LOOPE -> case args of
      [R16 R_CX,  I8 i] -> set_addrsize AddrSize16 << imm8 i << oc 0xE1
      [R32 R_ECX, I8 i] -> set_addrsize AddrSize32 << imm8 i << oc 0xE1
      [R64 R_RCX, I8 i] -> set_addrsize AddrSize64 << imm8 i << oc 0xE1
      _ -> invalidArgs

    LOOPNE -> case args of
      [R16 R_CX,  I8 i] -> set_addrsize AddrSize16 << imm8 i << oc 0xE0
      [R32 R_ECX, I8 i] -> set_addrsize AddrSize32 << imm8 i << oc 0xE0
      [R64 R_RCX, I8 i] -> set_addrsize AddrSize64 << imm8 i << oc 0xE0
      _ -> invalidArgs

    JMP -> case args of
      [I8  i] -> imm8 i << oc 0xEB
      [I16 i]
        | not mode64
        -> osz16 << imm16 i << oc 0xE9
      [I32 i] -> osz32 << imm32 i << oc 0xE9
      [R16 r]
        | not mode64
        -> osz16 << rm_ext_reg 0x4 r << oc 0xFF
      [R32 r]
        | not mode64
        -> osz32 << rm_ext_reg 0x4 r << oc 0xFF
      [R64 r]
        | mode64 -- default to 64-bit size
        -> rm_ext_reg 0x4 r << oc 0xFF
      [M16 m]
        | not mode64
        -> osz16 << rm_ext_mem 0x4 m << oc 0xFF
      [M32 m]
        | not mode64
        -> osz32 << rm_ext_mem 0x4 m << oc 0xFF
      [M64 m]
        | mode64 -- default to 64-bit size
        -> rm_ext_mem 0x4 m << oc 0xFF
      -- TODO: other forms: ptr k:n, m k:n
      _       -> invalidArgs

    INTO -> do
      assert_not_mode64
      assert_no_args
      oc 0xCE

    INT1 -> assert_no_args >> oc 0xF1

    INT3 -> assert_no_args >> oc 0xCC

    INT  -> case args of
      [I8 i] -> imm8 i << oc 0xCD
      _      -> invalidArgs

    SYSCALL -> do
      assert_mode64
      assert_no_args
      oc_0F 0x05

    SYSRET comp_mode -> do
      assert_mode64
      assert_no_args
      -- do we switch to 32-bit compatibility mode?
      if comp_mode
        then                 oc_0F 0x07
        else osz64 << oc_0F 0x07

    SYSENTER -> do
      assert_no_args
      oc_0F 0x34

    SYSEXIT comp_mode -> do
      assert_no_args
      -- do we switch to 32-bit compatibility mode?
      if comp_mode
        then                 oc_0F 0x35
        else osz64 << oc_0F 0x35

    LEAVE osz -> do
      assert_no_args
      case osz of
        OpSize64 | mode64     -> oc 0xC9
        OpSize32 | not mode64 -> osz32 << oc 0xC9
        OpSize16              -> osz16 << oc 0xC9
        _ -> invalidArgs

    RET -> case args of
      []      -> oc 0xC3
      [I16 i] -> imm16 i << oc 0xC2
      _       -> invalidArgs

    RET_FAR -> case args of
      []      -> oc 0xCB
      [I16 i] -> imm16 i << oc 0xCA
      _       -> invalidArgs

    CALL -> case args of
      [I16 i] -> osz16 << imm16 i << oc 0xE8
      [I32 i] -> osz32 << imm32 i << oc 0xE8
      [R16 r] | not mode64 -> osz16 << rm_ext_reg 0x2 r << oc 0xFF
      [R32 r] | not mode64 -> osz32 << rm_ext_reg 0x2 r << oc 0xFF
      [M16 m] | not mode64 -> osz16 << rm_ext_mem 0x2 m << oc 0xFF
      [M32 m] | not mode64 -> osz32 << rm_ext_mem 0x2 m << oc 0xFF
      -- default to 64-bit operand size in 64-bit mode
      [R64 r]              ->          rm_ext_reg 0x2 r << oc 0xFF
      [M64 m]              ->          rm_ext_mem 0x2 m << oc 0xFF
      _ -> invalidArgs

    CALL_FAR -> noEncoding -- TODO: far CALLs

    UD0 -> case args of
      [R32 r, R32 rm] -> rm_reg_reg r rm << oc_0F 0xFF
      [R32 r, M32 rm] -> rm_reg_mem r rm << oc_0F 0xFF
      _ -> invalidArgs

    UD1 -> case args of
      [R32 r, R32 rm] -> rm_reg_reg r rm << oc_0F 0xB9
      [R32 r, M32 rm] -> rm_reg_mem r rm << oc_0F 0xB9
      _ -> invalidArgs

    UD2 -> do
      assert_no_args
      oc_0F 0x0B

    CMC -> assert_no_args >> oc 0xF5
    CLC -> assert_no_args >> oc 0xF8
    STC -> assert_no_args >> oc 0xF9
    CLI -> assert_no_args >> oc 0xFA
    STI -> assert_no_args >> oc 0xFB
    CLD -> assert_no_args >> oc 0xFC
    STD -> assert_no_args >> oc 0xFD

    LAHF -> assert_no_args >> oc 0x9F
    SAHF -> assert_no_args >> oc 0x9E

    POPF sz -> do
      assert_no_args
      case sz of
        OpSize8  -> efail EInvalidOpSize8
        OpSize16 -> osz16 << oc 0x9D
        OpSize32 -> osz32 << oc 0x9D
        OpSize64 -> osz64 << oc 0x9D

    PUSHF sz -> do
      assert_no_args
      case sz of
        OpSize8  -> efail EInvalidOpSize8
        OpSize16 -> osz16 << oc 0x9C
        OpSize32 -> osz32 << oc 0x9C
        OpSize64 -> osz64 << oc 0x9C

    CPUID -> do
      assert_no_args
      oc_0F 0xA2

    PAUSE -> do
      assert_no_args
      p_F3 << oc 0x90

    NOP sz -> do
      let mem a = Mem Nothing Nothing NoLock a Nothing
      let ra = if mode64 then Just R_RAX else Just R_EAX
      case sz of
        1 ->         oc 0x90
        2 -> p_66 << oc 0x90
        -- multi-byte NOPs as recommended in Intel manual
        -- Not supported on all models
        3 ->         rm_ext_mem 0x0 (mem (MemAbs ra Scale1 Nothing NoDisp))     << oc_0F 0x1F
        4 ->         rm_ext_mem 0x0 (mem (MemAbs ra Scale1 Nothing (Disp8 0)))  << oc_0F 0x1F
        5 ->         rm_ext_mem 0x0 (mem (MemAbs ra Scale1 ra      (Disp8 0)))  << oc_0F 0x1F
        6 -> p_66 << rm_ext_mem 0x0 (mem (MemAbs ra Scale1 ra      (Disp8 0)))  << oc_0F 0x1F
        7 ->         rm_ext_mem 0x0 (mem (MemAbs ra Scale1 Nothing (Disp32 0))) << oc_0F 0x1F
        8 ->         rm_ext_mem 0x0 (mem (MemAbs ra Scale1 ra      (Disp32 0))) << oc_0F 0x1F
        9 -> p_66 << rm_ext_mem 0x0 (mem (MemAbs ra Scale1 ra      (Disp32 0))) << oc_0F 0x1F
        _ -> invalidArgs

    MWAIT -> do
      assert_no_args
      set_modrm 0xC9 << oc_0F 0x01

    ADCX -> do
      require_extension Ext.ADX
      case args of
        [R32 r, M32 m] -> do
          rm_reg_mem r m << p_66 << oc_0F38 0xF6
        [R64 r, M64 m] -> do
          assert_mode64
          osz64 << rm_reg_mem r m << p_66 << oc_0F38 0xF6
        [R32 r, R32 rm] -> do
          rm_reg_reg r rm << p_66 << oc_0F38 0xF6
        [R64 r, R64 rm] -> do
          assert_mode64
          osz64 << rm_reg_reg r rm << p_66 << oc_0F38 0xF6
        _ -> invalidArgs

    ADOX -> do
      require_extension Ext.ADX
      case args of
        [R32 r, M32 m] -> do
          rm_reg_mem r m << p_F3 << oc_0F38 0xF6
        [R64 r, M64 m] -> do
          assert_mode64
          osz64 << rm_reg_mem r m << p_F3 << oc_0F38 0xF6
        [R32 r, R32 rm] -> do
          rm_reg_reg r rm << p_F3 << oc_0F38 0xF6
        [R64 r, R64 rm] -> do
          assert_mode64
          osz64 << rm_reg_reg r rm << p_F3 << oc_0F38 0xF6
        _ -> invalidArgs

    STR -> case args of
      [M16 m] -> rm_ext_mem 0x1 m << oc_0F 0x00
      -- it is zero-extended to the whole register.
      [R16 r] -> rm_ext_reg 0x1 r << oc_0F 0x00
      _ -> invalidArgs

    LTR -> case args of
      [M16 m] -> rm_ext_mem 0x3 m << oc_0F 0x00
      -- it is zero-extended to the whole register.
      [R16 r] -> rm_ext_reg 0x3 r << oc_0F 0x00
      _ -> invalidArgs

    SGDT -> do
      case args of
        [OpMem m] -> rm_ext_mem 0x0 m << oc_0F 0x01
        _ -> invalidArgs

    LGDT -> do
      case args of
        [OpMem m] -> rm_ext_mem 0x2 m << oc_0F 0x01
        _ -> invalidArgs

    SLDT -> do
      case args of
        [M16 m] -> rm_ext_mem 0x0 m << oc_0F 0x00
        [R16 r] -> rm_ext_reg 0x0 r << oc_0F 0x00
        _ -> invalidArgs

    LLDT -> do
      case args of
        [M16 m] -> rm_ext_mem 0x2 m << oc_0F 0x00
        [R16 r] -> rm_ext_reg 0x2 r << oc_0F 0x00
        _ -> invalidArgs

    SIDT -> do
      case args of
        [OpMem m] -> rm_ext_mem 0x1 m << oc_0F 0x01
        _ -> invalidArgs

    LIDT -> do
      case args of
        [OpMem m] -> rm_ext_mem 0x3 m << oc_0F 0x01
        _ -> invalidArgs

    RDFSBASE -> do
      assert_mode64
      require_extension Ext.FSGSBASE
      case args of
        [R32 r] ->          rm_ext_reg 0x0 r << p_F3 << oc_0F 0xAE
        [R64 r] -> osz64 << rm_ext_reg 0x0 r << p_F3 << oc_0F 0xAE
        _ -> invalidArgs

    RDGSBASE -> do
      assert_mode64
      require_extension Ext.FSGSBASE
      case args of
        [R32 r] ->          rm_ext_reg 0x1 r << p_F3 << oc_0F 0xAE
        [R64 r] -> osz64 << rm_ext_reg 0x1 r << p_F3 << oc_0F 0xAE
        _ -> invalidArgs

    WRFSBASE -> do
      assert_mode64
      require_extension Ext.FSGSBASE
      case args of
        [R32 r] ->          rm_ext_reg 0x2 r << p_F3 << oc_0F 0xAE
        [R64 r] -> osz64 << rm_ext_reg 0x2 r << p_F3 << oc_0F 0xAE
        _ -> invalidArgs

    WRGSBASE -> do
      assert_mode64
      require_extension Ext.FSGSBASE
      case args of
        [R32 r] ->          rm_ext_reg 0x3 r << p_F3 <<  oc_0F 0xAE
        [R64 r] -> osz64 << rm_ext_reg 0x3 r << p_F3 <<  oc_0F 0xAE
        _ -> invalidArgs

    SWAPGS -> do
      assert_mode64
      assert_no_args
      set_modrm 0xF8 << oc_0F 0x01

    RDPID -> do
      require_extension Ext.RDPID
      case args of
        [R32 r] | not mode64 -> rm_ext_reg 0x7 r << p_F3 << oc_0F 0xC7
        [R64 r] |     mode64 -> rm_ext_reg 0x7 r << p_F3 << oc_0F 0xC7
        _ -> invalidArgs

    RDTSC -> do
      assert_no_args
      oc_0F 0x31

    RDTSCP -> do
      assert_no_args
      set_modrm 0xF9 << oc_0F 0x01

    LSL -> case args of
      [R16 d, R16 s] -> osz16 << rm_reg_reg d s << oc_0F 0x03
      [R16 d, M16 s] -> osz16 << rm_reg_mem d s << oc_0F 0x03
      [R32 d, R32 s] -> osz32 << rm_reg_reg d s << oc_0F 0x03
      [R32 d, M16 s] -> osz32 << rm_reg_mem d s << oc_0F 0x03
      [R64 d, R32 s] -> osz64 << rm_reg_reg d s << oc_0F 0x03
      [R64 d, M16 s] -> osz64 << rm_reg_mem d s << oc_0F 0x03
      _ -> invalidArgs

    PREFETCHW -> do
      require_extension Ext.PREFETCHW
      case args of
        [OpMem m] -> rm_ext_mem 0x1 m << oc_0F 0x0D
        _ -> invalidArgs

    MFENCE -> do
      require_extension Ext.SSE2
      assert_no_args
      -- Can be encoded as 0F AE Fx, where x in the range 0-7 (i.e. ModRM.rm
      -- field is ignored). We use 0F AE F0.
      set_modrm 0xF0 << oc_0F 0xAE

    SFENCE -> do
      require_extension Ext.SSE
      assert_no_args
      -- Can be encoded as 0F AE Fx, where x in the range 8-F (i.e. ModRM.rm
      -- field is ignored). We use 0F AE F8.
      set_modrm 0xF8 << oc_0F 0xAE

    LFENCE -> do
      require_extension Ext.SSE2
      assert_no_args
      -- Can be encoded as 0F AE Ex, where x in the range 8-F (i.e. ModRM.rm
      -- field is ignored). We use 0F AE E8.
      set_modrm 0xE8 << oc_0F 0xAE


    ADDPS ->
      case args of
        [V128 v, M128 m] -> do
          require_extension Ext.SSE
          rm_vec_mem v m << oc_0F 0x58
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE
          rm_vec_vec v1 v2 << oc_0F 0x58
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs

    ADDPD ->
      case args of
        [V128 v, M128 m] -> do
          require_extension Ext.SSE2
          rm_vec_mem v m << p_66 << oc_0F 0x58
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE2
          rm_vec_vec v1 v2 << p_66 << oc_0F 0x58
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs

    ADDSS ->
      case args of
        [V128 v, M32 m] -> do
          require_extension Ext.SSE
          rm_vec_mem v m << p_F3 << oc_0F 0x58
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE
          rm_vec_vec v1 v2 << p_F3 << oc_0F 0x58
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs

    ADDSD ->
      case args of
        [V128 v, M32 m] -> do
          require_extension Ext.SSE2
          rm_vec_mem v m << p_F2 << oc_0F 0x58
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE2
          rm_vec_vec v1 v2 << p_F2 << oc_0F 0x58
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs

    SUBPS ->
      case args of
        [V128 v, M128 m] -> do
          require_extension Ext.SSE
          rm_vec_mem v m << oc_0F 0x5C
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE
          rm_vec_vec v1 v2 << oc_0F 0x5C
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs

    SUBPD ->
      case args of
        [V128 v, M128 m] -> do
          require_extension Ext.SSE2
          rm_vec_mem v m << p_66 << oc_0F 0x5C
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE2
          rm_vec_vec v1 v2 << p_66 << oc_0F 0x5C
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs

    SUBSS ->
      case args of
        [V128 v, M32 m] -> do
          require_extension Ext.SSE
          rm_vec_mem v m << p_F3 << oc_0F 0x5C
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE
          rm_vec_vec v1 v2 << p_F3 << oc_0F 0x5C
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs

    SUBSD ->
      case args of
        [V128 v, M32 m] -> do
          require_extension Ext.SSE2
          rm_vec_mem v m << p_F2 << oc_0F 0x5C
        [V128 v1, V128 v2] -> do
          require_extension Ext.SSE2
          rm_vec_vec v1 v2 << p_F2 << oc_0F 0x5C
        -- TODO: add VEX and EVEX encodings
        _ -> invalidArgs
