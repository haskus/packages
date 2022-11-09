{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Haskus.Arch.X86_64.ISA.Ops where

import Data.Word
import Data.Int
import Data.Bits
import qualified Control.Monad.State as S
import Data.Bifunctor

import Haskus.Arch.X86_64.ISA.Output

-- General purpose instructions
data GPInsn cond mem reg imm
  -- Data transfer
  = MoveMemToReg mem reg
  | MoveRegToMem reg mem
  | MoveRegToReg reg reg
  | MoveImmToReg imm reg
  | MoveImmToMem imm mem

  | CondMoveMemToReg cond mem reg
  | CondMoveRegToMem cond reg mem
  | CondMoveRegToReg cond reg reg
  | CondMoveImmToReg cond imm reg
  | CondMoveImmToMem cond imm mem

  -- Exchange
  | Exchange
  | ExchangeThenAdd
  | CompareExchange Lock

  | ByteSwap

  -- Stack manipulation
  | StackPush
  | StackPop
  | StackPushAllRegs
  | StackPopAllRegs

  -- Conversions
  | SignExtendInplace   -- CBW/CWDE
  | SignExtensionInto   -- CWD/CDQ
  | SignExtendInto      -- MOVSX/MOVSXD
  | ZeroExtendInto      -- MOVZX

  -- Arithmetic
  | Add
  | AddWithCarry
  | Sub
  | SubWithBorrow
  | Increment
  | Decrement
  | Negate
  | MultiplyUnsigned
  | MultiplySigned
  | DivideUnsigned
  | DivideSigned

  -- Comparison
  | Compare
  | Test

  -- Logical
  | And
  | Or
  | Xor
  | Not
  | ShiftLeft
  | ShiftRight
  | ShiftRightSigned
  | ShiftLeftInto           -- SHLD
  | ShiftRightInto          -- SHRD
  | RotateLeft              -- ROL
  | RotateRight             -- ROR
  | RotateThroughCarryLeft  -- RCL
  | RotateThroughCarryRight -- RCR

  -- Bit test
  | BitTest
  | BitTestAndSet
  | BitTestAndClear
  | BitTestAndComplement

  -- Bit scan
  | BitScanLowToHigh -- BSF
  | BitScanHighToLow -- BSR

  -- Byte set
  | CondSet cond -- SETcc

  -- Control transfer
  | Jump
  | Call
  | Return
  | CondJump
  | Loop LoopCond
  | JumpOnZeroCounter -- JCXZ/JECXZ
  | ProcEnter         -- ENTER
  | ProcLeave         -- LEAVE

  -- Interruptions
  | Interrupt
  | InterruptReturn
  | InterruptOnOverflow -- INTO

  -- String instructions
  -- TODO
  -- (MOVS, CMPS, SCAS, LODS, STOS)
  -- (REP, REPE/Z, REPNE/NZ)

  -- I/O instructions
  | InputIntoReg        -- IN
  | InputIntoString     -- INS
  | OutputFromReg       -- OUT
  | OutputFromString    -- OUTS

  -- Flags control
  | SetCarryFlag        -- STC
  | ClearCarryFlag      -- CLC
  | ComplementCarryFlag -- CMC
  | SetDirectionFlag    -- STD
  | ClearDirectionFlag  -- CLD
  | LoadFlags           -- LAHF
  | StoreFlags          -- SAHF
  | PushFlags           -- PUSHF
  | PopFlags            -- POPF
  | PushAllFlags        -- PUSHFD
  | PopAllFlags         -- POPFD
  | SetInterruptFlag    -- STI
  | ClearInterruptFlag  -- CLI

  -- Miscellaneous
  | ComputeAddress      -- LEA
  | TableLookup         -- XLAT/XLATB
  | CPUID
  | NOP
  | Undefined           -- UD (invalid opcode)

data Lock
  = Lock   -- ^ Lock memory access during the operation
  | NoLock -- ^ Don't lock memory

data LoopCond


-- | Relocation
data Reloc mark loc
  = RelocImm8 !mark !loc
      -- ^ Write imm8 value for marker at the given location
  | RelocImm16 !mark !loc
      -- ^ Write imm16 value for marker at the given location
  | RelocImm32 !mark !loc
      -- ^ Write imm32 value for marker at the given location
  deriving (Show)

data DefaultOperandSize
  = DefaultOperandSize16
  | DefaultOperandSize32
  deriving (Show)

-- | Assembler code generator type-class
--
-- For most instructions, it supports value arguments or markers. Markers
-- produce relocations that can be fixed up in a following pass.
class Output m => Asm m where
  -- | Get current default operand size
  getOperandSize :: m DefaultOperandSize

data CGState = CGState
  { cg_bytes   :: ![Word8]
  , cg_pos     :: !Location
  , cg_op_size :: !DefaultOperandSize
  }
  deriving (Show)

initCGState :: CGState
initCGState = CGState [] 0 DefaultOperandSize32


newtype CodeGen a = CodeGen (S.State CGState a)
  deriving newtype (Functor,Applicative,Monad)

instance Output CodeGen where
  putW8 b    = CodeGen $ S.modify \s -> s
                  { cg_bytes = b : cg_bytes s
                  , cg_pos   = cg_pos s + 1
                  }
  getLoc = CodeGen (S.gets cg_pos)

instance Asm CodeGen where
  getOperandSize = CodeGen (S.gets cg_op_size)

runCodeGen :: CodeGen a -> (a, CGState)
runCodeGen (CodeGen m) = second fix_state $ S.runState m initCGState
  where
    fix_state s = s
      { cg_bytes  = reverse (cg_bytes s)
      }

newtype LocImm8  = LocImm8  Location
newtype LocImm16 = LocImm16 Location
newtype LocImm32 = LocImm32 Location
newtype LocImm64 = LocImm64 Location

-- | Location of a sign-extended 32-bit value
newtype LocImm32SE = LocImm32SE Location

-- | Enable 16-bit operand size if it isn't the default
setOperandSize16 :: Asm m => m ()
setOperandSize16 = getOperandSize >>= \case
  DefaultOperandSize16 -> pure ()
  DefaultOperandSize32 -> putW8 0x66

-- | Enable 32-bit operand size if it isn't the default
setOperandSize32 :: Asm m => m ()
setOperandSize32 = getOperandSize >>= \case
  DefaultOperandSize16 -> putW8 0x66
  DefaultOperandSize32 -> pure ()
  
-- | Enable 64-bit operand size (REX.W)
putRexW :: Asm m => m ()
putRexW = putW8 0b01001000

data REX_B
  = REX_B_0
  | REX_B_1

data REX_R
  = REX_R_0
  | REX_R_1

data REX_W
  = REX_W_0
  | REX_W_1

data REX_X
  = REX_X_0
  | REX_X_1

data NeedRex
  = MayHaveRex
  | MustntHaveRex
  | MustHaveRex

-- | Put REX prefix
putRex :: Asm m => REX_W -> REX_R -> REX_X -> REX_B -> m ()
{-# INLINE putRex #-} -- inline as it often simplifies
putRex w r x b = putW8 (0b0100000
                    .|. fromREX_W w
                    .|. fromREX_R r
                    .|. fromREX_X x
                    .|. fromREX_B b)
  where
    fromREX_W = \case
      REX_W_0 -> 0b0000
      REX_W_1 -> 0b1000
    fromREX_R = \case
      REX_R_0 -> 0b0000
      REX_R_1 -> 0b0100
    fromREX_X = \case
      REX_X_0 -> 0b0000
      REX_X_1 -> 0b0010
    fromREX_B = \case
      REX_B_0 -> 0b0000
      REX_B_1 -> 0b0001

putOpcode :: Asm m => Word8 -> m ()
putOpcode = putW8

putImm8 :: Asm m => Word8 -> m LocImm8
putImm8 v = do
  loc <- LocImm8 <$> getLoc
  putW8 v
  pure loc

putImm16 :: Asm m => Word16 -> m LocImm16
putImm16 v = do
  loc <- LocImm16 <$> getLoc
  putW16 v
  pure loc

putImm32 :: Asm m => Word32 -> m LocImm32
putImm32 v = do
  loc <- LocImm32 <$> getLoc
  putW32 v
  pure loc

putImm32SE :: Asm m => Word32 -> m LocImm32SE
putImm32SE v = do
  loc <- LocImm32SE <$> getLoc
  putW32 v
  pure loc

----------------
-- ModRM
----------------

newtype ModRM = RawModRM Word8

data Mod
  = Mod00
  | Mod01
  | Mod10
  | Mod11

fromMod :: Mod -> Word8
fromMod = \case
  Mod00 -> 0b00
  Mod01 -> 0b01
  Mod10 -> 0b10
  Mod11 -> 0b11

toMod :: Word8 -> Mod
toMod = \case
  0b00 -> Mod00
  0b01 -> Mod01
  0b10 -> Mod10
  _    -> Mod11


{-# COMPLETE ModRM #-}
pattern ModRM :: Mod -> Word3 -> Word3 -> ModRM
pattern ModRM m r rm <- (extractModRM -> (m,r,rm))
  where
    ModRM m r rm = RawModRM ((fromMod m `unsafeShiftL` 6) .|. (fromWord3 r `unsafeShiftL` 3) .|. fromWord3 rm)

extractModRM :: ModRM -> (Mod, Word3, Word3)
extractModRM (RawModRM w) =
  ( toMod (w `unsafeShiftR` 6)
  , Word3 ((w `unsafeShiftR` 3) .&. 0b111)
  , Word3 (w .&. 0b111)
  )

putModRM :: Asm m => ModRM -> m ()
putModRM (RawModRM w) = putW8 w

-- | Assert that a Word8 is on 3 bits
assertWord3 :: Word8 -> Word3
assertWord3 w
  | w .&. 0b11111000 == 0 = Word3 w
  | otherwise = error $ "assertWord3: number too large (" ++ show w ++ ")"

-- | 3-bit word
newtype Word3 = Word3 { fromWord3 :: Word8 }

instance Num Word3 where
  fromInteger v         = assertWord3 (fromInteger v)
  (Word3 a) + (Word3 b) = Word3 ((a+b) .&. 0b111)
  (Word3 a) * (Word3 b) = Word3 ((a*b) .&. 0b111)
  abs a                 = a
  signum (Word3 0)      = Word3 0
  signum _              = Word3 1
  negate _              = error "negate for Word3 not supported"

modRM_OpcodeReg :: Word3 -> Word3 -> ModRM
modRM_OpcodeReg opcode reg = ModRM Mod11 opcode reg

-- | Some registers are encoded with 3 bits, some others with 4 bits, some
-- others can be encoded with either 3 or 4 bits.
--
-- The number of available bits to encode a register is variable because
-- prefixes (especially REX) are optional and contain the additional bits.
--
-- That's why some registers encoded only on 3-bits (AH, BH, CH, DH) can't be
-- encoded when a REX prefix is used (for another reason). That's why some
-- registers encoded only on 4-bits (SIL, DIL, SPL, BPL) can't be encoded
-- whithout a REX prefix.
--
-- We can't just encode a register with its number, we need to keep track of its
-- number of bits. We use the 2 MSB of the reg code to indicate if it must be encoded on
-- 3 bits (MSB = 0b01), on 4 bits (MSB = 0b10), or on any number of bits (MSB =
-- 0b00). The actual value of the additional bit is at its expected position:
-- bit 3 (counting from 0).
--
-- We could use an ADT instead of packing RegBits and RegNum in a Word8. This is
-- an example of premature optimization.
newtype RegCode = RawRegCode Word8

data RegBits
  = Only3Bits
  | Only4Bits
  | AnyBits

fromRegBits :: RegBits -> Word8
fromRegBits = \case
  AnyBits   -> 0b00000000
  Only3Bits -> 0b01000000
  Only4Bits -> 0b10000000

toRegBits :: Word8 -> RegBits
toRegBits w = case w .&. 0b1100000 of
  0b00000000         -> AnyBits
  0b01000000         -> Only3Bits
  _ {- 0b10000000 -} -> Only4Bits

toRegNum :: Word8 -> RegNum
toRegNum w = RegNum (w .&. 0b00111111)

newtype RegNum = RegNum { fromRegNum :: Word8 }

{-# COMPLETE RegCode #-}
pattern RegCode :: RegBits -> RegNum -> RegCode
pattern RegCode b n <- (extractRegCode -> (b,n))
  where
    RegCode b n = toRegCode b n

extractRegCode :: RegCode -> (RegBits, RegNum)
extractRegCode (RawRegCode w) = (toRegBits w, toRegNum w)

toRegCode :: RegBits -> RegNum -> RegCode
toRegCode b n = RawRegCode (fromRegBits b .|. fromRegNum n)

data MayEncode a
  = MustEncode a
  | MayEncode  a
  | DontEncode
  deriving (Functor)

-- | Indicate if a register encoding requires an additional bit, and its value.
regEncoding :: RegCode -> (MayEncode Bool, Word3)
regEncoding (RegCode bits num) =
  let n = fromRegNum num
  in case bits of
      Only3Bits -> (DontEncode              , Word3 n)
      Only4Bits -> (MustEncode (testBit n 3), Word3 (n .&. 0b0111))
      AnyBits   -> (MayEncode False         , Word3 n)

-- | Encode a register in RM field of ModRM
--
-- TODO: return ModRM_RM value instead of Word3
regRMEncoding :: RegCode -> (MayEncode REX_B, Word3)
regRMEncoding c = (fmap toREX_B mextra, n)
  where
    (mextra,n) = regEncoding c
    toREX_B = \case
      False -> REX_B_0
      True  -> REX_B_1

-- | Encode a register in Reg field of ModRM
--
-- TODO: return ModRM_Reg value instead of Word3
regRegEncoding :: RegCode -> (MayEncode REX_R, Word3)
regRegEncoding c = (fmap toREX_R mextra, n)
  where
    (mextra,n) = regEncoding c
    toREX_R = \case
      False -> REX_R_0
      True  -> REX_R_1
  

-- ========================================
-- Instructions
-- ========================================

class Put a where
  type PutResult a
  put :: Asm m => a -> m (PutResult a)

instance Put Word8  where
  type PutResult Word8 = ()
  put = putW8

instance Put Word16 where
  type PutResult Word16 = ()
  put = putW16

instance Put Word32 where
  type PutResult Word32 = ()
  put = putW32

instance Put Word64 where
  type PutResult Word64 = ()
  put = putW64

instance Put Int8   where
  type PutResult Int8 = ()
  put = putI8

instance Put Int16  where
  type PutResult Int16 = ()
  put = putI16

instance Put Int32  where
  type PutResult Int32 = ()
  put = putI32

instance Put Int64  where
  type PutResult Int64 = ()
  put = putI64

------------------------------------------
-- ADC: add with carry
------------------------------------------

-- Variants
-----------

-- | Add with carry 8-bit constant to AL
newtype ADC_AL_i8 = ADC_AL_i8 Word8

-- | Add with carry 16-bit constant to AX
newtype ADC_AX_i16 = ADC_AX_i16 Word16

-- | Add with carry 32-bit constant to EAX
newtype ADC_EAX_i32 = ADC_EAX_i32 Word32

-- | Add with carry sign-extended 32-bit constant to RAX
newtype ADC_RAX_i32 = ADC_RAX_i32 Word32

-- | Add with carry 8-bit constant to 8-bit register
data ADC_r8_i8 = ADC_r8_i8 RegCode Word8

-- | Add with carry 16-bit constant to 16-bit register
data ADC_r16_i16 = ADC_r16_i16 RegCode Word16

-- | Add with carry 32-bit constant to 32-bit register
data ADC_r32_i32 = ADC_r32_i32 RegCode Word32

-- | Add with carry sign-extended 32-bit constant to 64-bit register
data ADC_r64_i32 = ADC_r64_i32 RegCode Word32

-- | Add with carry 8-bit constant to 8-bit memory
--data ADC_m8_i8 = ADC_m8_i8 EA Word8

-- Machine code generation
--------------------------

instance Put ADC_AL_i8 where
  -- return offset of the imm8 value
  type PutResult ADC_AL_i8 = LocImm8

  put (ADC_AL_i8 v) = do
    putOpcode 0x14
    putImm8 v

instance Put ADC_AX_i16 where
  -- return offset of the imm16 value
  type PutResult ADC_AX_i16 = LocImm16

  put (ADC_AX_i16 v) = do
    setOperandSize16
    putOpcode 0x15
    putImm16 v

instance Put ADC_EAX_i32 where
  -- return offset of the imm32 value
  type PutResult ADC_EAX_i32 = LocImm32

  put (ADC_EAX_i32 v) = do
    setOperandSize32
    putOpcode 0x15
    putImm32 v

instance Put ADC_RAX_i32 where
  -- return offset of the imm32 value
  type PutResult ADC_RAX_i32 = LocImm32SE

  put (ADC_RAX_i32 v) = do
    putRexW
    putOpcode 0x15
    putImm32SE v

instance Put ADC_r8_i8 where
  -- return offset of the imm8 value
  type PutResult ADC_r8_i8 = LocImm8

  put (ADC_r8_i8 r v) = do
    let (m_rex_b, r_lsb) = regRMEncoding r
    case m_rex_b of
      DontEncode       -> pure ()
      MayEncode _rex_b -> pure ()
      MustEncode rex_b -> putRex REX_W_0 REX_R_0 REX_X_0 rex_b
    putOpcode 0x80
    putModRM (modRM_OpcodeReg 2 r_lsb)
    putImm8 v

instance Put ADC_r16_i16 where
  -- return offset of the imm16 value
  type PutResult ADC_r16_i16 = LocImm16

  put (ADC_r16_i16 r v) = do
    setOperandSize16
    let (m_rex_b, r_lsb) = regRMEncoding r
    case m_rex_b of
      DontEncode       -> pure ()
      MayEncode _rex_b -> pure ()
      MustEncode rex_b -> putRex REX_W_0 REX_R_0 REX_X_0 rex_b
    putOpcode 0x81
    putModRM (modRM_OpcodeReg 2 r_lsb)
    putImm16 v

instance Put ADC_r32_i32 where
  -- return offset of the imm32 value
  type PutResult ADC_r32_i32 = LocImm32

  put (ADC_r32_i32 r v) = do
    setOperandSize32
    let (m_rex_b, r_lsb) = regRMEncoding r
    case m_rex_b of
      DontEncode       -> pure ()
      MayEncode _rex_b -> pure ()
      MustEncode rex_b -> putRex REX_W_0 REX_R_0 REX_X_0 rex_b
    putOpcode 0x81
    putModRM (modRM_OpcodeReg 2 r_lsb)
    putImm32 v

instance Put ADC_r64_i32 where
  -- return offset of the imm32 value
  type PutResult ADC_r64_i32 = LocImm32SE

  put (ADC_r64_i32 r v) = do
    let (m_rex_b, r_lsb) = regRMEncoding r
    case m_rex_b of
      DontEncode       -> error "ADC_r64_i32: can't encode register argument"
      MayEncode  rex_b -> putRex REX_W_1 REX_R_0 REX_X_0 rex_b
      MustEncode rex_b -> putRex REX_W_1 REX_R_0 REX_X_0 rex_b
    putOpcode 0x81
    putModRM (modRM_OpcodeReg 2 r_lsb)
    putImm32SE v

-- > runCodeGen $ putADC_AL_imm8 (V 15) >> putADC_AL_imm8 (M "Imm8 marker") >> putADC_AL_imm8 (V 27) >> putADC_AX_imm16 (V 0x0102)
-- ((),CGState {cg_relocs = [RelocImm8 "Imm8 marker" 3], cg_bytes = [20,15,20,0,20,27,102,21,2,1], cg_pos = 10, cg_op_size = DefaultOperandSize32})
