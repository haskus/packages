{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Instruction encoding
module Haskus.Arch.X86_64.ISA.Encoding
   ( -- * Encoding
     Encoding (..)
   , OpcodeBit (..)
   , OpcodeEncoding (..)
   , EncodingProperties(..)
   , HLEAction (..)
   , ValidMod (..)
   , hasImmediate
   , encSupportHLE
   , encSupportMode
   , encRequiredExtensions
   , encSupportExtensions
   , ctxSupportEncoding
   , encSupportPrefix
   , encValidModRMMode
   , encAllowPrefix66
   , encMayHaveMemoryOperand
   , isLegacyEncoding
   , isVexEncoding
   , encLockable
   , encRepeatable
   , encBranchHintable
   , encRequireModRM
   , encGenerateOpcodes
   , setAddrFam
   -- * Generic opcode
   , Opcode (..)
   , OpcodeMap (..)
   , LegacyMap (..)
   , opcodeByte
   , opcodeMap
   , opcodeB
   , opcodeR
   , opcodeX
   , opcodeW
   , opcodeL
   -- * Legacy prefixes
   , module Haskus.Arch.X86_64.ISA.Encoding.Prefix
   -- * REX prefix
   , module Haskus.Arch.X86_64.ISA.Encoding.Rex
   -- * VEX/XOP prefix
   , Vex (..)
   , vexW
   , vexR
   , vexX
   , vexB
   , vexL
   , vexVVVV
   , vexMMMMM
   , vexMapSelect
   , vexPP
   , vexPrefix
   -- * ModRM/SIB
   , ModRM(..)
   , SIB(..)
   , Scale(..)
   , RMMode(..)
   , Mode(..)
   , newModRM
   , rmField
   , regField
   , modField
   , modeField
   , modRMFields
   , rmMode
   , useDisplacement
   , useSIB
   , scaleField
   , indexField
   , baseField
   , rmRegMode
   )
where

import Haskus.Utils.Maybe
import Haskus.Utils.List (nub)
import Haskus.Utils.Solver
import Haskus.Binary.Bits
import Haskus.Number.Word
import Haskus.Binary.BitField hiding (Size)
import Haskus.Arch.X86_64.ISA.MicroArch
import Haskus.Arch.X86_64.ISA.Context
import Haskus.Arch.X86_64.ISA.Size
import Haskus.Arch.X86_64.ISA.Solver
import Haskus.Arch.X86_64.ISA.Register
import Haskus.Arch.X86_64.ISA.Memory
import Haskus.Arch.X86_64.ISA.Operand
import Haskus.Arch.X86_64.ISA.Extension
import Haskus.Arch.Common.Register
import Haskus.Utils.Flow

import Haskus.Arch.X86_64.ISA.Encoding.Prefix
import Haskus.Arch.X86_64.ISA.Encoding.Rex

import Haskus.Utils.List ((\\))
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Applicative

-- | Instruction encoding
data Encoding = Encoding
   { encOpcodeEncoding  :: !OpcodeEncoding       -- ^ Opcode encoding
   , encMandatoryPrefix :: !(Maybe Prefix)       -- ^ Mandatory prefix
   , encOpcodeMap       :: !OpcodeMap            -- ^ Map
   , encOpcode          :: {-# UNPACK #-} !Word8 -- ^ Opcode
   , encOpcodeExt       :: !(Maybe Word8)        -- ^ Opcode extension in ModRM.reg
   , encOpcodeFullExt   :: !(Maybe Word8)        -- ^ Opcode extension in full ModRM byte
   , encOpcodeWExt      :: !(Maybe Bool)         -- ^ Opcode extension in REX.W, VEX.W, etc.
   , encOpcodeLExt      :: !(Maybe Bool)         -- ^ Opcode extension in VEX.L, etc.
   , encReversableBit   :: !(Maybe Word)         -- ^ Args are reversed if the given bit is
                                                 --   set in the opcode.
   , encNoForce8Bit     :: !(Maybe Word)         -- ^ Operand size is 8 if the given bit is
                                                 --   unset in the opcode. Otherwise, the
                                                 --   size is defined by operand-size
                                                 --   prefix and REX.W bit
   , encSignExtendImmBit:: !(Maybe Word)         -- ^ Used in conjunction with a set
                                                 --   NoForce8Bit bit. Imm8 operand is used
                                                 --   and sign-extended if the given bit is
                                                 --   set
   , encFPUDestBit      :: !(Maybe Word)         -- ^ Opcode bit: register destination (0 if ST0, 1 if ST(i))
                                                 --   only if both operands are registers!
   , encFPUPopBit       :: !(Maybe Word)         -- ^ Opcode bit: pop the FPU register,
                                                 --   only if destination is (ST(i))
   , encFPUSizableBit   :: !(Maybe Word)         -- ^ Opcode bit: change the FPU size (only if memory operand)
   , encProperties      :: ![EncodingProperties] -- ^ Encoding properties
   , encOperands        :: ![OperandSpecP]       -- ^ Operand encoding
   }
   deriving (Show)

-- | Some bits in the opcode may be meaningful.
data OpcodeBit
   = OcBitNoForce8Bit   -- ^ If unset, 8-bit operand size, otherwise any other
   | OcBitImmSignExtend -- ^ If set (and OcBitNoForce8Bit too), use sign-extended imm8 operand
   | OcBitReversable    -- ^ Args are reversed if the given bit is set
   | OcBitFPUDest       -- ^ FPU register destination: 0 if ST0, 1 if ST(i)
   | OcBitFPUPop        -- ^ Pop the FPU register (only if OcBitFPUDest is set)
   | OcBitFPUSizable    -- ^ Change the FPU operand size (only if memory operand)
   deriving (Show,Eq)


-- | Opcode encoding
data OpcodeEncoding
   = EncLegacy -- ^ Legacy encoding
   | EncVEX    -- ^ VEX encoding
   deriving (Show,Eq,Ord)

-- | Encoding properties
data EncodingProperties
   = LongModeSupport          -- ^ Supported in 64 bit mode
   | LegacyModeSupport        -- ^ Supported in legacy/compatibility mode
   | Lockable                 -- ^ Support LOCK prefix (only if a memory operand
                              --   is used)
   | ImplicitLock             -- ^ Implicitly locked (lock prefix still supported)
   | BranchHintable           -- ^ Support branch-hint prefixes
   | Repeatable               -- ^ Allow repeat prefix
   | Commutable               -- ^ Operands can be commuted
   | DefaultOperandSize64     -- ^ Default operand size is 64-bits for this
                              --   instruction in LongMode
   | NoOperandSize64          -- ^ 64-bit operand size not supported
   | Extension Extension      -- ^ Required CPU extension
   | Arch X86Arch             -- ^ Instruction added starting at the given arch
   | DefaultSegment X86Reg    -- ^ Default register
   | HLE HLEAction            -- ^ Hardware-lock elision (HLE) prefix support
   deriving (Show,Eq)

-- | Hardware-lock elision prefixes
data HLEAction
   = XAcquire
   | XRelease
   | XBoth
   deriving (Show,Eq)

hasImmediate :: Encoding -> Bool
hasImmediate e = any (isImmediate . opStore) (encOperands e)

isLegacyEncoding :: Encoding -> Bool
isLegacyEncoding = (== EncLegacy) . encOpcodeEncoding

isVexEncoding :: Encoding -> Bool
isVexEncoding = (== EncVEX) . encOpcodeEncoding

-- | Indicate if LOCK prefix is allowed
encLockable :: Encoding -> Bool
encLockable e = Lockable     `elem` encProperties e
             || ImplicitLock `elem` encProperties e

-- | Indicate if branch hint prefixes are allowed
encBranchHintable :: Encoding -> Bool
encBranchHintable e = BranchHintable `elem` encProperties e

-- | Indicate if REPEAT prefix is allowed
encRepeatable :: Encoding -> Bool
encRepeatable e = Repeatable `elem` encProperties e

encRequireModRM :: Encoding -> Bool
encRequireModRM e = hasOpExt || hasOps
   where
      -- use opcode extension in ModRM.reg or in full ModRM
      hasOpExt = isJust (encOpcodeExt e) || isJust (encOpcodeFullExt e)

      -- has operands in ModRM
      hasOps   = any matchEnc (encOperands e)
      matchEnc x = case opStore x of
         S_RM         -> True
         S_Reg        -> True
         S_Imm        -> False
         S_Imm8h      -> False
         S_Imm8l      -> False
         S_Implicit   -> False
         S_Vvvv       -> False
         S_OpcodeLow3 -> False

-- | Valid ModRM.mod values
data ValidMod
   = ModeOnlyReg  -- ^ Only register
   | ModeOnlyMem  -- ^ Only memory
   | ModeBoth     -- ^ Register and memory
   | ModeNone     -- ^ None
   deriving (Show,Eq,Ord)

-- | ModRM.mod only supports the given value
encValidModRMMode :: Encoding -> ValidMod
encValidModRMMode e = case ots of
      []  -> ModeNone
      [x] -> foldl comb ModeNone (Set.map toM (getTerminals x))
      _   -> error ("encValidModRMMode: more than one ModRM.rm param: " ++ show ots)
   where
      ots = opFam <$> filter ((== S_RM) . opStore) (encOperands e)

      comb ModeBoth    _           = ModeBoth
      comb _           ModeBoth    = ModeBoth
      comb ModeNone    c           = c
      comb c           ModeNone    = c
      comb ModeOnlyReg ModeOnlyReg = ModeOnlyReg
      comb ModeOnlyMem ModeOnlyMem = ModeOnlyMem
      comb ModeOnlyReg ModeOnlyMem = ModeBoth
      comb ModeOnlyMem ModeOnlyReg = ModeBoth

      toM = \case
         T_Mem _ -> ModeOnlyMem
         T_Reg _ -> ModeOnlyReg
         x       -> error ("encValidModRMMode: invalid param type: " ++ show x)

-- | Indicate if a memory operand may be encoded
encMayHaveMemoryOperand :: Encoding -> Bool
encMayHaveMemoryOperand e = memRm || memOp
   where
      memRm = case encValidModRMMode e of
         ModeNone    -> False
         ModeOnlyReg -> False
         ModeOnlyMem -> True
         ModeBoth    -> True
      memOp = encOperands e
         ||> (\(OperandSpec _ x _) -> x)
         |> fmap getTerminals
         |> Set.unions
         |> Set.toList
         |> any isTMem
      isTMem (T_Mem _) = True
      isTMem _         = False

-- | Return predicates of the encoding
encPredicates :: Encoding -> Set X86Pred
encPredicates enc = encOperands enc
   |> fmap getPredicates
   |> Set.unions


-- | Indicate if prefix 66 (override operand-size) can be used
encAllowPrefix66 :: Encoding -> Bool
encAllowPrefix66 e =
   encMandatoryPrefix e == Just P_66
   || PrefixPred Prefix66 `elem` encPredicates e

-- | Test if an encoding support the given Hardware-Lock Elision prefix
encSupportHLE :: HLEAction -> Encoding -> Bool
encSupportHLE a e = case filter isHLE (encProperties e) of
      []       -> False
      [HLE a'] -> a' == XBoth || a == a'
      xs       -> error ("Invalid HLE actions: "++show xs)
   where
      isHLE (HLE _) = True
      isHLE _       = False

-- | Test if an encoding is supported in a given mode
encSupportMode :: Mode -> Encoding -> Bool
encSupportMode mode enc = p `elem` encProperties enc
  where p = case mode of
              Mode64    -> LongModeSupport
              _         -> LegacyModeSupport

-- | Get required extensions for the encoding
encRequiredExtensions :: Encoding -> [Extension]
encRequiredExtensions enc =
   mapMaybe extractExt (encProperties enc)
   where
      extractExt (Extension e) = Just e
      extractExt _             = Nothing

-- | Indicate if an encoding is supported given a set of extensions.
-- For now, check if the required extensions are enabled.
encSupportExtensions :: ExtensionSet -> Encoding -> Bool
encSupportExtensions exts enc =
   -- FIXME: don't use a list, use (Bit)Set ops
   null (encRequiredExtensions enc \\ exts)

-- | Test if an encoding is supported in a given execution context
ctxSupportEncoding :: Context -> Encoding -> Bool
ctxSupportEncoding ctx enc =
   encSupportMode (ctxMode ctx) enc
   && encSupportExtensions (ctxExtensions ctx) enc


-- | Some instructions store flags and values into the opcode byte. This method
-- returns the list of potential opcodes for an encoding
encGenerateOpcodes :: Encoding -> [Word8]
encGenerateOpcodes e = nub ocs
   where
      -- the original opcode
      oc = encOpcode e

      -- reversed (check: can we have reversed + operand in opcode (or something
      -- else)?)
      (roc,rsoc) = case (encReversableBit e, encNoForce8Bit e) of
               (Just i, Nothing) -> (Just (setBit oc i), Nothing)
               (Just i, Just i2) -> (Just (setBit oc i), Just (setBit (setBit oc i2) i))
               _                 -> (Nothing,Nothing)
      -- sizable, sign-extended
      (szoc,seoc) = case (encNoForce8Bit e, encSignExtendImmBit e) of
               (Nothing,Nothing) -> (Nothing,Nothing)
               (Just i, Nothing) -> (Just (setBit oc i),Nothing)
               (Just i, Just i2) -> (Just (setBit oc i), Just (setBit (setBit oc i2) i))
               (Nothing, Just i) ->  (Nothing,Just (setBit oc i))

      -- FPU flags
      fps = [encFPUDestBit e, encFPUSizableBit e, encFPUPopBit e, Nothing]
      mf (Just x , Just y ) = setBit (setBit oc y) x
      mf (Nothing, Just y ) = setBit oc y
      mf (Just x , Nothing) = setBit oc x
      mf (Nothing, Nothing) = oc
      fs = [ mf (x,y) | x <- fps, y <- fps]

      -- opcodes with different flags
      ocs' = oc : (fs ++ catMaybes [roc,rsoc,szoc,seoc])

      -- operand stored in the opcode
      ocs = if S_OpcodeLow3 `elem` fmap opStore (encOperands e)
               then [o + i | o <- ocs', i <- [0..7]]
               else ocs'

-- | Test if an encoding support a given prefix
encSupportPrefix :: Encoding -> Prefix -> Bool
encSupportPrefix e x =
   Just x == encMandatoryPrefix e || case x of
      -- operand-size prefix
      P_66 -> encAllowPrefix66 e
      -- address-size prefix
      P_67 -> encMayHaveMemoryOperand e
      -- CS segment override / Branch not taken hint
      P_2E -> encMayHaveMemoryOperand e || encBranchHintable e
      -- DS segment override / Branch taken hint
      P_3E -> encMayHaveMemoryOperand e || encBranchHintable e
      -- ES segment override
      P_26 -> encMayHaveMemoryOperand e
      -- FS segment override
      P_64 -> encMayHaveMemoryOperand e
      -- GS segment override
      P_65 -> encMayHaveMemoryOperand e
      -- SS segment override
      P_36 -> encMayHaveMemoryOperand e
      -- LOCK prefix
      P_F0 -> encLockable e
      -- REPZ / XRELEASE
      P_F3 -> encRepeatable e || encSupportHLE XRelease e
      -- REPNZ / XACQUIRE
      P_F2 -> encRepeatable e || encSupportHLE XAcquire e

-------------------------------------------------------------------
-- Generic opcode
-------------------------------------------------------------------

-- | Generic opcode
data Opcode
   = OpLegacy (Maybe Rex) LegacyMap !Word8
   | OpVex    Vex  !Word8
   | OpXop    Vex  !Word8
   deriving (Show,Eq)

-- | Opcode map
data OpcodeMap
   = MapLegacy LegacyMap
   | MapVex    !Word8
   | MapXop    !Word8
   deriving (Show,Eq,Ord)

-- | Legacy opcode map
data LegacyMap
   = MapPrimary
   | Map0F
   | Map0F38
   | Map0F3A
   | Map3DNow
   deriving (Show,Eq,Ord)


-- | Opcode byte
opcodeByte :: Opcode -> Word8
opcodeByte (OpLegacy _ _ x) = x
opcodeByte (OpVex _ x)      = x
opcodeByte (OpXop _ x)      = x

-- | Get the opcode map
opcodeMap :: Opcode -> OpcodeMap
opcodeMap = \case
   OpLegacy _ t _ -> MapLegacy t
   OpVex  v     _ -> vexMapSelect v
   OpXop  v     _ -> vexMapSelect v

-- | Base extension
opcodeB :: Opcode -> Word8
opcodeB = \case
   OpVex v _               -> if vexB v then 1 else 0
   OpXop v _               -> if vexB v then 1 else 0
   OpLegacy (Just rex) _ _ -> if testRexB rex then 1 else 0
   OpLegacy Nothing    _ _ -> 0

-- | Reg extension
opcodeR :: Opcode -> Word8
opcodeR = \case
   OpVex v _               -> if vexR v then 1 else 0
   OpXop v _               -> if vexR v then 1 else 0
   OpLegacy (Just rex) _ _ -> if testRexR rex then 1 else 0
   OpLegacy Nothing    _ _ -> 0

-- | Index extension
opcodeX :: Opcode -> Word8
opcodeX = \case
   OpVex v _               -> if vexX v then 1 else 0
   OpXop v _               -> if vexX v then 1 else 0
   OpLegacy (Just rex) _ _ -> if testRexX rex then 1 else 0
   OpLegacy Nothing    _ _ -> 0

-- | W (64-bit operand size)
opcodeW :: Opcode -> Bool
opcodeW = \case
   OpVex v _               -> vexW v
   OpXop v _               -> vexW v
   OpLegacy (Just rex) _ _ -> testRexW rex
   OpLegacy Nothing    _ _ -> False

-- | Get vector length (stored in VEX.L, XOP.L, etc.)
opcodeL :: Opcode -> Maybe Bool
opcodeL = \case
   OpVex v _ -> Just $ vexL v
   OpXop v _ -> Just $ vexL v
   _         -> Nothing


-------------------------------------------------------------------
-- VEX prefix
-------------------------------------------------------------------

-- | A VEX prefix
data Vex
   = Vex2 !Word8           -- ^ Two-byte VEX prefix
   | Vex3 !Word8 !Word8    -- ^ Three-byte VEX prefix
   deriving (Show,Eq)

vexW :: Vex -> Bool
vexW (Vex2 _)   = False
vexW (Vex3 _ x) = testBit x 7

vexR :: Vex -> Bool
vexR (Vex2 x)   = not $ testBit x 7
vexR (Vex3 x _) = not $ testBit x 7

vexX :: Vex -> Bool
vexX (Vex2 _)   = False
vexX (Vex3 x _) = not $ testBit x 6

vexB :: Vex -> Bool
vexB (Vex2 _)   = False
vexB (Vex3 x _) = not $ testBit x 5

vexL :: Vex -> Bool
vexL (Vex2 x)   = testBit x 2
vexL (Vex3 _ x) = testBit x 2

vexVVVV :: Vex -> Word8
vexVVVV (Vex2 x)   = complement (x `shiftR` 3) .&. 0x0F
vexVVVV (Vex3 _ x) = complement (x `shiftR` 3) .&. 0x0F

vexPP :: Vex -> Word8
vexPP (Vex2 x)   = x .&. 0x03
vexPP (Vex3 _ x) = x .&. 0x03

vexPrefix :: Vex -> Maybe Prefix
vexPrefix v = case vexPP v of
   0x00 -> Nothing
   0x01 -> Just P_66
   0x02 -> Just P_F3
   0x03 -> Just P_F2
   _    -> error "Invalid VEX.pp"

vexMMMMM :: Vex -> Word8
vexMMMMM (Vex2 _)   = 0x01
vexMMMMM (Vex3 x _) = x .&. 0x1F

vexMapSelect :: Vex -> OpcodeMap
vexMapSelect = MapVex . vexMMMMM

-------------------------------------------------------------------
-- ModRM/SIB
-------------------------------------------------------------------

-- | ModRM byte
newtype ModRM = ModRM (BitFields Word8
  '[ BitField 2 "mode" Word8
   , BitField 3 "reg"  Word8
   , BitField 3 "rm"   Word8
   ])
   deriving (Show,Eq)

-- | SIB byte
newtype SIB = SIB Word8 deriving (Show,Eq)

-- | Mode for the R/M field
data RMMode
   = RMRegister   -- ^ Direct register addressing
   | RMBaseIndex  -- ^ Memory addressing with only base/index register
   | RMSIB        -- ^ Memory addressing with SIB byte
   deriving (Show, Eq)

-- | Create a ModRM byte (check inputs)
newModRM :: Word8 -> Word8 -> Word8 -> ModRM
newModRM md rm reg
   | md  > 3 = error "Invalid value for mod field (> 3)"
   | rm  > 7 = error "Invalid value for rm field (> 7)"
   | reg > 7 = error "Invalid value for reg field (> 7)"
   | otherwise = ModRM
         $ updateField @"mode" md
         $ updateField @"reg"  reg
         $ updateField @"rm"   rm
         $ BitFields 0


-- | Get r/m field in ModRM
rmField :: ModRM -> Word8
rmField (ModRM x) = extractField @"rm" x

-- | Get reg field in ModRM
regField :: ModRM -> Word8
regField (ModRM x) = extractField @"reg" x

-- | Get mod field in ModRM
modField :: ModRM -> Word8
modField (ModRM x) = extractField @"mode" x


-- | Mode for pattern matching
data ModField
   = Mode00
   | Mode01
   | Mode10
   | Mode11
   deriving (Show,Eq,Enum)

-- | Get mod field in ModRM
modeField :: ModRM -> ModField
modeField = toEnum . fromIntegral . modField

-- | Get the tree fields (mod,reg,rm)
modRMFields :: ModRM -> (Word8,Word8,Word8)
modRMFields (ModRM x) = matchFields x

-- | Indicate R/M field mode
rmMode :: AddressSize -> ModRM -> RMMode
rmMode sz rm = case (sz, modField rm, rmField rm) of
   (_,3,_)          -> RMRegister
   (AddrSize16,_,_) -> RMBaseIndex
   (_,_,4)          -> RMSIB
   _                -> RMBaseIndex

-- | Indicate if the r/m field contains a register
rmRegMode :: ModRM -> Bool
rmRegMode rm = modField rm == 3

-- | Indicate if displacement bytes follow
useDisplacement :: AddressSize -> Maybe SIB -> ModRM -> Maybe Size
useDisplacement sz sib modrm = case (sz,modField modrm,rmField modrm) of
   -- 16-bit addressing
   (AddrSize16, 0, 0b110) -> Just Size16
   (AddrSize16, 1,     _) -> Just Size8
   (AddrSize16, 2,     _) -> Just Size16
   (AddrSize16, _,     _) -> Nothing

   -- 64 bit uses 32 bit addressing
   (_, 0, 0b101)          -> Just Size32
   (_, 1,     _)          -> Just Size8
   (_, 2,     _)          -> Just Size32
   (_, 0, 0b100)          -> case sib of
      Nothing -> error "SIB required"
      Just s  -> if baseField s == 0b101
         then Just Size32
         else Nothing

   _                      -> Nothing

-- | Indicate if a SIB byte follows
useSIB :: AddressSize -> ModRM -> Bool
useSIB sz modrm = case (sz,modField modrm,rmField modrm) of
   (AddrSize16, _, _) -> False -- no SIB in 16 bit addressing
   (_, 3, _)          -> False -- direct register addressing
   (_, _, 4)          -> True
   _                  -> False


-- | Get SIB scale field
scaleField :: SIB -> Scale
scaleField (SIB x) = case x `shiftR` 6 of
   0 -> Scale1
   1 -> Scale2
   2 -> Scale4
   3 -> Scale8
   _ -> error "Invalid scaling factor"

-- | Get SIB index field
indexField :: SIB -> Word8
indexField (SIB x) = (x `shiftR` 3) .&. 0x07

-- | Get SIB base field
baseField :: SIB -> Word8
baseField (SIB x) = x .&. 0x07

-- | Set a memory address from ModRM/SIB
--
-- TODO: replace this function with a predicated stuff
setAddrFam :: Bool -> [Prefix] -> Opcode -> AddressSize -> Bool -> ModRM -> Maybe SIB -> Maybe Size -> Maybe Word64 -> AddrFam -> AddrFam
setAddrFam is64bitMode' ps oc addressSize useExtRegs modrm msib mdispSize mdisp fam = fam
      { addrFamSeg      = FixedSeg <$> seg'
      , addrFamBase     = base
      , addrFamIndex    = regFamFromReg <$> idx
      , addrFamScale    = scl
      , addrFamDisp     = mdisp
      , addrFamDispSize = mdispSize
      }
   where
      -- segment override prefixes
      segOverride = case filter ((== 3) . prefixGroup) ps of
         []     -> Nothing
         [P_2E] -> Just R_CS
         [P_3E] -> Just R_DS
         [P_26] -> Just R_ES
         [P_64] -> Just R_FS
         [P_65] -> Just R_GS
         [P_36] -> Just R_SS
         xs -> error ("More than one segment-override prefix: "++show xs)

      sib' = fromJust msib
            
      -- | Extended ModRM.rm (with REX.B, VEX.B, etc.)
      modRMrm = opcodeB oc `uncheckedShiftL` 3 .|. rmField modrm

      -- | Extended SIB index (with REX.X, VEX.X, etc.)
      sibIdx = opcodeX oc `uncheckedShiftL` 3 .|. indexField sib'
            
      -- | Extended SIB base (with REX.B, VEX.B, etc.)
      sibBase = opcodeB oc `uncheckedShiftL` 3 .|. baseField sib'

      gpr sz r  = regGPR useExtRegs sz r


      toR = case addressSize of
               AddrSize32 -> gpr 32
               AddrSize64 -> gpr 64
               AddrSize16 -> error "Trying to use AddrSize16"
      base = if addressSize == AddrSize16
               then case (modField modrm, rmField modrm) of
                  (_,    0b000) -> Just R_BX
                  (_,    0b001) -> Just R_BX
                  (_,    0b010) -> Just R_BP
                  (_,    0b011) -> Just R_BP
                  (_,    0b100) -> Nothing    
                  (_,    0b101) -> Nothing    
                  (0b00, 0b110) -> Nothing    
                  (_,    0b110) -> Just R_BP
                  (_,    0b111) -> Just R_BX
                  _             -> error "Invalid 16-bit addressing"
               else case (modField modrm, rmField modrm) of
                  (0b00, 0b101) -> if is64bitMode'
                                       then Just R_RIP
                                       else Nothing
                  -- SIB: if mod is 0b00, don't use EBP as base.
                  (0b00, 0b100)
                     | baseField sib' == 0b101 -> Nothing
                  (_,    0b100) -> Just (toR (fromIntegral sibBase))
                  _             -> Just (toR (fromIntegral modRMrm))
      idx = if addressSize == AddrSize16
               then case rmField modrm of
                  0b000 -> Just R_SI
                  0b001 -> Just R_DI
                  0b010 -> Just R_SI
                  0b011 -> Just R_DI
                  0b100 -> Just R_SI
                  0b101 -> Just R_DI
                  0b110 -> Nothing
                  0b111 -> Nothing
                  _     -> error "Invalid 16-bit addressing"
            else case (rmField modrm, indexField sib') of
               -- SIB: if index is 0b100 (should be ESP), don't
               -- use any index
               (0b100, 0b100) -> Nothing
               (0b100, _    ) -> Just (toR (fromIntegral sibIdx))
               _              -> Nothing -- no SIB
      scl = if addressSize /= AddrSize16 && rmField modrm == 0b100
               then Just (scaleField sib')
               else Nothing

      seg' = segOverride <|> (baseDefaultSegment <$> base)

