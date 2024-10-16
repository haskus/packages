module Haskus.Arch.X86_64.ISA.Encoding.Enc
  ( Enc(..)
  , emptyEnc
  , encSize
  , encImmOffset
  , encDispOffset
  , Opcode (..)
  , LegacyMap (..)
  , EncError (..)
  , encCheck
  , encode
  , encodeToArray#
  )
where

import Haskus.Binary.Word
import Haskus.Memory.Writer
import Haskus.Memory.Writer.SizedWriter (SizedWriter(..), fromSizedWriter)
import Haskus.Arch.X86_64.ISA.Encoding.Prefix
import Haskus.Arch.X86_64.ISA.Encoding.Rex
import Haskus.Arch.X86_64.ISA.Encoding.ModRM
import Haskus.Arch.X86_64.ISA.Encoding.Disp
import Haskus.Arch.X86_64.ISA.Encoding.SIB
import Haskus.Arch.X86_64.ISA.Encoding.Vex
import Haskus.Arch.X86_64.ISA.Encoding.Xop
import Haskus.Arch.X86_64.ISA.Size

import Data.Maybe
import GHC.Exts

-- | Instruction encoding specification
data Enc = Enc
  { encPrefixes :: [Prefix]            -- ^ Prefixes (up to 5)
  , encRex      :: !(Maybe Rex)        -- ^ Rex prefix
  , encOpcode   :: !(Maybe Opcode)     -- ^ Opcode (optional to allow naked prefixes)
  , encModRM    :: !(Maybe ModRM)      -- ^ ModRM
  , encSIB      :: !(Maybe SIB)        -- ^ SIB
  , encDisp     :: !Disp               -- ^ Displacement
  , encImm      :: !(Maybe SizedValue) -- ^ Immediate
  }
  deriving (Show)

emptyEnc :: Enc
emptyEnc = Enc [] Nothing Nothing Nothing Nothing NoDisp Nothing

-- | Encoding size in bytes
encSize :: Enc -> Word
encSize Enc{..} = full_size
  where
    sz_maybe_u8 = maybe 0 (const 1)
    sz_opcode   = maybe 0 opcodeSize
    sz_sized    = maybe 0 sizedValueSizeInBytes
    full_size   = fromIntegral (length encPrefixes)
                  + sz_maybe_u8 encRex
                  + sz_opcode encOpcode
                  + sz_maybe_u8 encModRM
                  + sz_maybe_u8 encSIB
                  + dispSizeInBytes encDisp
                  + sz_sized encImm

-- | Offset of the immediate (if any)
encImmOffset :: Enc -> Maybe Word
encImmOffset Enc{..} = maybe Nothing (const (Just imm_offset)) encImm
  where
    sz_maybe_u8 = maybe 0 (const 1)
    sz_opcode   = maybe 0 opcodeSize
    imm_offset  = fromIntegral (length encPrefixes)
                  + sz_maybe_u8 encRex
                  + sz_opcode encOpcode
                  + sz_maybe_u8 encModRM
                  + sz_maybe_u8 encSIB
                  + dispSizeInBytes encDisp

-- | Offset of the displacement (if any)
encDispOffset :: Enc -> Maybe Word
encDispOffset Enc{..} = case encDisp of
    NoDisp -> Nothing
    _      -> Just disp_offset
  where
    sz_maybe_u8 = maybe 0 (const 1)
    sz_opcode   = maybe 0 opcodeSize
    disp_offset = fromIntegral (length encPrefixes)
                  + sz_maybe_u8 encRex
                    -- 3DNow! opcode is in the immediate position, after the
                    -- displacement
                  + (if maybe False is3DNowOpcode encOpcode then 0 else sz_opcode encOpcode)
                  + sz_maybe_u8 encModRM
                  + sz_maybe_u8 encSIB


-- | Instruction opcode
data Opcode
  = Op_Leg  !LegacyMap !U8 -- ^ Legacy encoding
  | Op_Vex  !Vex       !U8  -- ^ Vex prefix: 2-byte or 3-byte prefix + opcode
  | Op_Xop  !Xop       !U8  -- ^ Xop prefix: 3-byte prefix + opcode
  deriving (Show,Eq,Ord)

data LegacyMap
  = Map_Primary -- ^ Primary opcode map
  | Map_0F      -- ^ Secondary opcode map
  | Map_0F38    -- ^ 0F_38 opcode map
  | Map_0F3A    -- ^ 0F_3A opcode map
  | Map_0F0F    -- ^ 3DNow opcode map
  deriving (Show,Eq,Ord)

writeLegacyMap :: LegacyMap -> Writer s
writeLegacyMap = \case
  Map_Primary  -> mempty
  Map_0F       -> writeU8 0x0F
  Map_0F38     -> writeU8 0x0F <> writeU8 0x38
  Map_0F3A     -> writeU8 0x0F <> writeU8 0x3A
  Map_0F0F     -> writeU8 0x0F <> writeU8 0x0F

opcodeSize :: Opcode -> Word
opcodeSize = \case
  Op_Leg m _ -> case m of
    Map_Primary -> 1
    Map_0F      -> 2
    Map_0F38    -> 3
    Map_0F3A    -> 3
    Map_0F0F    -> 3
  Op_Vex v _ -> vexSize v + 1
  Op_Xop  {} -> 4

isLegacyOpcode :: Maybe Opcode -> Bool
isLegacyOpcode = \case
  Nothing           -> True
  Just (Op_Leg {})  -> True
  Just (Op_Vex  {}) -> False
  Just (Op_Xop  {}) -> False

is3DNowOpcode :: Opcode -> Bool
is3DNowOpcode = \case
  Op_Leg Map_0F0F _ -> True
  _                 -> False

data EncError
  = TooManyPrefixes !Word       -- ^ More than 5 prefixes
  | RexNotAllowed               -- ^ Rex prefix not allowed with the given opcode encoding
  | PrefixNotAllowed [Prefix]   -- ^ Prefix not allowed with the given opcode encoding
  | Imm64NotAllowed             -- ^ 64-bit immediate not allowed with displacement
  | TooManyBytes !Word          -- ^ More than 15-byte long encoding
  | ImmNotAllowedWith3DNow      -- ^ Immediate operand not allowed with 3DNow! instructions
  deriving (Show,Eq,Ord)

-- | Check the validity of an encoding
encCheck :: Enc -> [EncError]
encCheck e@Enc{..} = concat
  [ 
    -- at most 5 prefixes
    let n = fromIntegral (length encPrefixes) in wh (n > 5) (TooManyPrefixes n)

    -- REX only allowed with legacy opcode encoding
  , wh (isJust encRex && not (isLegacyOpcode encOpcode)) RexNotAllowed

    -- Non-legacy encodings only support prefixes from groups 2 and 3
  , let g2o3 p = case prefixGroup p of
          2 -> True
          3 -> True
          _ -> False
        ps = filter (not . g2o3) encPrefixes
    in wh (not (null ps || isLegacyOpcode encOpcode)) (PrefixNotAllowed ps)

    -- Instructions with 64-bit immediate have no displacement, and vice versa
  , let is_size64 = \case
          Just (SizedValue64 {}) -> True
          _                      -> False
    in wh (is_size64 encImm && hasDisp encDisp) Imm64NotAllowed

    -- Instruction size <= 15 bytes
  , let full_size = encSize e
    in wh (full_size > 15) (TooManyBytes full_size)

    -- Immediate operand not allowed with 3DNow! instructions
  , wh (maybe False is3DNowOpcode encOpcode && isJust encImm) ImmNotAllowedWith3DNow
  ]
  where
    wh c err = if c then [err] else []

-- | Encode an instruction.
--
-- Assumres that 15 bytes of memory are available (maximum instruction size).
encode :: Enc -> Writer s
encode Enc{..} = mconcat
  [ mconcat (map (writeU8 . fromPrefix) encPrefixes) -- prefixes
  , maybe mempty (writeU8 . rexU8)      encRex       -- REX
  , case encOpcode of
      Nothing -> mempty
      Just o  -> case o of
        Op_Leg m oc -> case m of
          Map_0F0F -> writeLegacyMap m -- 3DNow! opcode goes after the operands
          _        -> writeLegacyMap m <> writeU8 oc
        Op_Vex v oc -> writeVex v <> writeU8 oc
        Op_Xop x oc -> writeXop x <> writeU8 oc
  
  , maybe mempty writeModRM encModRM
  , maybe mempty writeSIB   encSIB
  , writeDisp encDisp
  , maybe mempty writeSizedValueLE encImm
    
  , case encOpcode of
      -- 3DNow! opcode goes after the operands
      Just (Op_Leg Map_0F0F oc) -> writeU8 oc
      _                         -> mempty
  ]

-- | Allocate a ByteArray and encode the instruction into it.
--
-- Don't use it if you have many instructions to encode: use a bigger array or
-- reuse one instead of allocating a fresh one for every instruction.
encodeToArray# :: Enc -> ByteArray#
encodeToArray# enc =
  let !(W# sz) = encSize enc
      !sw      = SizedWriter sz (encode enc)
  in runRW# \s0 -> case fromSizedWriter sw s0 of
      (# _, b #) -> b
