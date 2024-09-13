module Haskus.Arch.X86_64.ISA.Encoding.Enc
  ( Enc(..)
  , emptyEnc
  , encSize
  , encImmOffset
  , encDispOffset
  , Opcode (..)
  , EncError (..)
  , check
  , encode
  )
where

import Haskus.Binary.Word
import Haskus.Memory.Writer
import Haskus.Arch.X86_64.ISA.Encoding.Prefix
import Haskus.Arch.X86_64.ISA.Encoding.Rex
import Haskus.Arch.X86_64.ISA.Encoding.ModRM
import Haskus.Arch.X86_64.ISA.Size

import Data.Maybe

-- | Instruction encoding specification
data Enc = Enc
  { encPrefixes :: [Prefix]            -- ^ Prefixes (up to 5)
  , encRex      :: !(Maybe Rex)        -- ^ Rex prefix
  , encOpcode   :: !(Maybe Opcode)     -- ^ Opcode (optional to allow naked prefixes)
  , encModRM    :: !(Maybe ModRM)      -- ^ ModRM
  , encSIB      :: !(Maybe U8)         -- ^ SIB
  , encDisp     :: !(Maybe SizedValue) -- ^ Displacement
  , encImm      :: !(Maybe SizedValue) -- ^ Immediate
  }

emptyEnc :: Enc
emptyEnc = Enc [] Nothing Nothing Nothing Nothing Nothing Nothing

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
                  + sz_sized encDisp
                  + sz_sized encImm

-- | Offset of the immediate (if any)
encImmOffset :: Enc -> Maybe Word
encImmOffset Enc{..} = maybe Nothing (const (Just imm_offset)) encImm
  where
    sz_maybe_u8 = maybe 0 (const 1)
    sz_opcode   = maybe 0 opcodeSize
    sz_sized    = maybe 0 sizedValueSizeInBytes
    imm_offset  = fromIntegral (length encPrefixes)
                  + sz_maybe_u8 encRex
                  + sz_opcode encOpcode
                  + sz_maybe_u8 encModRM
                  + sz_maybe_u8 encSIB
                  + sz_sized encDisp

-- | Offset of the displacement (if any)
encDispOffset :: Enc -> Maybe Word
encDispOffset Enc{..} = maybe Nothing (const (Just disp_offset)) encDisp
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
  = Op      !U8         -- ^ Primary opcode map
  | Op_0F   !U8         -- ^ Secondary opcode map
  | Op_0F38 !U8         -- ^ 0F_38 opcode map
  | Op_0F3A !U8         -- ^ 0F_3A opcode map
  | Op_0F0F !U8         -- ^ 3DNow! opcode map
  | Op_Vex2 !U8 !U8     -- ^ Vex 2-byte opcode
  | Op_Vex3 !U8 !U8 !U8 -- ^ Vex 3-byte opcode
  | Op_Xop  !U8 !U8 !U8 -- ^ Xop 3-byte opcode
  deriving (Show,Eq,Ord)

opcodeSize :: Opcode -> Word
opcodeSize = \case
  Op      {} -> 1
  Op_0F   {} -> 2
  Op_0F38 {} -> 3
  Op_0F3A {} -> 3
  Op_0F0F {} -> 3
  Op_Vex2 {} -> 2
  Op_Vex3 {} -> 3
  Op_Xop  {} -> 3

isLegacyOpcode :: Maybe Opcode -> Bool
isLegacyOpcode = \case
  Nothing           -> True
  Just (Op      {}) -> True
  Just (Op_0F   {}) -> True
  Just (Op_0F38 {}) -> True
  Just (Op_0F3A {}) -> True
  Just (Op_0F0F {}) -> True
  Just (Op_Vex2 {}) -> False
  Just (Op_Vex3 {}) -> False
  Just (Op_Xop  {}) -> False

is3DNowOpcode :: Opcode -> Bool
is3DNowOpcode = \case
  Op_0F0F {} -> True
  _          -> False

data EncError
  = TooManyPrefixes !Word       -- ^ More than 5 prefixes
  | RexNotAllowed               -- ^ Rex prefix not allowed with the given opcode encoding
  | PrefixNotAllowed [Prefix]   -- ^ Prefix not allowed with the given opcode encoding
  | Imm64NotAllowed             -- ^ 64-bit immediate not allowed with displacement
  | Disp64NotAllowed            -- ^ 64-bit displacement not allowed with immediate
  | TooManyBytes !Word          -- ^ More than 15-byte long encoding
  | ImmNotAllowedWith3DNow      -- ^ Immediate operand not allowed with 3DNow! instructions
  deriving (Show,Eq,Ord)

-- | Check the validity of an encoding
check :: Enc -> [EncError]
check e@Enc{..} = concat
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
    in wh (null ps || isLegacyOpcode encOpcode) (PrefixNotAllowed ps)

    -- Instructions with 64-bit immediate have no displacement, and vice versa
  , let is_size64 = \case
          Just (SizedValue64 {}) -> True
          _                      -> False
    in wh (is_size64 encImm && isJust encDisp) Imm64NotAllowed
       <> wh (is_size64 encDisp && isJust encImm) Disp64NotAllowed

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
      Nothing                 -> mempty
      Just (Op      oc      ) -> writeU8 oc
      Just (Op_0F   oc      ) -> writeU8 0x0F <> writeU8 oc
      Just (Op_0F38 oc      ) -> writeU8 0x0F <> writeU8 0x38 <> writeU8 oc
      Just (Op_0F3A oc      ) -> writeU8 0x0F <> writeU8 0x3A <> writeU8 oc
      Just (Op_0F0F _oc     ) -> writeU8 0x0F <> writeU8 0x0F -- 3DNow! opcode goes after the operands
      Just (Op_Vex2 v1 oc   ) -> writeU8 0xC5 <> writeU8 v1   <> writeU8 oc
      Just (Op_Vex3 v1 v2 oc) -> writeU8 0xC4 <> writeU8 v1   <> writeU8 v2 <> writeU8 oc
      Just (Op_Xop  v1 v2 oc) -> writeU8 0x8F <> writeU8 v1   <> writeU8 v2 <> writeU8 oc
  
  , maybe mempty (writeU8 . modrmU8) encModRM
  , maybe mempty writeU8 encSIB
  , maybe mempty writeSizedValueLE encDisp
  , maybe mempty writeSizedValueLE encImm
    
  , case encOpcode of
      Just (Op_0F0F oc) -> writeU8 oc -- 3DNow! opcode goes after the operands
      _                 -> mempty
  ]
