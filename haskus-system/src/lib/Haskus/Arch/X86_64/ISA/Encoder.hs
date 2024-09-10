module Haskus.Arch.X86_64.ISA.Encoder
  ( Enc(..)
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
import Haskus.Arch.X86_64.ISA.Size

import Data.Maybe

-- | Instruction encoding specification
data Enc = Enc
  { encPrefixes :: [Prefix]            -- ^ Prefixes (up to 5)
  , encRex      :: !(Maybe Rex)        -- ^ Rex prefix
  , encOpcode   :: !Opcode             -- ^ Opcode
  , encModRM    :: !(Maybe U8)         -- ^ ModRM
  , encSIB      :: !(Maybe U8)         -- ^ SIB
  , encDisp     :: !(Maybe SizedValue) -- ^ Displacement
  , encImm      :: !(Maybe SizedValue) -- ^ Immediate
  }

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

isLegacyOpcode :: Opcode -> Bool
isLegacyOpcode = \case
  Op      {} -> True
  Op_0F   {} -> True
  Op_0F38 {} -> True
  Op_0F3A {} -> True
  Op_0F0F {} -> True
  Op_Vex2 {} -> False
  Op_Vex3 {} -> False
  Op_Xop  {} -> False

data EncError
  = TooManyPrefixes !Int        -- ^ More than 5 prefixes
  | RexNotAllowed               -- ^ Rex prefix not allowed with the given opcode encoding
  | PrefixNotAllowed [Prefix]   -- ^ Prefix not allowed with the given opcode encoding
  | Imm64NotAllowed             -- ^ 64-bit immediate not allowed with displacement
  | Disp64NotAllowed            -- ^ 64-bit displacement not allowed with immediate
  deriving (Show,Eq,Ord)

-- | Check the validity of an encoding
check :: Enc -> [EncError]
check Enc{..} = concat
  [ 
    -- at most 5 prefixes
    let n = length encPrefixes in wh (n > 5) (TooManyPrefixes n)

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
  ]
  where
    wh c e = if c then [e] else []

-- | Encode an instruction.
--
-- Assumres that 15 bytes of memory are available (maximum instruction size).
encode :: Enc -> Writer s
encode Enc{..} = mconcat
  [ mconcat (map (writeU8 . fromPrefix) encPrefixes) -- prefixes
  , maybe mempty (writeU8 . rexU8)      encRex       -- REX
  , case encOpcode of
      Op      oc       -> writeU8 oc
      Op_0F   oc       -> writeU8 0x0F <> writeU8 oc
      Op_0F38 oc       -> writeU8 0x0F <> writeU8 0x38 <> writeU8 oc
      Op_0F3A oc       -> writeU8 0x0F <> writeU8 0x3A <> writeU8 oc
      Op_0F0F _oc      -> writeU8 0x0F <> writeU8 0x0F -- 3DNow! opcode goes after the operands
      Op_Vex2 v1 oc    -> writeU8 0xC5 <> writeU8 v1   <> writeU8 oc
      Op_Vex3 v1 v2 oc -> writeU8 0xC4 <> writeU8 v1   <> writeU8 v2 <> writeU8 oc
      Op_Xop  v1 v2 oc -> writeU8 0x8F <> writeU8 v1   <> writeU8 v2 <> writeU8 oc
  
  , maybe mempty writeU8 encModRM
  , maybe mempty writeU8 encSIB
  , maybe mempty writeSizedValueLE encDisp
  , maybe mempty writeSizedValueLE encImm
    
  , case encOpcode of
      Op_0F0F oc       -> writeU8 oc -- 3DNow! opcode goes after the operands
      _                -> mempty
  ]
