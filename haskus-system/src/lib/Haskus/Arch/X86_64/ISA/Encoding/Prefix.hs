-- | Instruction prefixes
module Haskus.Arch.X86_64.ISA.Encoding.Prefix
  ( Prefix (..)
  , toPrefix
  , fromPrefix
  , prefixGroup
  )
where

import Haskus.Number.Word

---------------------------------------------------------------------------
-- Note [Prefixes]
-- ~~~~~~~~~~~~~~~
-- 
-- An instruction optionally begins with up to five prefixes, in any
-- order. These prefixes can:
--    1) modify the instruction's default address size
--    2) modify the instruction's default operand size
--    3) modify the instruction's memory address segment
--    4) be used as an opcode extension
--    5) provide atomic bus locking or hardware-lock elision (HLE)
--    6) repeat the instruction until a condition is met
--
-- Note: the effective sizes of the operands may not be the same: the shorter
-- may be sign-extended or zero-extended.
--
-- Prefixes that are used as opcode extensions are mandatory.
--
--
-- Note [Prefix groups]
-- ~~~~~~~~~~~~~~~~~~~~
--
-- Prefixes are organized in five groups. An instruction may include
-- at most one prefix from each group. The result of using multiple prefixes
-- from a single group is undefined.
--
-- We give the original meaning in parentheses, but prefixes can be used with
-- other meanings.
--
-- G1: 0x66 (Operand-size override)
-- G2: 0x67 (Address-size override)
-- G3: 0x2E (CS segment override)
--     0x3E (DS segment override)
--     0x26 (ES segment override)
--     0x64 (FS segment override)
--     0x65 (GS segment override)
--     0x36 (SS segment override)
-- G4: 0xF0 (atomic memory access (lock))
-- G5: 0xF3 (repeat while zero)
--     0xF2 (repeat while non-zero)
--
-- New opcode encodings (VEX, XOP, etc.) only support prefixes from groups G2
-- and G3.
---------------------------------------------------------------------------


-- | Prefixes
data Prefix
  = P_66
  | P_67
  | P_2E
  | P_3E
  | P_26
  | P_64
  | P_65
  | P_36
  | P_F0
  | P_F3
  | P_F2
  deriving (Show,Eq,Ord)

toPrefix :: Word8 -> Maybe Prefix
toPrefix = \case
  0x66 -> Just P_66
  0x67 -> Just P_67
  0x2E -> Just P_2E
  0x3E -> Just P_3E
  0x26 -> Just P_26
  0x64 -> Just P_64
  0x65 -> Just P_65
  0x36 -> Just P_36
  0xF0 -> Just P_F0
  0xF3 -> Just P_F3
  0xF2 -> Just P_F2
  _    -> Nothing

fromPrefix :: Prefix -> Word8
fromPrefix = \case
  P_66 -> 0x66
  P_67 -> 0x67
  P_2E -> 0x2E
  P_3E -> 0x3E
  P_26 -> 0x26
  P_64 -> 0x64
  P_65 -> 0x65
  P_36 -> 0x36
  P_F0 -> 0xF0
  P_F3 -> 0xF3
  P_F2 -> 0xF2

-- | Get the prefix group
prefixGroup :: Prefix -> Int
prefixGroup = \case
  P_66  -> 1
  P_67  -> 2
  P_2E  -> 3
  P_3E  -> 3
  P_26  -> 3
  P_64  -> 3
  P_65  -> 3
  P_36  -> 3
  P_F0  -> 4
  P_F3  -> 5
  P_F2  -> 5
