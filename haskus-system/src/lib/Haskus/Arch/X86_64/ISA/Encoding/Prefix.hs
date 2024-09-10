-- | Instruction prefixes
module Haskus.Arch.X86_64.ISA.Encoding.Prefix
  ( LegacyPrefix (..)
  , toLegacyPrefix
  , fromLegacyPrefix
  , legacyPrefixGroup
  )
where

import Haskus.Number.Word

---------------------------------------------------------------------------
-- Note [Prefixes]
-- ~~~~~~~~~~~~~~~
-- 
-- An instruction optionally begins with up to four legacy prefixes, in any
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
-- Legacy prefixes that are used as opcode extensions are mandatory.
--
--
-- Note [Prefix groups]
-- ~~~~~~~~~~~~~~~~~~~~
--
-- Legacy prefixes are organized in five groups. An instruction may include
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
-- New opcode encodings (VEX, XOP, etc.) only support legacy prefixes from
-- groups G2 and G3.
---------------------------------------------------------------------------


-- | Legacy prefixes
data LegacyPrefix
   = LegacyPrefix66
   | LegacyPrefix67
   | LegacyPrefix2E
   | LegacyPrefix3E
   | LegacyPrefix26
   | LegacyPrefix64
   | LegacyPrefix65
   | LegacyPrefix36
   | LegacyPrefixF0
   | LegacyPrefixF3
   | LegacyPrefixF2
   deriving (Show,Eq,Ord)

toLegacyPrefix :: Word8 -> Maybe LegacyPrefix
toLegacyPrefix = \case
   0x66 -> Just LegacyPrefix66
   0x67 -> Just LegacyPrefix67
   0x2E -> Just LegacyPrefix2E
   0x3E -> Just LegacyPrefix3E
   0x26 -> Just LegacyPrefix26
   0x64 -> Just LegacyPrefix64
   0x65 -> Just LegacyPrefix65
   0x36 -> Just LegacyPrefix36
   0xF0 -> Just LegacyPrefixF0
   0xF3 -> Just LegacyPrefixF3
   0xF2 -> Just LegacyPrefixF2
   _    -> Nothing

fromLegacyPrefix :: LegacyPrefix -> Word8
fromLegacyPrefix = \case
   LegacyPrefix66 -> 0x66
   LegacyPrefix67 -> 0x67
   LegacyPrefix2E -> 0x2E
   LegacyPrefix3E -> 0x3E
   LegacyPrefix26 -> 0x26
   LegacyPrefix64 -> 0x64
   LegacyPrefix65 -> 0x65
   LegacyPrefix36 -> 0x36
   LegacyPrefixF0 -> 0xF0
   LegacyPrefixF3 -> 0xF3
   LegacyPrefixF2 -> 0xF2

-- | Get the legacy prefix group
legacyPrefixGroup :: LegacyPrefix -> Int
legacyPrefixGroup = \case
   LegacyPrefix66  -> 1
   LegacyPrefix67  -> 2
   LegacyPrefix2E  -> 3
   LegacyPrefix3E  -> 3
   LegacyPrefix26  -> 3
   LegacyPrefix64  -> 3
   LegacyPrefix65  -> 3
   LegacyPrefix36  -> 3
   LegacyPrefixF0  -> 4
   LegacyPrefixF3  -> 5
   LegacyPrefixF2  -> 5

