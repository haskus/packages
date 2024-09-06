-- | Operating modes
module Haskus.Arch.X86_64.ISA.OperatingMode
  ( Mode (..)
  , allModes
  , is64bitMode
  , is32bitMode
  , is16bitMode
  , isLongMode
  , isLegacyMode
  , supportsRipRelativeAddr
  , modeName
  )
where

-- | X86 and X86-64 operating modes
data Mode
  = Mode64    -- ^ 64-bit mode ("long 64-bit mode")
  | Mode64_32 -- ^ 64-bit sub mode: compat with 32-bit code ("long compatibility mode")
  | Mode32    -- ^ 32-bit mode ("protected mode")
  | Mode32_16 -- ^ 32-bit sub mode: compat with 16-bit code ("virtual-8086 mode")
  | Mode16    -- ^ 16-bit mode ("real mode")
  deriving (Show,Eq,Ord,Enum,Bounded)

-- | All the X86 modes
allModes :: [Mode]
allModes  = [ minBound .. maxBound ]

-- | Return the mode name
modeName :: Mode -> String
modeName = \case
  Mode64    -> "Long 64-bit mode (64-bit)"
  Mode64_32 -> "Long compatibility mode (32-bit)"
  Mode32    -> "Legacy protected mode (32-bit)"
  Mode32_16 -> "Legacy virtual-8086 mode (16-bit)"
  Mode16    -> "Legacy real-mode (16-bit)"

-- | Indicate if it is 64-bit mode
is64bitMode :: Mode -> Bool
is64bitMode = \case
  Mode64 -> True
  _      -> False

-- | Indicate if it is 32-bit mode
is32bitMode :: Mode -> Bool
is32bitMode = \case
  Mode64_32 -> True
  Mode32    -> True
  _         -> False

-- | Indicate if it is 16-bit mode
is16bitMode :: Mode -> Bool
is16bitMode = \case
  Mode32_16 -> True
  Mode16    -> True
  _         -> False

-- | Indicate if it is a long mode
isLongMode :: Mode -> Bool
isLongMode = \case
  Mode64    -> True
  Mode64_32 -> True
  _         -> False

-- | Indicate if it is a legacy mode
isLegacyMode :: Mode -> Bool
isLegacyMode = not . isLongMode

-- | Is RIP-relative addressing supported?
supportsRipRelativeAddr :: Mode -> Bool
supportsRipRelativeAddr = \case
   Mode64    -> True
   _         -> False
