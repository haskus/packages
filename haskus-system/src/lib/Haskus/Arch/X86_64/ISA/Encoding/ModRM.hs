-- | ModRM byte
module Haskus.Arch.X86_64.ISA.Encoding.ModRM
  ( ModRM
  , writeModRM
  , mkModRM
  , mkModRM_reg
  , mkModRM_rm
  , mkModRM_mod

  , mkModRM_mod_rm
  )
where

import Haskus.Binary.Word
import Haskus.Binary.Bits
import qualified Haskus.Memory.Writer as W

-- | ModRM byte: mm_rrr_aaa
newtype ModRM
  = ModRM U8
  deriving (Show,Eq,Ord)

instance Semigroup ModRM where
  ModRM x <> ModRM y = ModRM (x .|. y)

instance Monoid ModRM where
  mempty = ModRM 0

mkModRM :: U8 -> U8 -> U8 -> ModRM
mkModRM m r a = ModRM ((m `shiftL` 6) .|. (r `shiftL` 3) .|. a)

-- | Store value in ModRM.rm
mkModRM_rm :: U8 -> ModRM
mkModRM_rm v = ModRM v

-- | Store value in ModRM.reg
mkModRM_reg :: U8 -> ModRM
mkModRM_reg v = ModRM (v `shiftL` 3)

-- | Store value in ModRM.mod
mkModRM_mod :: U8 -> ModRM
mkModRM_mod m = ModRM (m `shiftL` 6)

-- | Store value in ModRM.mod and ModRM.rm
mkModRM_mod_rm :: U8 -> U8 -> ModRM
mkModRM_mod_rm m rm = ModRM (m `shiftL` 6 .|. rm)

-- | Write a ModRM byte
writeModRM :: ModRM -> W.Writer s
writeModRM (ModRM u) = W.writeU8 u
