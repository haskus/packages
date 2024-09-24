-- | ModRM byte
module Haskus.Arch.X86_64.ISA.Encoding.ModRM
  ( ModRM
  , modrmU8
  , mkModRM
  , mkModRM_ext_reg
  , mkModRM_regs_reg_rm
  , mkModRM_mod_rm
  )
where

import Haskus.Binary.Word
import Haskus.Binary.Bits

-- | ModRM byte: mm_rrr_aaa
newtype ModRM
  = ModRM U8
  deriving (Show,Eq,Ord)

instance Semigroup ModRM where
  ModRM x <> ModRM y = ModRM (x .|. y)

instance Monoid ModRM where
  mempty = ModRM 0

modrmU8 :: ModRM -> U8
modrmU8 (ModRM w) = w

mkModRM :: U8 -> U8 -> U8 -> ModRM
mkModRM m r a = ModRM ((m `shiftL` 6) .|. (r `shiftL` 3) .|. a)

mkModRM_ext_reg :: U8 -> U8 -> ModRM
mkModRM_ext_reg ext r = ModRM (0b11_000_000 .|. (ext `shiftL` 3) .|. r)

mkModRM_regs_reg_rm :: U8 -> U8 -> ModRM
mkModRM_regs_reg_rm r m = ModRM (0b11_000_000 .|. (r `shiftL` 3) .|. m)

mkModRM_mod_rm :: U8 -> U8 -> ModRM
mkModRM_mod_rm m rm = ModRM (m `shiftL` 6 .|. rm)
