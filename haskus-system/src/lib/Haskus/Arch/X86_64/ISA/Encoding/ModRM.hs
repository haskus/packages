-- | ModRM byte
module Haskus.Arch.X86_64.ISA.Encoding.ModRM
  ( ModRM
  , writeModRM
  , mkModRM
  , mkModRM_ext
  , mkModRM_ext_reg
  , mkModRM_reg
  , mkModRM_regs_reg_rm
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

-- | Store opcode extension in ModRM.reg and register in ModRM.rm
mkModRM_ext_reg :: U8 -> U8 -> ModRM
mkModRM_ext_reg ext r = ModRM (0b11_000_000 .|. (ext `shiftL` 3) .|. r)

-- | Store register in ModRM.reg
mkModRM_reg :: U8 -> ModRM
mkModRM_reg r = ModRM r

-- | Store opcode extension in ModRM.reg
mkModRM_ext :: U8 -> ModRM
mkModRM_ext ext = ModRM (ext `shiftL` 3)

mkModRM_regs_reg_rm :: U8 -> U8 -> ModRM
mkModRM_regs_reg_rm r m = ModRM (0b11_000_000 .|. (r `shiftL` 3) .|. m)

mkModRM_mod_rm :: U8 -> U8 -> ModRM
mkModRM_mod_rm m rm = ModRM (m `shiftL` 6 .|. rm)

-- | Write a ModRM byte
writeModRM :: ModRM -> W.Writer s
writeModRM (ModRM u) = W.writeU8 u
