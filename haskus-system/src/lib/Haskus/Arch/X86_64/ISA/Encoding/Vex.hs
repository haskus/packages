module Haskus.Arch.X86_64.ISA.Encoding.Vex
  ( Vex(..)
  , vexSize
  , writeVex
  , mkVex_LIG_F2_0F_WIG
  , mkVex_LIG_F3_0F_WIG
  , mkVex_128_66_0F_WIG
  , mkVex_256_66_0F_WIG
  , mkVex_128_F2_0F_WIG
  , mkVex_256_F2_0F_WIG
  , mkVex_128_F3_0F_WIG
  , mkVex_256_F3_0F_WIG
  , mkVex_128_0F_WIG
  , mkVex_256_0F_WIG
  , mkVex_LZ_0F38_W0
  , mkVex_LZ_0F38_W1
  , extendVex
  , vexSetB
  , vexSetR
  , vexSetV
  )
where

import Haskus.Binary.Word
import Haskus.Binary.Bits
import qualified Haskus.Memory.Writer as W

data Vex
  = Vex2 !U8      -- ^ RvvvvLpp
  | Vex3 !U8 !U8  -- ^ RXBmmmmm WvvvvLpp
  deriving (Show,Eq,Ord)

mkVex_LIG_F2_0F_WIG :: Vex
mkVex_LIG_F3_0F_WIG :: Vex
mkVex_128_66_0F_WIG :: Vex
mkVex_256_66_0F_WIG :: Vex
mkVex_128_F2_0F_WIG :: Vex
mkVex_256_F2_0F_WIG :: Vex
mkVex_128_F3_0F_WIG :: Vex
mkVex_256_F3_0F_WIG :: Vex
mkVex_128_0F_WIG :: Vex
mkVex_256_0F_WIG :: Vex
mkVex_LZ_0F38_W0 :: Vex
mkVex_LZ_0F38_W1 :: Vex

mkVex_LIG_F2_0F_WIG = Vex2 0b1_1111_0_11 -- R=1, vvvv=1111, L=0, pp=11
mkVex_LIG_F3_0F_WIG = Vex2 0b1_1111_0_11 -- R=1, vvvv=1111, L=0, pp=10
mkVex_128_66_0F_WIG = Vex2 0b1_1111_0_01 -- R=1, vvvv=1111, L=0, pp=01
mkVex_256_66_0F_WIG = Vex2 0b1_1111_1_01 -- R=1, vvvv=1111, L=1, pp=01
mkVex_128_F2_0F_WIG = Vex2 0b1_1111_0_11 -- R=1, vvvv=1111, L=0, pp=11
mkVex_256_F2_0F_WIG = Vex2 0b1_1111_1_11 -- R=1, vvvv=1111, L=1, pp=11
mkVex_128_F3_0F_WIG = Vex2 0b1_1111_0_10 -- R=1, vvvv=1111, L=0, pp=10
mkVex_256_F3_0F_WIG = Vex2 0b1_1111_1_10 -- R=1, vvvv=1111, L=1, pp=10
mkVex_128_0F_WIG    = Vex2 0b1_1111_0_00 -- R=1, vvvv=1111, L=0, pp=00
mkVex_256_0F_WIG    = Vex2 0b1_1111_1_00 -- R=1, vvvv=1111, L=1, pp=00
mkVex_LZ_0F38_W0    = Vex3 0b111_00010   -- RXB=111, mmmmm=00010
                           0b0_1111_0_00 -- W=0, vvvv=1111, L=0, pp=00
mkVex_LZ_0F38_W1    = Vex3 0b111_00010   -- RXB=111, mmmmm=00010
                           0b1_1111_0_00 -- W=1, vvvv=1111, L=0, pp=00

-- | Set vvvv field in given U8
set_v :: U8 -> U8 -> U8
set_v v r = ((complement v .&. 0b1111) `shiftL` 3) .|. (r .&. 0b1_0000_1_11)

vexSetV :: U8 -> Vex -> Vex
vexSetV v = \case
  Vex2 a   -> Vex2 (set_v v a)
  Vex3 a b -> Vex3 a (set_v v b)

-- | Extend a Vex2 into a Vex3
extendVex :: Vex -> (U8,U8)
extendVex = \case
  Vex3 b c -> (b,c)
  Vex2 a   -> (b,c)
    where
      b = (a .&. 0b1000_0000) -- keep R
           .|. 0b0_1_1_00001  -- X=0 (inv), B=0 (inv), mmmmm=00001 (0F)
      c = a .&. 0b0111_1111   -- W=0, keep vvvv-L-pp

vexSetB :: Vex -> Vex
vexSetB (extendVex -> (a,b)) = Vex3 (a .&. 0b1_1_0_11111) b

vexSetR :: Vex -> Vex
vexSetR = \case
  Vex2 a   -> Vex2 (a .&. 0b0_1_1_11111)
  Vex3 a b -> Vex3 (a .&. 0b0_1_1_11111) b

-- | VEX prefix size in bytes
vexSize :: Vex -> U
vexSize = \case
  Vex2 {} -> 2
  Vex3 {} -> 3

-- | Write a VEX prefix
writeVex :: Vex -> W.Writer s
writeVex = \case
  Vex2 v1    -> W.writeU8 0xC5 <> W.writeU8 v1
  Vex3 v1 v2 -> W.writeU8 0xC4 <> W.writeU8 v1 <> W.writeU8 v2
