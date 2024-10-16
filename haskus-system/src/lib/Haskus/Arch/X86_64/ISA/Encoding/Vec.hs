{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Haskus.Arch.X86_64.ISA.Encoding.Vec
  ( Vec
  , vecCode
  , vecCodeX
  , VecType(..)
  , vecType
  , isVec64
  , isVec128
  , isVec256
  , isVec512
  , pattern R_XMM0
  , pattern R_XMM1
  , pattern R_XMM2
  , pattern R_XMM3
  , pattern R_XMM4
  , pattern R_XMM5
  , pattern R_XMM6
  , pattern R_XMM7
  , pattern R_XMM8
  , pattern R_XMM9
  , pattern R_XMM10
  , pattern R_XMM11
  , pattern R_XMM12
  , pattern R_XMM13
  , pattern R_XMM14
  , pattern R_XMM15
  , pattern R_YMM0
  , pattern R_YMM1
  , pattern R_YMM2
  , pattern R_YMM3
  , pattern R_YMM4
  , pattern R_YMM5
  , pattern R_YMM6
  , pattern R_YMM7
  , pattern R_YMM8
  , pattern R_YMM9
  , pattern R_YMM10
  , pattern R_YMM11
  , pattern R_YMM12
  , pattern R_YMM13
  , pattern R_YMM14
  , pattern R_YMM15
  )
where

import Haskus.Binary.Word
import Haskus.Binary.Bits

-- | Vector register
--
--  6-7: size 00=64  (MMX)
--            01=128 (XMM)
--            10=256 (YMM)
--            11=512 (ZMM)
--  3-0: register code
newtype Vec
  = Vec U8
  deriving (Eq,Ord)

vecCode :: Vec -> U8
vecCode (Vec w) = w .&. 0b00_0_0_1111

-- | Code with the MSB extracted to be put in REX.X for example
vecCodeX :: Vec -> (Bool, U8)
vecCodeX r = (testBit c 3, c .&. 0b0111)
  where
    !c = vecCode r

data VecType
  = VecMMX
  | VecXMM
  | VecYMM
  | VecZMM
  deriving (Eq,Ord)

instance Show VecType where
  show = \case
    VecMMX -> "MMX"
    VecXMM -> "XMM"
    VecYMM -> "YMM"
    VecZMM -> "ZMM"

vecType :: Vec -> VecType
vecType (Vec w) = case w `shiftR` 6 of
  0b00 -> VecMMX
  0b01 -> VecXMM
  0b10 -> VecYMM
  _    -> VecZMM


instance Show Vec where
  show v = show (vecType v) ++ show (vecCode v)


isVec64 :: Vec -> Maybe Vec
isVec64 v = if vecType v == VecMMX then Just v else Nothing

isVec128 :: Vec -> Maybe Vec
isVec128 v = if vecType v == VecXMM then Just v else Nothing

isVec256 :: Vec -> Maybe Vec
isVec256 v = if vecType v == VecYMM then Just v else Nothing

isVec512 :: Vec -> Maybe Vec
isVec512 v = if vecType v == VecZMM then Just v else Nothing

pattern R_XMM0  = Vec 0b01_00_0000
pattern R_XMM1  = Vec 0b01_00_0001
pattern R_XMM2  = Vec 0b01_00_0010
pattern R_XMM3  = Vec 0b01_00_0011
pattern R_XMM4  = Vec 0b01_00_0100
pattern R_XMM5  = Vec 0b01_00_0101
pattern R_XMM6  = Vec 0b01_00_0110
pattern R_XMM7  = Vec 0b01_00_0111
pattern R_XMM8  = Vec 0b01_00_1000
pattern R_XMM9  = Vec 0b01_00_1001
pattern R_XMM10 = Vec 0b01_00_1010
pattern R_XMM11 = Vec 0b01_00_1011
pattern R_XMM12 = Vec 0b01_00_1100
pattern R_XMM13 = Vec 0b01_00_1101
pattern R_XMM14 = Vec 0b01_00_1110
pattern R_XMM15 = Vec 0b01_00_1111

pattern R_YMM0  = Vec 0b10_00_0000
pattern R_YMM1  = Vec 0b10_00_0001
pattern R_YMM2  = Vec 0b10_00_0010
pattern R_YMM3  = Vec 0b10_00_0011
pattern R_YMM4  = Vec 0b10_00_0100
pattern R_YMM5  = Vec 0b10_00_0101
pattern R_YMM6  = Vec 0b10_00_0110
pattern R_YMM7  = Vec 0b10_00_0111
pattern R_YMM8  = Vec 0b10_00_1000
pattern R_YMM9  = Vec 0b10_00_1001
pattern R_YMM10 = Vec 0b10_00_1010
pattern R_YMM11 = Vec 0b10_00_1011
pattern R_YMM12 = Vec 0b10_00_1100
pattern R_YMM13 = Vec 0b10_00_1101
pattern R_YMM14 = Vec 0b10_00_1110
pattern R_YMM15 = Vec 0b10_00_1111
