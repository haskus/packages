{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Haskus.Arch.X86_64.ISA.Encoding.Vec
  ( Vec
  , vecCode
  , vecCodeX
  , vecREX
  , VecType(..)
  , vecType
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
--    4: REX: require REX to be present
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

vecREX :: Vec -> Bool
vecREX (Vec w) = w .&. 0b00_0_1_0000 /= 0

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
