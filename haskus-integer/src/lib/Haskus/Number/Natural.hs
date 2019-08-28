{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

-- | Multi-precision natural
--
-- Compared to BigNat, it uses a simple Word# for smaller values
module Haskus.Number.Natural
   ( Natural (..)
   , naturalFromWord
   , naturalFromWord#
   , naturalAdd
   , naturalShiftL
   )
where

#include "MachDeps.h"

import Haskus.Number.BigNat
import GHC.Exts
import Data.Bits

-- | A Natural
--
-- Invariant: small values (and 0) use NSmall
data Natural
   = NSmall Word#
   | NBig   {-# UNPACK #-} !BigNat

instance Eq Natural where
   (==) = naturalEq

instance Num Natural where
   (+) = naturalAdd

instance Bits Natural where
   shiftL w n = naturalShiftL w (fromIntegral n)


-- | Eq for Natural
naturalEq :: Natural -> Natural -> Bool
naturalEq (NSmall a) (NSmall b) = isTrue# (a `eqWord#` b)
naturalEq (NBig   a) (NBig   b) = bigNatEq a b
naturalEq (NSmall _) (NBig   _) = False
naturalEq (NBig   _) (NSmall _) = False

-- | Add two naturals
naturalAdd :: Natural -> Natural -> Natural
naturalAdd (NSmall 0##) b            = b
naturalAdd a            (NSmall 0##) = a
naturalAdd (NSmall a)   (NSmall b)   = case plusWord2# a b of
   (# 0##,r0 #) -> NSmall r0
   (#  r1,r0 #) -> NBig (bigNatFrom2LimbsMS r1 r0)
naturalAdd (NSmall a)   (NBig b)    = NBig (bigNatAddWord# b a)
naturalAdd (NBig a)     (NSmall b)  = NBig (bigNatAddWord# a b)
naturalAdd (NBig a)     (NBig b)    = NBig (bigNatAdd a b)

-- | Create a Natural from a Word#
naturalFromWord# :: Word# -> Natural
naturalFromWord# w = NSmall w

-- | Create a Natural from a Word
naturalFromWord :: Word -> Natural
naturalFromWord (W# w) = NSmall w

-- | Shift left
naturalShiftL :: Natural -> Word -> Natural
naturalShiftL (NBig b)       c = NBig (bigNatShiftL b c)
naturalShiftL a@(NSmall 0##) _ = a
naturalShiftL (NSmall b)     (W# c#)
   | isTrue# (clz# b `leWord#` c#) = NSmall (b `uncheckedShiftL#` i)
   | otherwise                     = NBig (bigNatFrom2LimbsMS
                                      (b `uncheckedShiftRL#` (WORD_SIZE_IN_BITS# -# i))
                                      (b `uncheckedShiftL#` i))
   where
      !i = word2Int# c#

