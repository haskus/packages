{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Multi-precision natural
--
-- Compared to BigNat, it uses a simple Word# for smaller values
module Haskus.Number.Natural
   ( Natural (..)
   , pattern NSmall
   , pattern NBig
   , naturalFromWord
   , naturalFromWord#
   , naturalAdd
   , naturalMul
   , naturalShiftL
   , naturalShiftR
   )
where

#include "MachDeps.h"

import Haskus.Number.BigNat
import GHC.Exts
import Data.Bits

-- | A Natural
--
-- Invariant: small values (and 0) use NSmall
data Natural = Natural (# Word# | ByteArray# #)
--    = NSmall Word#
--    | NBig   {-# UNPACK #-} !BigNat

{-# COMPLETE NSmall, NBig #-}
pattern NSmall :: Word# -> Natural
pattern NSmall w = Natural (# w | #)

pattern NBig :: BigNat -> Natural
pattern NBig bn <- Natural (# | (BigNat -> bn) #)
   where
      NBig (BigNat ba) = Natural (# | ba #)

instance Eq Natural where
   (==) = naturalEq

instance Num Natural where
   (+) = naturalAdd
   (*) = naturalMul

instance Bits Natural where
   shiftL w n = naturalShiftL w (fromIntegral n)
   shiftR w n = naturalShiftR w (fromIntegral n)


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


-- | Multiply two naturals
naturalMul :: Natural -> Natural -> Natural
naturalMul a@(NSmall 0##) _              = a
naturalMul _              b@(NSmall 0##) = b
naturalMul (NSmall 1##)   b              = b
naturalMul a              (NSmall 1##)   = a
naturalMul (NSmall a)   (NSmall b)   = case timesWord2# a b of
   (# 0##,r0 #) -> NSmall r0
   (#  r1,r0 #) -> NBig (bigNatFrom2LimbsMS r1 r0)
naturalMul (NBig a)     (NBig b)    = NBig (bigNatMul a b)
-- TODO: implement and use bigNatMulWord#
naturalMul (NSmall a)   (NBig b)    = NBig (bigNatMul b (bigNatFromWord# a))
naturalMul (NBig a)     (NSmall b)  = NBig (bigNatMul a (bigNatFromWord# b))


-- | Create a Natural from a Word#
naturalFromWord# :: Word# -> Natural
naturalFromWord# w = NSmall w

-- | Create a Natural from a Word
naturalFromWord :: Word -> Natural
naturalFromWord (W# w) = NSmall w

-- | Shift left
naturalShiftL :: Natural -> Word -> Natural
naturalShiftL a              0 = a
naturalShiftL (NBig b)       c = NBig (bigNatShiftL b c)
naturalShiftL a@(NSmall 0##) _ = a
naturalShiftL (NSmall b)     c@(W# c#)
   | isTrue# (clz# b `geWord#` c#) = NSmall (b `uncheckedShiftL#` i)
   | otherwise                     = NBig (bigNatShiftL (bigNatFromWord# b) c)
   where
      !i = word2Int# c#

-- | Shift right
naturalShiftR :: Natural -> Word -> Natural
naturalShiftR a              0  = a
naturalShiftR a@(NSmall 0##) _  = a
naturalShiftR (NSmall b)(W# c#) = NSmall (b `uncheckedShiftRL#` (word2Int# c#))
naturalShiftR (NBig   b)     c  = case bigNatShiftR b c of
   r@(BigNat ba) -> case bigNatLimbCount# ba of
      0# -> NSmall 0##
      1# -> NSmall (indexWordArray# ba 0#)
      _  -> NBig r

