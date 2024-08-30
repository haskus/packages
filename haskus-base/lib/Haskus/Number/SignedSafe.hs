{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Signed safe numbers
module Haskus.Number.SignedSafe
   ( Signed (..)
   , signedIsZero
   , signedIsNaN
   , signedFromBitNat
   , signedNegate
   , signedPos
   , signedNeg
   )
where

import Haskus.Number.BitNat
import Haskus.Binary.Bits
import Haskus.Utils.Types
import Prelude hiding (isNaN)

-- | A signed number (not in two-complement form)
--
-- * Bits: ddd..ddds where "s" is the sign bit
-- * Allows symetric positive and negative numbers
-- * Negative zero is NaN
--
newtype Signed (b :: Nat)
   = Signed (BitNat (b+1))

-- | Show instance for Signed
instance
   ( KnownNat b
   , Integral (BitNatWord b)
   , IndexableBits (BitNatWord (b+1))
   , Num (BitNatWord (b+1))
   , Eq (BitNatWord (b+1))
   , Integral (BitNatWord (b+1))
   , ShiftableBits (BitNatWord (b+1))
   , Narrow (b+1) ((b+1)-1)
   ) => Show (Signed b)
   where
   showsPrec d x@(Signed b)
      | signedIsNaN x = showString "NaN"
      | otherwise     =
            showParen (d /= 0)
            $ showString (if signedIsPositive x
                  then ""
                  else "-")
            . showsPrec 0 (bitNatToNatural (b .>>. NatVal @1))

-- | Positive signed literal
--
-- >>> signedPos @5
-- 5
-- >>> signedPos @0
-- 0
--
signedPos :: forall (v :: Nat) b.
   ( b ~ NatBitCount v
   , MakeBitNat b
   , Bitwise (BitNatWord b)
   , Integral (BitNatWord (b+1))
   , KnownNat v
   , ShiftableBits (BitNatWord (b+1))
   , Widen b (b+1)
   ) => Signed b
signedPos = Signed @b (bitNat @v .<<. NatVal @1)

-- | Negative signed literal
--
-- >>> signedNeg @5
-- -5
-- >>> signedNeg @0
-- 0
--
signedNeg :: forall (v :: Nat) b.
   ( b ~ NatBitCount v
   , MakeBitNat b
   , Bitwise (BitNatWord b)
   , KnownNat v
   , Widen b (b+1)
   , ShiftableBits (BitNatWord (b+1))
   , IsBitNat (b+1)
   ) => Signed b
signedNeg = if signedIsZero k then k else signedNegate k
   where
      k = signedPos @v @b


-- | Test for zero
--
-- >>> signedIsZero (signedNeg @5)
-- False
-- >>> signedIsZero (signedNeg @0)
-- True
--
signedIsZero ::
   ( Num (BitNatWord (b+1))
   , Eq (BitNatWord (b+1))
   ) => Signed b -> Bool
signedIsZero (Signed b) = b == bitNatZero

-- | Test for NaN
--
-- >>> signedIsNaN (signedPos @5)
-- False
-- >>> signedIsNaN (signedPos @0)
-- False
--
signedIsNaN ::
   ( Num (BitNatWord (b+1))
   , Eq (BitNatWord (b+1))
   ) => Signed b -> Bool
signedIsNaN (Signed b) = b == bitNatOne

-- | Test if positive
--
-- >>> signedIsPositive (signedPos @5)
-- True
-- >>> signedIsPositive (signedPos @0)
-- True
-- >>> signedIsPositive (signedNeg @5)
-- False
--
signedIsPositive ::
   ( IndexableBits (BitNatWord (b+1))
   ) => Signed b -> Bool
signedIsPositive (Signed b) = not (bitNatTestBit b 0)


-- | Create from a BitNat
--
-- >>> signedFromBitNat (bitNat @18)
-- 18
--
signedFromBitNat ::
   ( ShiftableBits (BitNatWord (b+1))
   , Widen b (b+1)
   ) => BitNat b -> Signed b
signedFromBitNat b = Signed (b .<<. NatVal @1)

-- | Negate a signed number
--
-- >>> signedNegate (signedPos @5)
-- -5
-- >>> signedNegate (signedNeg @5)
-- 5
--
signedNegate ::
   ( IsBitNat (b+1)
   ) => Signed b -> Signed b
signedNegate s
   | signedIsNaN s = s
   | otherwise     = case s of 
      Signed b -> Signed (b `bitNatXor` bitNatOne)
