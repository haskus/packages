{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Signed numbers
module Haskus.Number.Signed
   ( Signed (..)
   , SignedIsZero
   , signedIsZero
   , SignedFromBitNat
   , signedFromBitNat
   , SignedNegate
   , signedNegate
   , SignedPos
   , signedPos
   , SignedNeg
   , signedNeg
   )
where

import Haskus.Number.BitNat
import Haskus.Binary.Bits
import Haskus.Utils.Types

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> import Haskus.Number.BitNat

-- | A signed number (not in two-complement form)
--
-- * Bits: ddd..ddds where "s" is the sign bit
-- * Allows symetric positive and negative numbers
-- * Positive and negative zeros are zero
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
      | signedIsZero x = showString "0"
      | otherwise      =
            showParen (d /= 0)
            $ showString (if signedIsPositive x
                  then ""
                  else "-")
            . showsPrec 0 (bitNatToNatural (b .>>. NatVal @1))

type SignedPos b v =
   ( b ~ NatBitCount v
   , MakeBitNat b
   , KnownNat v
   , BitNatShiftLeft b 1
   )

-- | Positive signed literal
--
-- >>> signedPos @5
-- 5
-- >>> signedPos @0
-- 0
--
signedPos :: forall (v :: Nat) b.
   ( SignedPos b v
   ) => Signed b
signedPos = Signed @b (bitNat @v .<<. NatVal @1)


type SignedNeg b v =
   ( SignedPos b v 
   , SignedNegate b
   )

-- | Negative signed literal
--
-- >>> signedNeg @5
-- -5
-- >>> signedNeg @0
-- 0
--
signedNeg :: forall (v :: Nat) b.
   ( SignedNeg b v 
   ) => Signed b
signedNeg = signedNegate (signedPos @v @b)


type SignedIsZero b =
   ( BitNatShiftRight (b+1) 1
   )


-- | Test for zero
--
-- >>> signedIsZero (signedNeg @5)
-- False
-- >>> signedIsZero (signedNeg @0)
-- True
--
signedIsZero :: forall b.
   ( SignedIsZero b
   ) => Signed b -> Bool
signedIsZero (Signed b) = (b .>>. NatVal @1 == bitNatZero)

type SignedIsPositive b =
   ( IndexableBits (BitNatWord (b+1))
   )

-- | Test if positive
--
-- >>> signedIsPositive (signedPos @5)
-- True
-- >>> signedIsPositive (signedPos @0)
-- True
-- >>> signedIsPositive (signedNeg @5)
-- False
--
signedIsPositive :: forall b.
   ( SignedIsPositive b
   ) => Signed b -> Bool
signedIsPositive (Signed b) = not (bitNatTestBit b 0)


type SignedFromBitNat b =
   ( ShiftableBits (BitNatWord (b+1))
   , Widen b (b+1)
   )

-- | Create from a BitNat
--
-- >>> signedFromBitNat (bitNat @18)
-- 18
--
signedFromBitNat :: forall b.
   ( SignedFromBitNat b
   ) => BitNat b -> Signed b
signedFromBitNat b = Signed (b .<<. NatVal @1)


type SignedNegate b =
   ( IsBitNat (b+1)
   )

-- | Negate a signed number
--
-- >>> signedNegate (signedPos @5)
-- -5
-- >>> signedNegate (signedNeg @5)
-- 5
--
signedNegate ::
   ( SignedNegate b
   ) => Signed b -> Signed b
signedNegate (Signed b)= Signed (b `bitNatXor` bitNatOne)
