{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | A natural number in a specified range (fixed and checked at compile-time)
module Haskus.Format.Number.NaturalRange
   ( NatRange
   , pattern NatRange
   , natRange
   , safeMakeNatRange
   , makeNatRange
   , unsafeMakeNatRange
   , widenNatRange
   , (.++.)
   )
where

import Haskus.Format.Number.BitNat
import Haskus.Utils.Types
import Numeric.Natural

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> :set -XScopedTypeVariables


-- | A natural number in the specified range
newtype NatRange (f :: Nat) (t :: Nat) = NatRange' (BitNat (NatBitCount (t-f+1)))

-- | Show instance for natural range
instance
   ( KnownNat (t-f)
   , KnownNat t
   , KnownNat f
   , Num (BitNatWord (NatBitCount (t-f+1)))
   , Integral (BitNatWord (NatBitCount (t-f+1)))
   ) => Show (NatRange f t) where
   showsPrec d x = showParen (d /= 0)
      $ showString "NatRange @"
      . showsPrec 0 (natValue' @f)
      . showString " @"
      . showsPrec 0 (natValue' @t)
      . showString " "
      . showsPrec 0 (toNaturalNatRange x)

type CheckInRange f t n =
   ( Assert (n <=? t) (() :: Constraint)
      ('ShowType n
       ':<>: 'Text " isn't in the range ["
       ':<>: 'ShowType f
       ':<>: 'Text ","
       ':<>: 'ShowType t
       ':<>: 'Text "]"
      )
   , Assert (f <=? n) (() :: Constraint)
      ('ShowType n
       ':<>: 'Text " isn't in the range ["
       ':<>: 'ShowType f
       ':<>: 'Text ","
       ':<>: 'ShowType t
       ':<>: 'Text "]"
      )
   )

type NatRangeBitCount f t = NatBitCount (t-f+1)

type MakeNatRange f t =
   ( Integral (BitNatWord (NatRangeBitCount f t))
   , MakeBitNat (NatRangeBitCount f t)
   , KnownNat f
   , KnownNat t
   , Assert (f <=? t) (() :: Constraint)
      ('Text "["
       ':<>: 'ShowType f
       ':<>: 'Text ","
       ':<>: 'ShowType t
       ':<>: 'Text "] isn't a valid range"
      )
   )

-- | Create a value in a Natural range
unsafeMakeNatRange :: forall f t.
   ( MakeNatRange f t
   ) => Natural -> NatRange f t
unsafeMakeNatRange v = NatRange' (BitNat @(NatRangeBitCount f t) (v - natValue @f))

-- | Create a value in a Natural range (check validity)
safeMakeNatRange :: forall f t.
   ( MakeNatRange f t
   ) => Natural -> Maybe (NatRange f t)
safeMakeNatRange v
   | v < natValue @f || v > natValue @t = Nothing
   | otherwise                          = Just (unsafeMakeNatRange @f @t v)

-- | Create a value in a Natural range (check validity and throw on error)
makeNatRange :: forall f t.
   ( MakeNatRange f t
   ) => Natural -> NatRange f t
makeNatRange v = case safeMakeNatRange @f @t v of
   Nothing ->error $ show v ++ " isn't in the range ["
               ++ show (natValue @f :: Natural)
               ++ ","
               ++ show (natValue @t :: Natural)
               ++ "]"
   Just x -> x


-- | Create a value in a Natural range
natRange :: forall (n :: Nat) f t.
   ( MakeNatRange f t
   , CheckInRange f t n
   , KnownNat n
   ) => NatRange f t
natRange = unsafeMakeNatRange (natValue @n)

-- | Convert a NatRange into a Natural
toNaturalNatRange :: forall f t.
   ( KnownNat f
   , Integral (BitNatWord (NatBitCount (t-f+1)))
   ) => NatRange f t -> Natural
toNaturalNatRange (NatRange' x) = natValue @f + toNaturalW x

-- | Natural range pattern
--
-- >>> NatRange @10 @12 11
-- NatRange @10 @12 11
--
pattern NatRange :: forall (f :: Nat) (t :: Nat).
   ( MakeNatRange f t
   ) => Natural -> NatRange f t
{-# COMPLETE NatRange #-}
pattern NatRange x <- (toNaturalNatRange -> x)
   where
      NatRange x = makeNatRange @f @t x


-------------------------------------------------
-- Widening
-------------------------------------------------

-- | Widen a natural
--
-- >>> let a = NatRange @18 @100 25
-- >>> widenNatRange @16 @200 a
-- NatRange @16 @200 25
--
widenNatRange :: forall f2 t2 f1 t1.
   ( WidenNatRange f1 t1 f2 t2
   ) => NatRange f1 t1 -> NatRange f2 t2
widenNatRange (NatRange a) = NatRange a

type WidenNatRange f1 t1 f2 t2 =
   ( Assert ((f2 <=? f1) `AndB` (t1 <=? t2)) (() :: Constraint)
      ('Text "Can't widen a natural range ["
       ':<>: 'ShowType f1
       ':<>: 'Text ","
       ':<>: 'ShowType t1
       ':<>: 'Text "] into range ["
       ':<>: 'ShowType f2
       ':<>: 'Text ","
       ':<>: 'ShowType t2
       ':<>: 'Text "]"
      )
   , MakeNatRange f1 t1
   , MakeNatRange f2 t2
   )

-- | Add two natural ranges
--
-- >>> NatRange @2 @4 3 .++. NatRange @7 @17 13
-- NatRange @9 @21 16
--
(.++.) ::
   ( MakeNatRange f1 t1
   , MakeNatRange f2 t2
   , MakeNatRange (f1+f2) (t1+t2)
   ) => NatRange f1 t1 -> NatRange f2 t2 -> NatRange (f1+f2) (t1+t2)
(.++.) (NatRange x) (NatRange y) = NatRange (x+y)

