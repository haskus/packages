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

module Haskus.Format.Number
   ( NatVal (..)
   , Widen
   , widen
   , Narrow
   , narrow
   , W
   , pattern W
   , unsafeMakeW
   , safeMakeW
   , zeroW
   , oneW
   , extractW
   , compareW
   , (.+.)
   , (.-.)
   , (.*.)
   , (./.)
   , (.<<.)
   , (.>>.)
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Bits
import Haskus.Utils.Types
import Haskus.Utils.Types.Proxy
import Numeric.Natural
import Data.Kind

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> :set -XScopedTypeVariables

newtype W (b :: Nat)
   = W' (WW b)

pattern W :: forall n. (Integral (WW n), MakeW n) => Natural -> W n
pattern W x <- (toNatural -> x)
   where
      W x = makeW @n x

mapW :: (WW a -> WW a) -> W a -> W a
mapW f (W' x) = W' (f x)

zipWithW :: (WW a -> WW a -> WW b) -> W a -> W a -> W b
zipWithW f (W' x) (W' y) = W' (f x y)

-- | Show instance for naturals
instance (KnownNat b, Show (WW b)) => Show (W b) where
   showsPrec d (W' x) = showParen (d /= 0)
      $ showString "W @"
      . showsPrec 0 (natValue' @b)
      . showString " "
      . showsPrec 0 x

type family WW b where
   WW 0 = TypeError ('Text "Naturals encoded on 0 bits are not allowed")
   WW b = WW' (b <=? 8) (b <=? 16) (b <=? 32) (b <=? 64)

type family WW' b8 b16 b32 b64 where
   WW' 'True _ _ _ = Word8
   WW' _ 'True _ _ = Word16
   WW' _ _ 'True _ = Word32
   WW' _ _ _ 'True = Word64
   WW' _ _ _ _     = Natural

-------------------------------------------------
-- Creation
-------------------------------------------------

-- | Zero natural
zeroW :: Num (WW a) => W a
zeroW = W' 0

-- | One natural
oneW :: Num (WW a) => W a
oneW = W' 1

-- | Convert a W into a Natural
toNatural :: Integral (WW a) => W a -> Natural
toNatural (W' x) = fromIntegral x

-- | Create a natural
unsafeMakeW :: forall a. (Maskable a (WW a)) => WW a -> W a
unsafeMakeW x = W' (mask @a x)

type MakeW a =
   ( Maskable a (WW a)
   , ShiftableBits (WW a)
   , Show (WW a)
   , Eq (WW a)
   , Num (WW a)
   )

-- | Create a natural (check overflow)
safeMakeW :: forall a. MakeW a => Natural -> Maybe (W a)
safeMakeW x = 
   let
      x' = fromIntegral x :: WW a
   in case x' `uncheckedShiftR` natValue' @a of
      0 -> Just (unsafeMakeW x')
      _ -> Nothing

-- | Create a natural (check overflow and throw an error)
makeW :: forall a. MakeW a => Natural -> W a
makeW x = case safeMakeW x of
   Just y  -> y
   Nothing -> error $
               "`" ++ show x
               ++ "` is out of the range of values that can be encoded by a "
               ++ show (natValue' @a)
               ++ "-bit natural number: [0.."
               ++ show (2 ^ (natValue' @a) -1 :: Natural)
               ++ "]"

-- | Extract the primitive value
extractW :: W a -> WW a
extractW (W' a) = a

-------------------------------------------------
-- Widening
-------------------------------------------------

-- | Widen a natural
--
-- >>>  widen @7 (W @5 25)
-- W @7 25
--
widen :: forall b a. Widen a b => W a -> W b
widen (W' a) = W' (fromIntegral a)

type Widen a b =
   ( Assert (a <=? b) (() :: Constraint)
      ('Text "Can't widen a natural of "
       ':<>: 'ShowType a
       ':<>: 'Text " bits into a natural of "
       ':<>: 'ShowType b
       ':<>: 'Text " bits"
      )
   , Integral (WW a)
   , Integral (WW b)
   )

-- | Narrow a natural
--
-- >>> narrow @3 (W @5 25)
-- W @3 1
--
narrow :: forall b a. Narrow a b => W a -> W b
narrow (W' a) = unsafeMakeW (fromIntegral a)

type Narrow a b =
   ( Assert (b <=? a) (() :: Constraint)
      ('Text "Can't narrow a natural of "
       ':<>: 'ShowType a
       ':<>: 'Text " bits into a natural of "
       ':<>: 'ShowType b
       ':<>: 'Text " bits"
      )
   , Integral (WW a)
   , Integral (WW b)
   , Maskable b (WW b)
   )
   
-------------------------------------------------
-- Comparison
-------------------------------------------------

-- | Compare two naturals
compareW :: forall a b.
   ( Ord (WW (Max a b))
   , Widen a (Max a b)
   , Widen b (Max a b)
   ) => W a -> W b -> Ordering
compareW x y = compare x' y'
   where
      W' x' = widen @(Max a b) x
      W' y' = widen @(Max a b) y

instance Eq (WW a) => Eq (W a) where
   (W' x) == (W' y) = x == y

instance Ord (WW a) => Ord (W a) where
   compare (W' x) (W' y) = compare x y

-------------------------------------------------
-- Addition / Subtraction
-------------------------------------------------

-- | Add two Naturals
--
-- >>> W @5 25 .+. W @2 3
-- W @6 28
--
(.+.) :: forall a b m.
   ( m ~ (Max a b + 1)
   , Widen a m
   , Widen b m
   , Num (WW m)
   ) => W a -> W b -> W m
(.+.) x y = zipWithW (+) (widen @m x) (widen @m y)

-- | Sub two Naturals
--
-- >>> W @5 25 .-. W @2 3
-- Just (W @5 22)
--
-- >>> W @5 2 .-. W @2 3
-- Nothing
--
(.-.) :: forall a b m.
   ( m ~ Max a b
   , Widen a m
   , Widen b m
   , Num (WW m)
   ) => W a -> W b -> Maybe (W m)
(.-.) (widen @m -> x) (widen @m -> y) = case compare x y of
   LT -> Nothing
   EQ -> Just zeroW
   GT -> Just (zipWithW (-) x y)

-- | Multiply two Naturals
--
-- >>> W @5 25 .*. W @2 3
-- W @7 75
--
(.*.) :: forall a b m.
   ( m ~ (a + b)
   , Widen a m
   , Widen b m
   , Num (WW m)
   ) => W a -> W b -> W m
(.*.) x y = zipWithW (*) (widen @m x) (widen @m y)

-- | Divide two Naturals, return (factor,rest)
--
-- >>> W @5 25 ./. W @2 3
-- Just (W @5 8,W @2 1)
--
-- >>> W @5 25 ./. W @2 0
-- Nothing
--
-- > W @2 3 ./. W @5 25
-- Just (W @2 0,W @5 3)
--
(./.) :: forall a b m.
   ( m ~ Max a b
   , Widen a m
   , Widen b m
   , Num (WW (Min a b))
   ) => W a -> W b -> Maybe (W a,W (Min a b))
(./.) x y
   | y == zeroW = Nothing
   | otherwise  = Just (W' (fromIntegral q), W' (fromIntegral r))
   where
      (q,r) = quotRem x' y'
      W' x' = widen @m x
      W' y' = widen @m y

-------------------------------------------------
-- Shift
-------------------------------------------------

-- | Shift-left naturals
--
-- >>> let x = W @5 25
-- >>> x .<<. NatVal @2
-- W @7 100
--
-- >>> show (x .<<. NatVal @2) == show (x .*. W @3 4)
-- False
--
-- >>> x .<<. NatVal @2 == narrow (x .*. W @3 4)
-- True
--
(.<<.) :: forall (s :: Nat) a.
   ( ShiftableBits (WW (a + s))
   , KnownNat s
   , Widen a (a+s)
   ) => W a -> NatVal s -> W (a + s)
(.<<.) x _ = mapW (`uncheckedShiftL` natValue @s) (widen @(a+s) x)

-- | Shift-right naturals
--
-- >>> W @5 25 .>>. NatVal @2
-- W @3 6
--
(.>>.) :: forall (s :: Nat) a.
   ( ShiftableBits (WW a)
   , KnownNat s
   , Narrow a (a-s)
   ) => W a -> NatVal s -> W (a - s)
(.>>.) x _ = narrow @(a-s) (mapW (`uncheckedShiftR` natValue @s) x)
