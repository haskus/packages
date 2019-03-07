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

-- | Natural numbers
module Haskus.Format.Number.BitNat
   ( NatVal (..)
   , Widen
   , widen
   , Narrow
   , narrow
   , IsBitNat
   , BitNat
   , pattern BitNat
   , unsafeMakeBitNat
   , safeMakeBitNat
   , bitNat
   , bitNatZero
   , bitNatOne
   , extractW
   , compareW
   , (.+.)
   , (.-.)
   , (.*.)
   , (./.)
   , BitNatShiftLeft
   , BitNatShiftRight
   , (.<<.)
   , (.>>.)
   , bitNatTestBit
   , bitNatXor
   , bitNatAnd
   , bitNatOr
   -- * Internal
   , BitNatWord
   , MakeBitNat
   , bitNatToNatural
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Bits
import Haskus.Utils.Types
import Numeric.Natural

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> :set -XScopedTypeVariables

-- | A natural on `b` bits
newtype BitNat (b :: Nat)
   = BitNat' (BitNatWord b)

pattern BitNat :: forall (n :: Nat). (Integral (BitNatWord n), MakeBitNat n) => Natural -> BitNat n
{-# COMPLETE BitNat #-}
pattern BitNat x <- (bitNatToNatural -> x)
   where
      BitNat x = makeW @n x


-- | Create a natural number with the minimal number of bits required to store
-- it
--
-- >>> bitNat @5
-- BitNat @3 5
--
-- >>> bitNat @0
-- BitNat @1 0
--
-- >>> bitNat @158748521123465897456465
-- BitNat @78 158748521123465897456465
--
bitNat :: forall (v :: Nat) (n :: Nat).
   ( n ~ NatBitCount v
   , Integral (BitNatWord n)
   , MakeBitNat n
   , KnownNat v
   ) => BitNat n
bitNat = BitNat @n (natValue @v)

mapW :: (BitNatWord a -> BitNatWord a) -> BitNat a -> BitNat a
mapW f (BitNat' x) = BitNat' (f x)

zipWithW :: (BitNatWord a -> BitNatWord a -> BitNatWord b) -> BitNat a -> BitNat a -> BitNat b
zipWithW f (BitNat' x) (BitNat' y) = BitNat' (f x y)

-- | Show instance for BitNat
instance (KnownNat b, Integral (BitNatWord b)) => Show (BitNat b) where
   showsPrec d x = showParen (d /= 0)
      $ showString "BitNat @"
      . showsPrec 0 (natValue' @b)
      . showString " "
      . showsPrec 0 (bitNatToNatural x)

-- | BitNat backing type
type family BitNatWord b where
   BitNatWord 0 = TypeError ('Text "Naturals encoded on 0 bits are not allowed")
   BitNatWord b = BitNatWord' (b <=? 8) (b <=? 16) (b <=? 32) (b <=? 64)

type family BitNatWord' b8 b16 b32 b64 where
   BitNatWord' 'True _ _ _ = Word8
   BitNatWord' _ 'True _ _ = Word16
   BitNatWord' _ _ 'True _ = Word32
   BitNatWord' _ _ _ 'True = Word64
   BitNatWord' _ _ _ _     = Natural

-------------------------------------------------
-- Creation
-------------------------------------------------

type IsBitNat b =
   ( Num (BitNatWord b)
   , Integral (BitNatWord b)
   , Bitwise (BitNatWord b)
   , IndexableBits (BitNatWord b)
   )

-- | Zero natural
bitNatZero :: Num (BitNatWord a) => BitNat a
bitNatZero = BitNat' 0

-- | One natural
bitNatOne :: Num (BitNatWord a) => BitNat a
bitNatOne = BitNat' 1

-- | Convert a BitNat into a Natural
bitNatToNatural :: Integral (BitNatWord a) => BitNat a -> Natural
bitNatToNatural (BitNat' x) = fromIntegral x

-- | Create a natural
unsafeMakeBitNat :: forall a. (Maskable a (BitNatWord a)) => BitNatWord a -> BitNat a
unsafeMakeBitNat x = BitNat' (mask @a x)

type MakeBitNat a =
   ( Maskable a (BitNatWord a)
   , ShiftableBits (BitNatWord a)
   , Show (BitNatWord a)
   , Eq (BitNatWord a)
   , Num (BitNatWord a)
   )

-- | Create a natural (check overflow)
safeMakeBitNat :: forall a. MakeBitNat a => Natural -> Maybe (BitNat a)
safeMakeBitNat x =
   let
      x' = fromIntegral x :: BitNatWord a
   in case x' `uncheckedShiftR` natValue' @a of
      0 -> Just (unsafeMakeBitNat x')
      _ -> Nothing

-- | Create a natural (check overflow and throw on error)
makeW :: forall a. MakeBitNat a => Natural -> BitNat a
makeW x = case safeMakeBitNat x of
   Just y  -> y
   Nothing -> error $
               "`" ++ show x
               ++ "` is out of the range of values that can be encoded by a "
               ++ show (natValue' @a)
               ++ "-bit natural number: [0.."
               ++ show (2 ^ (natValue' @a) -1 :: Natural)
               ++ "]"

-- | Extract the primitive value
extractW :: BitNat a -> BitNatWord a
extractW (BitNat' a) = a

-------------------------------------------------
-- Widening / Narrowing
-------------------------------------------------

-- | Widen a natural
--
-- >>>  widen @7 (BitNat @5 25)
-- BitNat @7 25
--
widen :: forall b a. Widen a b => BitNat a -> BitNat b
widen (BitNat' a) = BitNat' (fromIntegral a)

type Widen a b =
   ( Assert (a <=? b) (() :: Constraint)
      ('Text "Can't widen a natural of "
       ':<>: 'ShowType a
       ':<>: 'Text " bits into a natural of "
       ':<>: 'ShowType b
       ':<>: 'Text " bits"
      )
   , Integral (BitNatWord a)
   , Integral (BitNatWord b)
   )

-- | Narrow a natural
--
-- >>> narrow @3 (BitNat @5 25)
-- BitNat @3 1
--
narrow :: forall b a. Narrow a b => BitNat a -> BitNat b
narrow (BitNat' a) = unsafeMakeBitNat (fromIntegral a)

type Narrow a b =
   ( Assert (b <=? a) (() :: Constraint)
      ('Text "Can't narrow a natural of "
       ':<>: 'ShowType a
       ':<>: 'Text " bits into a natural of "
       ':<>: 'ShowType b
       ':<>: 'Text " bits"
      )
   , Integral (BitNatWord a)
   , Integral (BitNatWord b)
   , Maskable b (BitNatWord b)
   )
   
-------------------------------------------------
-- Comparison
-------------------------------------------------

-- | Compare two naturals
compareW :: forall a b.
   ( Ord (BitNatWord (Max a b))
   , Widen a (Max a b)
   , Widen b (Max a b)
   ) => BitNat a -> BitNat b -> Ordering
compareW x y = compare x' y'
   where
      BitNat' x' = widen @(Max a b) x
      BitNat' y' = widen @(Max a b) y

instance Eq (BitNatWord a) => Eq (BitNat a) where
   (BitNat' x) == (BitNat' y) = x == y

instance Ord (BitNatWord a) => Ord (BitNat a) where
   compare (BitNat' x) (BitNat' y) = compare x y

-------------------------------------------------
-- Addition / Subtraction
-------------------------------------------------

-- | Add two Naturals
--
-- >>> BitNat @5 25 .+. BitNat @2 3
-- BitNat @6 28
--
(.+.) :: forall a b m.
   ( m ~ (Max a b + 1)
   , Widen a m
   , Widen b m
   , Num (BitNatWord m)
   ) => BitNat a -> BitNat b -> BitNat m
(.+.) x y = zipWithW (+) (widen @m x) (widen @m y)

-- | Sub two Naturals
--
-- >>> BitNat @5 25 .-. BitNat @2 3
-- Just (BitNat @5 22)
--
-- >>> BitNat @5 2 .-. BitNat @2 3
-- Nothing
--
(.-.) :: forall a b m.
   ( m ~ Max a b
   , Widen a m
   , Widen b m
   , Num (BitNatWord m)
   ) => BitNat a -> BitNat b -> Maybe (BitNat m)
(.-.) (widen @m -> x) (widen @m -> y) = case compare x y of
   LT -> Nothing
   EQ -> Just bitNatZero
   GT -> Just (zipWithW (-) x y)

-- | Multiply two Naturals
--
-- >>> BitNat @5 25 .*. BitNat @2 3
-- BitNat @7 75
--
(.*.) :: forall a b m.
   ( m ~ (a + b)
   , Widen a m
   , Widen b m
   , Num (BitNatWord m)
   ) => BitNat a -> BitNat b -> BitNat m
(.*.) x y = zipWithW (*) (widen @m x) (widen @m y)

-- | Divide two Naturals, return (factor,rest)
--
-- >>> BitNat @5 25 ./. BitNat @2 3
-- Just (BitNat @5 8,BitNat @2 1)
--
-- >>> BitNat @5 25 ./. BitNat @2 0
-- Nothing
--
-- > BitNat @2 3 ./. BitNat @5 25
-- Just (BitNat @2 0,BitNat @5 3)
--
(./.) :: forall a b m.
   ( m ~ Max a b
   , Widen a m
   , Widen b m
   , Num (BitNatWord (Min a b))
   ) => BitNat a -> BitNat b -> Maybe (BitNat a,BitNat (Min a b))
(./.) x y
   | y == bitNatZero = Nothing
   | otherwise  = Just (BitNat' (fromIntegral q), BitNat' (fromIntegral r))
   where
      (q,r) = quotRem x' y'
      BitNat' x' = widen @m x
      BitNat' y' = widen @m y

-------------------------------------------------
-- Shift
-------------------------------------------------

type BitNatShiftRight a s =
   ( ShiftableBits (BitNatWord a)
   , KnownNat s
   , Narrow a (a-s)
   )

type BitNatShiftLeft a s =
   ( ShiftableBits (BitNatWord (a+s))
   , KnownNat s
   , Widen a (a+s)
   )

-- | Shift-left naturals
--
-- >>> let x = BitNat @5 25
-- >>> x .<<. NatVal @2
-- BitNat @7 100
--
-- >>> show (x .<<. NatVal @2) == show (x .*. BitNat @3 4)
-- False
--
-- >>> x .<<. NatVal @2 == narrow (x .*. BitNat @3 4)
-- True
--
(.<<.) :: forall (s :: Nat) a.
   ( BitNatShiftLeft a s
   ) => BitNat a -> NatVal s -> BitNat (a + s)
(.<<.) x _ = mapW (`uncheckedShiftL` natValue @s) (widen @(a+s) x)

-- | Shift-right naturals
--
-- >>> BitNat @5 25 .>>. NatVal @2
-- BitNat @3 6
--
(.>>.) :: forall (s :: Nat) a.
   ( BitNatShiftRight a s
   ) => BitNat a -> NatVal s -> BitNat (a - s)
(.>>.) x _ = narrow @(a-s) (mapW (`uncheckedShiftR` natValue @s) x)


-- | Test a bit
bitNatTestBit ::
   ( IndexableBits (BitNatWord a)
   ) => BitNat a -> Word -> Bool
bitNatTestBit (BitNat' b) i = testBit b i

-- | Xor
bitNatXor :: forall a.
   ( IsBitNat a
   ) => BitNat a -> BitNat a -> BitNat a
bitNatXor (BitNat' a) (BitNat' b) = BitNat' (a `xor` b)

-- | And
bitNatAnd :: forall a.
   ( IsBitNat a
   ) => BitNat a -> BitNat a -> BitNat a
bitNatAnd (BitNat' a) (BitNat' b) = BitNat' (a .&. b)

-- | Or
bitNatOr :: forall a.
   ( IsBitNat a
   ) => BitNat a -> BitNat a -> BitNat a
bitNatOr (BitNat' a) (BitNat' b) = BitNat' (a .|. b)
