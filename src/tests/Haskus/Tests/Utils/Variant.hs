{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskus.Tests.Utils.Variant
   ( testsVariant
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.Either

import Haskus.Utils.Variant

data A = A deriving (Show,Eq)
data B = B deriving (Show,Eq)
data C = C deriving (Show,Eq)
data D = D deriving (Show,Eq)
data E = E deriving (Show,Eq)
data F = F deriving (Show,Eq)

type ABC = Variant '[A,B,C]
type DEF = Variant '[D,E,F]

b :: ABC
b = toVariantAt @1 B

b2d :: B -> D
b2d = const D

c2d :: C -> D
c2d = const D

b2def :: B -> DEF
b2def = const (toVariant E)

c2def :: C -> DEF
c2def = const (toVariant E)


testsVariant :: TestTree
testsVariant = testGroup "Variant" $
   [ testProperty "set/get by index (match)"
         (fromVariantAt @1 b == Just B)
   , testProperty "set/get by index (dont' match)"
         (fromVariantAt @0 b == Nothing)
   , testProperty "set/get by type (match)"
         (fromVariant    (toVariant B :: ABC) == Just B)
   , testProperty "set/get by type (don't match)"
         (fromVariant @C (toVariant B :: ABC) == Nothing)

   , testProperty "variant equality (match)"
         (b == b)
   , testProperty "variant equality (don't match)"
         (b /= toVariant C)

   , testProperty "update by index (match)"
         (updateVariantAt @1 (const D) b == toVariantAt @1 D)
   , testProperty "update by index (don't match)"
         (updateVariantAt @0 (const F) b == toVariantAt @1 B)
   , testProperty "update by type (match)"
         (updateVariantFirst b2d b == toVariantAt @1 D)
   , testProperty "update by type (don't match)"
         (updateVariantFirst c2d b == toVariant B)
   , testProperty "update/fold by index (match)"
         (foldMapVariantAt @1 b2def b == toVariant E)
   , testProperty "update/fold by index (don't match)"
         (foldMapVariantAt @2 c2def b == toVariant B)

   , testProperty "Convert into tuple"
         (variantToTuple b == (Nothing, Just B, Nothing))
   , testProperty "Convert single variant"
         (variantToValue (toVariant A :: Variant '[A]) == A)

   , testProperty "Lift Either: Left"
         (variantFromEither (Left A :: Either A B) == toVariant A)
   , testProperty "Lift Either: Right"
         (variantFromEither (Right B :: Either A B) == toVariant B)

   , testProperty "To Either: Left"
         (variantToEither (toVariant B :: Variant '[A,B]) == Left B)
   , testProperty "To Either: Right"
         (variantToEither (toVariant A :: Variant '[A,B]) == Right A)

   , testProperty "popVariantHead (match)"
         (popVariantHead (toVariant A :: ABC) == Right A)
   , testProperty "popVariantHead (don't match)"
         (isLeft (popVariantHead b))

   , testProperty "popVariantAt (match)"
         (popVariantAt @1 b == Right B)
   , testProperty "popVariantAt (don't match)"
         (isLeft (popVariantAt @2 b))

   , testProperty "popVariant (match)"
         (popVariant @D (toVariantAt @4 D :: Variant '[A,B,C,B,D,E,D]) == Right D)
   , testProperty "popVariant (match)"
         (popVariant @D (toVariantAt @6 D :: Variant '[A,B,C,B,D,E,D]) == Right D)
   , testProperty "popVariant (don't match)"
         (popVariant @B (toVariantAt @4 D :: Variant '[A,B,C,B,D,E,D]) == Left (toVariantAt @2 D))

   , testProperty "prependVariant"
         (fromVariantAt @4 (prependVariant @'[D,E,F] b) == Just B)
   , testProperty "appendVariant"
         (fromVariantAt @1 (appendVariant @'[D,E,F] b)  == Just B)

   , testProperty "alterVariant"
         (alterVariant @Num (+1) (toVariant (1.0 :: Float) :: Variant '[Int,Float]) == toVariant (2.0 :: Float))
   , testProperty "alterVariant"
         (alterVariant @Num (+1) (toVariant (1.0 :: Float) :: Variant '[Float,Int]) == toVariant (2.0 :: Float))

   , testProperty "traverseVariant"
         (traverseVariant @OrdNum (\x -> if x > 1 then Just x else Nothing)
            (toVariant (2.0 :: Float) :: Variant '[Float,Int]) == Just (toVariant (2.0 :: Float)))
   , testProperty "traverseVariant"
         (traverseVariant @OrdNum (\x -> if x > 1 then Just x else Nothing)
            (toVariant (0.5 :: Float) :: Variant '[Float,Int]) == Nothing)

   , testProperty "liftVariant"
         (fromVariant (liftVariant b :: Variant '[D,A,E,B,F,C])  == Just B)
   ]

class (Ord a, Num a) => OrdNum a
instance (Ord a, Num a) => OrdNum a
