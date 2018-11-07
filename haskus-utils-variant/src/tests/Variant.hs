{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Variant
   ( testsVariant
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.Either

import Haskus.Utils.Variant
import Haskus.Utils.ContFlow

data A = A deriving (Show,Eq)
data B = B deriving (Show,Eq)
data C = C deriving (Show,Eq)
data D = D deriving (Show,Eq)
data E = E deriving (Show,Eq)
data F = F deriving (Show,Eq)

type ABC = V '[A,B,C]
type DEF = V '[D,E,F]

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
   [ testProperty "get by index (match)"              $ fromVariantAt @1 b == Just B
   , testProperty "get by index (dont' match)"        $ fromVariantAt @0 b == Nothing
   , testProperty "pattern V: set"                    $ (V A :: ABC) == (toVariant A :: ABC)
   , testProperty "pattern V: match"                  $ case (V A :: ABC) of
                                                         V (x :: A) -> x == A
                                                         V (_ :: B) -> False
                                                         V (_ :: C) -> False
                                                         _          -> undefined
   , testProperty "pattern V: match2"                 $ case (V B :: ABC) of
                                                         V (_ :: A) -> False
                                                         V (x :: B) -> x == B
                                                         V (_ :: C) -> False
                                                         _          -> undefined
   , testProperty "pattern V: type application"       $ (V @Float 1.0 :: V '[Int,Float,String]) == toVariantAt @1 1.0
   , testProperty "get by type (match)"               $ fromVariant    (V B :: ABC) == Just B
   , testProperty "get by type (don't match)"         $ fromVariant @C (V B :: ABC) == Nothing
   , testProperty "variant equality (match)"          $ b == b
   , testProperty "variant equality (don't match)"    $ b /= V C
   , testProperty "update by index (match)"           $ mapVariantAt @1 (const D) b == toVariantAt @1 D
   , testProperty "update by index (don't match)"     $ mapVariantAt @0 (const F) b == toVariantAt @1 B
   , testProperty "update by type (match)"            $ mapVariantFirst b2d b == toVariantAt @1 D
   , testProperty "update by type (don't match)"      $ mapVariantFirst c2d b == V B
   , testProperty "update/fold by index (match)"      $ foldMapVariantAt @1 b2def b == V E
   , testProperty "update/fold by index (don't match)"$ foldMapVariantAt @2 c2def b == V B
   , testProperty "Convert into tuple"                $ variantToTuple b == (Nothing, Just B, Nothing)
   , testProperty "Convert single variant"            $ variantToValue (V A :: V '[A]) == A
   , testProperty "Lift Either: Left"                 $ variantFromEither (Left A :: Either A B) == V A
   , testProperty "Lift Either: Right"                $ variantFromEither (Right B :: Either A B) == V B
   , testProperty "To Either: Left"                   $ variantToEither (V B :: V '[A,B]) == Left B
   , testProperty "To Either: Right"                  $ variantToEither (V A :: V '[A,B]) == Right A
   , testProperty "popVariantHead (match)"            $ popVariantHead (V A :: ABC) == Right A
   , testProperty "popVariantHead (don't match)"      $ isLeft (popVariantHead b)
   , testProperty "popVariantAt (match)"              $ popVariantAt @1 b == Right B
   , testProperty "popVariantAt (don't match)"        $ isLeft (popVariantAt @2 b)

   , testProperty "popVariant (match)"                $ popVariant @D (toVariantAt @4 D :: V '[A,B,C,B,D,E,D]) == Right D
   , testProperty "popVariant (match)"                $ popVariant @D (toVariantAt @6 D :: V '[A,B,C,B,D,E,D]) == Right D
   , testProperty "popVariant (don't match)"          $ popVariant @B (toVariantAt @4 D :: V '[A,B,C,B,D,E,D]) == Left (toVariantAt @2 D)

   , testProperty "prependVariant"                    $ fromVariantAt @4 (prependVariant @'[D,E,F] b) == Just B
   , testProperty "appendVariant"                     $ fromVariantAt @1 (appendVariant @'[D,E,F] b)  == Just B

   , testProperty "alterVariant"                      $ alterVariant @Num (+1) (V @Float 1.0 :: V '[Int,Float]) == V @Float 2.0
   , testProperty "alterVariant"                      $ alterVariant @Num (+1) (V @Float 1.0 :: V '[Float,Int]) == V @Float 2.0

   , testProperty "traverseVariant"                   $ traverseVariant @OrdNum (\x -> if x > 1 then Just x else Nothing)
                                                            (V @Float 2.0 :: V '[Float,Int]) == Just (V @Float 2.0)
   , testProperty "traverseVariant"                   $ traverseVariant @OrdNum (\x -> if x > 1 then Just x else Nothing)
                                                            (V @Float 0.5 :: V '[Float,Int]) == Nothing
   , testProperty "liftVariant"                       $ fromVariant (liftVariant b :: V '[D,A,E,B,F,C])  == Just B
   , testProperty "splitVariant"                      $ case splitVariant @'[A,C,D] (V A :: V '[A,B,C,D,E,F]) of
                                                            Right (x :: V '[A,C,D]) -> x == V A
                                                            Left  (_ :: V '[B,E,F]) -> True
   , testProperty "splitVariant2"                     $ case splitVariant @'[A,C,D] (V E :: V '[A,B,C,D,E,F]) of
                                                            Right (_ :: V '[A,C,D]) -> True
                                                            Left  (y :: V '[B,E,F]) -> y == V E
   , testProperty "toCont"                            $ (toCont (V E :: V '[A,B,C,D,E,F]) >::>
                                                            ( \(_ :: A) -> False
                                                            , \(_ :: B) -> False
                                                            , \(_ :: C) -> False
                                                            , \(_ :: D) -> False
                                                            , \(_ :: E) -> True
                                                            , \(_ :: F) -> False
                                                            ))

   ]

class (Ord a, Num a) => OrdNum a
instance (Ord a, Num a) => OrdNum a
