{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}

module EADT
   ( testsEADT
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Haskus.Utils.Functor
import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH
import Haskus.Utils.Types
import Data.Functor.Classes

-------------------------------
-- List EADT
-------------------------------

data ConsF a l = ConsF a l deriving (Functor)
data NilF    l = NilF      deriving (Functor)

eadtPattern 'ConsF "Cons"
eadtPattern 'NilF  "Nil"
eadtInfixPattern 'ConsF ":->"

type ListF a = VariantF '[NilF, ConsF a]
type List  a = EADT     '[NilF, ConsF a]

instance Eq a => Eq1 (ConsF a) where
   liftEq cmp (ConsF a e1) (ConsF b e2) = a == b && cmp e1 e2

instance Eq1 NilF where
   liftEq _ _ _ = True

instance Ord a => Ord1 (ConsF a) where
   liftCompare cmp (ConsF a e1) (ConsF b e2) = compare a b <> cmp e1 e2

instance Ord1 NilF where
   liftCompare _ _ _ = EQ

instance Show a => Show1 (ConsF a) where
   liftShowsPrec shw _ p (ConsF a e) =
      showParen (p >= 10) (showString "ConsF " . showsPrec 10 a . showString " " . shw 10 e)

instance Show1 NilF where
   liftShowsPrec _ _ _ _ = showString "NilF"

-- example values:
list0 :: List String
list0 = Cons "Hello" $ Cons "World" Nil

-------------------------------
-- Show AlgebraC
-------------------------------

class MyShow (f :: Type -> Type) where
   myShow :: f String -> String

instance MyShow NilF where
   myShow _ = "[]"

instance Show a => MyShow (ConsF a) where
   myShow (ConsF a b) = show a ++ " : " ++ b

showBottomUp :: Show a => BottomUpT String (ListF a)
showBottomUp = toBottomUp @MyShow myShow

-------------------------------
-- numbersTo anamorphism
-------------------------------

numbersTo :: CoAlgebra (ListF String) Int
numbersTo 0 = FV (NilF :: NilF Int)
numbersTo n = FV (ConsF (show n) (n-1))

-------------------------------
-- numbersToMin5 apomorphism
-------------------------------

numbersToMin5 :: RCoAlgebra (ListF String) (List String) Int
numbersToMin5 0 = FV (NilF :: NilF (Either (List String) Int))
numbersToMin5 n
   | n > 5     = FV (ConsF (show n) (Right (n-1) :: Either (List String) Int))
   | otherwise = FV (ConsF "min" (Left (Nil :: List String) :: Either (List String) Int))

-------------------------------
-- Tests
-------------------------------

testsEADT :: TestTree
testsEADT = testGroup "EADT" $
   [ testProperty "eadtPattern: match" $
      case list0 of
         Cons (x :: String) _ -> x == "Hello"
         _                    -> False

   , testProperty "eadtInfixPattern: match" $
      case list0 of
         (x :: String) :-> _ -> x == "Hello"
         _                   -> False

   , testProperty "catamorphism: constraint" $
      cata showBottomUp list0 == "\"Hello\" : \"World\" : []"

   , testProperty "anamorphism: numbersTo" $
      ana numbersTo 5 == Cons "5" (Cons "4" (Cons "3" (Cons "2" (Cons "1" Nil))))

   , testProperty "apomorphism: numbersToMin5" $
      apo numbersToMin5 8 == Cons "8" (Cons "7" (Cons "6" (Cons "min" Nil)))
   ]
