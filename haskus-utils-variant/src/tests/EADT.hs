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
-- List Height Algebra
-------------------------------

nilHeight :: Algebra NilF Int
nilHeight _ = 0

consHeight :: Algebra (ConsF a) Int
consHeight (ConsF _ b) = 1 + b

heightAlgebras :: Algebras (ListF a) Int
heightAlgebras = Algebras (nilHeight,consHeight)

heightAlgebra :: Algebra (ListF a) Int
heightAlgebra = fromAlgebras heightAlgebras

-------------------------------
-- Show AlgebraC
-------------------------------

class MyShow (f :: Type -> Type) where
   myShow :: f String -> String

instance MyShow NilF where
   myShow _ = "[]"

instance Show a => MyShow (ConsF a) where
   myShow (ConsF a b) = show a ++ " : " ++ b

showAlgebraC :: AlgebraC MyShow String
showAlgebraC = AlgebraC @MyShow myShow

showAlgebras :: Show a => Algebras (ListF a) String
showAlgebras = fromAlgebraC showAlgebraC

showAlgebra :: Show a => Algebra (ListF a) String
showAlgebra = fromAlgebras showAlgebras

-------------------------------
-- Id paramorphism
-------------------------------

nilId :: RAlgebra NilF (List a) (List a)
nilId _ = Nil

consId :: RAlgebra (ConsF a) (List a) (List a)
consId (ConsF a (t,_)) = Cons a t

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

   , testProperty "catamorphism: Algebras" $
      cata heightAlgebra list0 == 2

   , testProperty "catamorphism: AlgebraC" $
      cata showAlgebra list0 == "\"Hello\" : \"World\" : []"

   , testProperty "paramorphism: id" $
      para (fromRAlgebras (RAlgebras (nilId, consId))) list0 == list0
   ]
