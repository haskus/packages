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

import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH
import Haskus.Utils.Types

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
-- Show AlgebraC
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
   ]
