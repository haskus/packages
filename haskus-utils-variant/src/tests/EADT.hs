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

data ConsF a l = ConsF a l deriving (Functor)
data NilF    l = NilF      deriving (Functor)

eadtPattern 'ConsF "Cons"
eadtPattern 'NilF  "Nil"
eadtInfixPattern 'ConsF ":->"

type List a = EADT '[ConsF a, NilF]

list0 :: List String
list0 = Cons "Hello" $ Cons "World" Nil

testsEADT :: TestTree
testsEADT = testGroup "EADT" $
   [ testProperty "eadtPattern: match"              $ case list0 of
                                                         Cons (x :: String) _ -> x == "Hello"
                                                         _                    -> False
   , testProperty "eadtInfixPattern: match"         $ case list0 of
                                                         (x :: String) :-> _ -> x == "Hello"
                                                         _                   -> False

   ]
