{-# LANGUAGE ScopedTypeVariables #-}

module Haskus.Tests.Utils.Solver
   ( testsSolver
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Haskus.Utils.Solver

type C = Constraint Bool Int ()

testsSolver :: TestTree
testsSolver = testGroup "Solver" $
   [ testProperty "Constraint reduce: CBool True"
         (constraintReduce predTrue (CBool True) == (CBool True :: C))
   , testProperty "Constraint reduce: CBool False"
         (constraintReduce predTrue (CBool False) == (CBool False :: C))
   , testProperty "Constraint reduce: Not False"
         (constraintReduce predTrue (Not (CBool False)) == (CBool True :: C))
   , testProperty "Constraint reduce: Not True"
         (constraintReduce predTrue (Not (CBool True)) == (CBool False :: C))
   , testProperty "Constraint reduce: And [True,True]"
         (constraintReduce predTrue (And [CBool True,CBool True]) == (CBool True :: C))
   , testProperty "Constraint reduce: And [True,False]"
         (constraintReduce predTrue (And [CBool True,CBool False]) == (CBool False :: C))
   , testProperty "Constraint reduce: Or [True,True]"
         (constraintReduce predTrue (Or [CBool True,CBool True]) == (CBool True :: C))
   , testProperty "Constraint reduce: Or [True,False]"
         (constraintReduce predTrue (Or [CBool True,CBool False]) == (CBool True :: C))
   , testProperty "Constraint reduce: Or [False,False]"
         (constraintReduce predTrue (Or [CBool False,CBool False]) == (CBool False :: C))
   , testProperty "Constraint reduce: Predicate True"
         (constraintReduce predId (Predicate True) == (CBool True :: C))
   , testProperty "Constraint reduce: Predicate False"
         (constraintReduce predId (Predicate False) == (CBool False :: C))
   , testProperty "Constraint reduce: RuleEval 0"
         (constraintReduce predTrue (RuleEval simpleRule 0) == (CBool False :: C))
   , testProperty "Constraint reduce: RuleEval 1"
         (constraintReduce predTrue (RuleEval simpleRule 1) == (CBool True :: C))
   ]

   where
      predTrue = const (Just True)
      predId   = Just
      simpleRule = NonTerminal
                     [ (CBool False, Terminal 0)
                     , (CBool True,  Terminal 1)
                     ]
