{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Haskus.Tests.Utils.Solver
   ( testsSolver
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.List

import Haskus.Utils.Solver


data Predi
   = PredA
   | PredB
   | PredC
   | PredD
   | PredE
   deriving (Show,Eq,Ord)

newtype Err
   = Err String
   deriving (Show,Eq,Ord)

type C   = Constraint Err Predi
type R a = Rule Err Predi a

testsSolver :: TestTree
testsSolver = testGroup "Solver" $
   [ testProperty "Constraint reduce: CBool True"
         (constraintReduce oracleAll (CBool True) == (CBool True :: C))
   , testProperty "Constraint reduce: CBool False"
         (constraintReduce oracleAll (CBool False) == (CBool False :: C))
   , testProperty "Constraint reduce: Not False"
         (constraintReduce oracleAll (Not (CBool False)) == (CBool True :: C))
   , testProperty "Constraint reduce: Not True"
         (constraintReduce oracleAll (Not (CBool True)) == (CBool False :: C))
   , testProperty "Constraint reduce: And [True,True]"
         (constraintReduce oracleAll (And [CBool True,CBool True]) == (CBool True :: C))
   , testProperty "Constraint reduce: And [True,False]"
         (constraintReduce oracleAll (And [CBool True,CBool False]) == (CBool False :: C))
   , testProperty "Constraint reduce: Or [True,True]"
         (constraintReduce oracleAll (Or [CBool True,CBool True]) == (CBool True :: C))
   , testProperty "Constraint reduce: Or [True,False]"
         (constraintReduce oracleAll (Or [CBool True,CBool False]) == (CBool True :: C))
   , testProperty "Constraint reduce: Or [False,False]"
         (constraintReduce oracleAll (Or [CBool False,CBool False]) == (CBool False :: C))

   , testProperty "Constraint reduce: Xor [True,False,True]"
         (constraintReduce oracleAll (Xor [CBool True,CBool False,CBool True]) == (CBool False :: C))
   , testProperty "Constraint reduce: Xor [True,False,False]"
         (constraintReduce oracleAll (Xor [CBool True,CBool False,CBool False]) == (CBool True :: C))

   , testProperty "Constraint reduce: Not (Xor [True,False,False])"
         (constraintReduce oracleAll (Not (Xor [CBool True,CBool False,CBool False])) == (CBool False :: C))
   , testProperty "Constraint reduce: Not (Xor [False,False,False])"
         (constraintReduce oracleAll (Not (Xor [CBool False,CBool False,CBool False])) == (CBool True :: C))

   , testProperty "Constraint reduce: matching oracle"
         (constraintReduce oracleA (Predicate PredA) == (CBool True :: C))
   , testProperty "Constraint reduce: non matching oracle"
         (constraintReduce oracleB (Predicate PredA) == (CBool False :: C))

   , testProperty "Constraint reduce: evalsTo 0"
         (constraintReduce oracleAll (simpleRule `evalsTo` Terminal 0) == (CBool False :: C))
   , testProperty "Constraint reduce: evalsTo 1"
         (constraintReduce oracleAll (simpleRule `evalsTo` Terminal 1) == (CBool True :: C))
   , testProperty "Constraint reduce: evals to D"
         (constraintReduce oracleA (d1 `evalsTo` d1 {pInt = Terminal 0}) == (CBool True :: C))

   , testProperty "Evals to: Terminal 0"
         ((Terminal 0 `evalsTo` (Terminal 0 :: R Int)) == (CBool True :: C))
   , testProperty "Evals to: Terminal 1"
         ((Terminal 1 `evalsTo` (Terminal 0 :: R Int)) == (CBool False :: C))
   
   , testProperty "Predicated data: matching"
         (reducePredicates oracleA d1 == Match (d1 {pInt = Terminal 0}))
   , testProperty "Predicated data: not matching"
         (case reducePredicates oracleB d1 of
            NoMatch -> True
            _       -> False
         )
   , testProperty "Predicated data: failing"
         (case reducePredicates oracleC d1 of
            MatchFail _ -> True
            _           -> False
         )
   , testProperty "Predicated data: divergent"
         (case reducePredicates oracleD d1 of
            DivergentMatch xs -> sort xs == sort [d1 { pInt = Terminal 1}, d1 { pInt = Terminal 0}]
            _                 -> False
         )
   , testProperty "Predicated data: not terminal"
         (case reducePredicates oracleAE d1 of
            MatchRule _ -> True
            _           -> False
         )

   , testProperty "Ordered non terminal 0"
         (case ruleReduce oracleAB (orderedNonTerminal [(Predicate PredA, Terminal 0 :: R Int)
                                                       ,(Predicate PredB, Terminal 1)
                                                       ]) of
            Match 0 -> True
            _       -> False
         )
   , testProperty "Ordered non terminal 1"
         (case ruleReduce oracleAB (orderedNonTerminal [(Predicate PredB, Terminal 1 :: R Int)
                                                       ,(Predicate PredA, Terminal 0)
                                                       ]) of
            Match 1 -> True
            _       -> False
         )
   , testProperty "Get predicates: flat"
         (sort (getPredicates d1) == sort [PredA,PredC,PredD,PredE])

   , testProperty "Get predicates: nested"
         (sort (getPredicates d2) == sort [PredA,PredB,PredC,PredD])

   , testProperty "Create predicate table: flat non terminal"
         (case createPredicateTable d1 of
            Left _   -> False
            Right xs -> sort (fmap fst xs) == sort
                           [ [Right PredA, Left  PredC, Left  PredD, Left  PredE]
                           , [Right PredA, Left  PredC, Left  PredD, Right PredE]
                           , [Left  PredA, Left  PredC, Left  PredD, Right PredE]
                           ]
         )
   , testProperty "Create predicate table: nested non terminal"
         (case createPredicateTable d2 of
            Left _   -> False
            Right xs -> sort (fmap fst xs) == sort
                           [ [Right PredA, Left  PredB, Left  PredC, Left  PredD]
                           , [Right PredA, Right PredB, Left  PredC, Left  PredD]
                           , [Right PredA, Left  PredB, Right PredC, Left  PredD]
                           , [Right PredA, Left  PredB, Left  PredC, Right PredD]
                           , [Right PredA, Left  PredB, Right PredC, Right PredD]
                           , [Left  PredA, Right PredB, Right PredC, Left  PredD]
                           , [Left  PredA, Right PredB, Left  PredC, Right PredD]
                           ]
         )
   ]

   where
      oracleAll = const (Just True)
      oracleA   = \case
         PredA -> Just True
         _     -> Just False
      oracleB   = \case
         PredB -> Just True
         _     -> Just False
      oracleC   = \case
         PredC -> Just True
         _     -> Just False
      oracleD   = \case
         PredD -> Just True
         _     -> Just False
      oracleAE   = \case
         PredA -> Nothing
         PredD -> Nothing
         _     -> Just False
      oracleAB   = \case
         PredA -> Just True
         PredB -> Just True
         _     -> Just False

      simpleRule :: R Int
      simpleRule = NonTerminal
                     [ (CBool False, Terminal 0)
                     , (CBool True,  Terminal 1)
                     ]
      d1 = D (NonTerminal [ (Predicate PredA, Terminal 0)
                          , (Predicate PredC, Fail (Err "D doesn't support predicate C"))
                          , (Predicate PredD, Terminal 0)
                          , (Predicate PredD, Terminal 1)
                          , (Predicate PredE, Terminal 0)
                          ])
             (Terminal "Test")
      d2 = D (NonTerminal [ (Predicate PredA, Terminal 0)
                          , (Predicate PredB, NonTerminal
                              [ (Predicate PredC, Terminal 1)
                              , (Predicate PredD, Terminal 2)
                              ])
                          ])
             (Terminal "Test")

data D = D
   { pInt  :: R Int
   , _pStr :: R String
   }
   deriving (Eq,Show,Ord)

instance Predicated D where
   type Pred    D = Predi
   type PredErr D = Err

   reducePredicates fp (D a b) =
      D <$> reducePredicates fp a
        <*> reducePredicates fp b

   getTerminals (D as bs) = [ D a b | a <- getTerminals as
                                    , b <- getTerminals bs
                             ]

   getPredicates (D a b) = concat
                              [ getPredicates a
                              , getPredicates b
                              ]
