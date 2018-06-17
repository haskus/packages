{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Haskus.Tests.Utils.Solver
   ( testsSolver
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.List

import Haskus.Utils.Solver
import Haskus.Utils.Flow


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

type C     = Constraint Err Predi
type R a t = RuleT Err Predi a t

data T
data NT

type family RuleT e p a s :: * where
   RuleT e p a T   = a
   RuleT e p a NT  = Rule e p a

data PD t = PD
   { pInt  :: R Int t
   , _pStr :: R String t
   }

deriving instance Eq (PD T)
deriving instance Show (PD T)
deriving instance Ord (PD T)
deriving instance Eq (PD NT)
deriving instance Show (PD NT)
deriving instance Ord (PD NT)

instance Predicated (PD NT) where
   type PredErr (PD NT)  = Err
   type Pred (PD NT)     = Predi
   type PredTerm (PD NT) = PD T

   liftTerminal (PD a b) = PD (liftTerminal a) (liftTerminal b)

   reducePredicates oracle (PD a b) =
      initP PD PD
         |> (`applyP` reducePredicates oracle a)
         |> (`applyP` reducePredicates oracle b)
         |> resultP

   getTerminals (PD as bs) = [ PD a b | a <- getTerminals as
                                      , b <- getTerminals bs
                             ]

   getPredicates (PD a b) = concat
                              [ getPredicates a
                              , getPredicates b
                              ]

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
         (constraintReduce oracleAll (simpleRule `evalsTo` 0) == (CBool False :: C))
   , testProperty "Constraint reduce: evalsTo 1"
         (constraintReduce oracleAll (simpleRule `evalsTo` 1) == (CBool True :: C))
   , testProperty "Constraint reduce: evals to D"
         (constraintReduce oracleA (d1 `evalsTo` PD 0 "Test") == (CBool True :: C))

   , testProperty "Evals to: Terminal 0"
         (((Terminal 0 :: R Int NT) `evalsTo` 0) == (CBool True :: C))
   , testProperty "Evals to: Terminal 1"
         (((Terminal 1 :: R Int NT) `evalsTo` 0) == (CBool False :: C))
   
   , testProperty "Predicated data: matching"
         (reducePredicates oracleA d1 == Match (PD 0 "Test"))
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
            MatchDiverge xs -> sort xs == sort [d1 { pInt = Terminal 1}, d1 { pInt = Terminal 0}]
            _               -> False
         )
   , testProperty "Predicated data: not terminal"
         (case reducePredicates oracleAE d1 of
            DontMatch _ -> True
            _           -> False
         )

   , testProperty "Ordered non terminal 0"
         (case reducePredicates oracleAB (orderedNonTerminal [(Predicate PredA, Terminal 0 :: R Int NT)
                                                             ,(Predicate PredB, Terminal 1)
                                                             ]) of
            Match 0 -> True
            _       -> False
         )
   , testProperty "Ordered non terminal 1"
         (case reducePredicates oracleAB (orderedNonTerminal [(Predicate PredB, Terminal 1 :: R Int NT)
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
         (case createPredicateTable d1 (const True) False of
            Left _   -> False
            Right xs -> sort (fmap (oraclePredicates . fst) xs) == sort
                           [ [(PredA, SetPred)  , (PredC, UnsetPred), (PredD, UnsetPred), (PredE, UnsetPred)]
                           , [(PredA, SetPred)  , (PredC, UnsetPred), (PredD, UnsetPred), (PredE, SetPred)]
                           , [(PredA, UnsetPred), (PredC, UnsetPred), (PredD, UnsetPred), (PredE, SetPred)]
                           ]
         )
   , testProperty "Create predicate table: nested non terminal"
         (case createPredicateTable d2 (const True) False of
            Left _   -> False
            Right xs -> sort (fmap (oraclePredicates . fst) xs) == sort
                  [ [(PredA,SetPred),(PredB,SetPred),(PredC,UnsetPred),(PredD,UnsetPred)]
                  , [(PredA,SetPred),(PredB,UnsetPred),(PredC,SetPred),(PredD,SetPred)]
                  , [(PredA,SetPred),(PredB,UnsetPred),(PredC,SetPred),(PredD,UnsetPred)]
                  , [(PredA,SetPred),(PredB,UnsetPred),(PredC,UnsetPred),(PredD,SetPred)]
                  , [(PredA,SetPred),(PredB,UnsetPred),(PredC,UnsetPred),(PredD,UnsetPred)]
                  , [(PredA,UnsetPred),(PredB,SetPred),(PredC,SetPred),(PredD,UnsetPred)]
                  , [(PredA,UnsetPred),(PredB,SetPred),(PredC,UnsetPred),(PredD,SetPred)]
                  , [(PredA,SetPred),(PredB,UnsetPred),(PredD,SetPred)]
                  , [(PredA,SetPred),(PredB,UnsetPred),(PredD,UnsetPred)]
                  , [(PredA,SetPred),(PredB,UnsetPred),(PredC,SetPred)]
                  , [(PredA,SetPred),(PredB,UnsetPred),(PredC,UnsetPred)]
                  , [(PredA,SetPred),(PredB,UnsetPred)]
                  ]
         )

   , testProperty "Create full predicate table: nested non terminal"
         (case createPredicateTable d2 (const True) True of
            Left _   -> False
            Right xs -> sort (fmap (oraclePredicates . fst) xs) == sort
                  [ [(PredA,SetPred),(PredB,UnsetPred),(PredC,SetPred),(PredD,SetPred)]
                  , [(PredA,UnsetPred),(PredB,SetPred),(PredC,UnsetPred),(PredD,SetPred)]
                  , [(PredA,SetPred),(PredB,UnsetPred),(PredC,UnsetPred),(PredD,SetPred)]
                  , [(PredA,UnsetPred),(PredB,SetPred),(PredC,SetPred),(PredD,UnsetPred)]
                  , [(PredA,SetPred),(PredB,UnsetPred),(PredC,SetPred),(PredD,UnsetPred)]
                  , [(PredA,SetPred),(PredB,SetPred),(PredC,UnsetPred),(PredD,UnsetPred)]
                  , [(PredA,SetPred),(PredB,UnsetPred),(PredC,UnsetPred),(PredD,UnsetPred)]
                  ]
         )
   ]

   where
      oracleAll = makeOracle (fmap (,SetPred) [PredA,PredB,PredC,PredD,PredE])
      oracleA   = makeOracle ((PredA,SetPred) : fmap (,UnsetPred) [PredB,PredC,PredD,PredE])
      oracleB   = makeOracle ((PredB,SetPred) : fmap (,UnsetPred) [PredA,PredC,PredD,PredE])
      oracleC   = makeOracle ((PredC,SetPred) : fmap (,UnsetPred) [PredA,PredB,PredD,PredE])
      oracleD   = makeOracle ((PredD,SetPred) : fmap (,UnsetPred) [PredA,PredB,PredC,PredE])
      oracleAE  = makeOracle ((PredA,UndefPred) : (PredD,UndefPred) : fmap (,UnsetPred) [PredB,PredC,PredE])
      oracleAB  = makeOracle ((PredA,SetPred) : (PredB,SetPred) : fmap (,UnsetPred) [PredC,PredD,PredE])

      simpleRule :: R Int NT
      simpleRule = NonTerminal
                     [ (CBool False, Terminal 0)
                     , (CBool True,  Terminal 1)
                     ]
      d1 :: PD NT
      d1 = PD (NonTerminal [ (Predicate PredA, Terminal 0)
                          , (Predicate PredC, Fail (Err "D doesn't support predicate C"))
                          , (Predicate PredD, Terminal 0)
                          , (Predicate PredD, Terminal 1)
                          , (Predicate PredE, Terminal 0)
                          ])
             (Terminal "Test")
      d2 :: PD NT
      d2 = PD (NonTerminal [ (Predicate PredA, Terminal 0)
                          , (Predicate PredB, NonTerminal
                              [ (Predicate PredC, Terminal 1)
                              , (Predicate PredD, Terminal 2)
                              ])
                          ])
             (Terminal "Test")
