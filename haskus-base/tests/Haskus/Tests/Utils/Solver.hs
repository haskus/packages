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
import Data.Kind (Type)
import qualified Data.Set as Set

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

type family RuleT e p a s :: Type where
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

   simplifyPredicates oracle (PD a b) =
      PD (simplifyPredicates oracle a)
         (simplifyPredicates oracle b)

   getTerminals (PD as bs) = Set.fromList
                              [ PD a b
                              | a <- Set.toList (getTerminals as)
                              , b <- Set.toList (getTerminals bs)
                              ]

   getPredicates (PD a b) = Set.union (getPredicates a) (getPredicates b)

testsSolver :: TestTree
testsSolver = testGroup "Solver" $
   [ testProperty "Constraint reduce: CBool True"
         (constraintSimplify oracleAll (CBool True) == (CBool True :: C))
   , testProperty "Constraint reduce: CBool False"
         (constraintSimplify oracleAll (CBool False) == (CBool False :: C))
   , testProperty "Constraint reduce: Not False"
         (constraintSimplify oracleAll (Not (CBool False)) == (CBool True :: C))
   , testProperty "Constraint reduce: Not True"
         (constraintSimplify oracleAll (Not (CBool True)) == (CBool False :: C))
   , testProperty "Constraint reduce: And [True,True]"
         (constraintSimplify oracleAll (And [CBool True,CBool True]) == (CBool True :: C))
   , testProperty "Constraint reduce: And [True,False]"
         (constraintSimplify oracleAll (And [CBool True,CBool False]) == (CBool False :: C))
   , testProperty "Constraint reduce: Or [True,True]"
         (constraintSimplify oracleAll (Or [CBool True,CBool True]) == (CBool True :: C))
   , testProperty "Constraint reduce: Or [True,False]"
         (constraintSimplify oracleAll (Or [CBool True,CBool False]) == (CBool True :: C))
   , testProperty "Constraint reduce: Or [False,False]"
         (constraintSimplify oracleAll (Or [CBool False,CBool False]) == (CBool False :: C))

   , testProperty "Constraint reduce: Xor [True,False,True]"
         (constraintSimplify oracleAll (Xor [CBool True,CBool False,CBool True]) == (CBool False :: C))
   , testProperty "Constraint reduce: Xor [True,False,False]"
         (constraintSimplify oracleAll (Xor [CBool True,CBool False,CBool False]) == (CBool True :: C))

   , testProperty "Constraint reduce: Not (Xor [True,False,False])"
         (constraintSimplify oracleAll (Not (Xor [CBool True,CBool False,CBool False])) == (CBool False :: C))
   , testProperty "Constraint reduce: Not (Xor [False,False,False])"
         (constraintSimplify oracleAll (Not (Xor [CBool False,CBool False,CBool False])) == (CBool True :: C))

   , testProperty "Constraint reduce: matching oracle"
         (constraintSimplify oracleA (Predicate PredA) == (CBool True :: C))
   , testProperty "Constraint reduce: non matching oracle"
         (constraintSimplify oracleB (Predicate PredA) == (CBool False :: C))

   , testProperty "Constraint reduce: evalsTo 0"
         (constraintSimplify oracleAll (simpleRule `evalsTo` 0) == (CBool False :: C))
   , testProperty "Constraint reduce: evalsTo 1"
         (constraintSimplify oracleAll (simpleRule `evalsTo` 1) == (CBool True :: C))
   , testProperty "Constraint reduce: evals to D"
         (constraintSimplify oracleA (d1 `evalsTo` PD 0 "Test") == (CBool True :: C))

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
         (case reducePredicates oracleAB (OrderedNonTerminal [(Predicate PredA, Terminal 0 :: R Int NT)
                                                             ,(Predicate PredB, Terminal 1)
                                                             ]) of
            Match 0 -> True
            _       -> False
         )
   , testProperty "Ordered non terminal 1"
         (case reducePredicates oracleAB (OrderedNonTerminal [(Predicate PredB, Terminal 1 :: R Int NT)
                                                             ,(Predicate PredA, Terminal 0)
                                                             ]) of
            Match 1 -> True
            _       -> False
         )
   , testProperty "Get predicates: flat"
         (getPredicates d1 == Set.fromList [PredA,PredC,PredD,PredE])

   , testProperty "Get predicates: nested"
         (getPredicates d2 == Set.fromList [PredA,PredB,PredC,PredD])

   , testProperty "Create predicate table: flat non terminal"
         (case createPredicateTable d1 (const True) of
            Left _   -> False
            Right xs -> sort (fmap (oraclePredicates . fst) xs) == sort
                           [ [(PredA, SetPred)  , (PredC, UnsetPred), (PredD, UnsetPred), (PredE, UnsetPred)]
                           , [(PredA, SetPred)  , (PredC, UnsetPred), (PredD, UnsetPred), (PredE, SetPred)]
                           , [(PredA, UnsetPred), (PredC, UnsetPred), (PredD, UnsetPred), (PredE, SetPred)]
                           ]
         )
   , testProperty "Create predicate table: nested non terminal"
         (case createPredicateTable d2 (const True) of
            Left _   -> False
            Right xs -> sort (fmap (oraclePredicates . fst) xs) == sort
                  [[(PredA,SetPred),(PredB,SetPred),(PredC,UnsetPred),(PredD,UnsetPred)]
                  ,[(PredA,SetPred),(PredB,UnsetPred)]
                  ,[(PredA,SetPred),(PredB,UnsetPred),(PredC,SetPred)]
                  ,[(PredA,SetPred),(PredB,UnsetPred),(PredC,SetPred),(PredD,SetPred)]
                  ,[(PredA,SetPred),(PredB,UnsetPred),(PredC,SetPred),(PredD,UnsetPred)]
                  ,[(PredA,SetPred),(PredB,UnsetPred),(PredC,UnsetPred)]
                  ,[(PredA,SetPred),(PredB,UnsetPred),(PredC,UnsetPred),(PredD,SetPred)]
                  ,[(PredA,SetPred),(PredB,UnsetPred),(PredC,UnsetPred),(PredD,UnsetPred)]
                  ,[(PredA,SetPred),(PredB,UnsetPred),(PredD,SetPred)]
                  ,[(PredA,SetPred),(PredB,UnsetPred),(PredD,UnsetPred)]
                  ,[(PredA,UnsetPred),(PredB,SetPred),(PredC,SetPred),(PredD,UnsetPred)]
                  ,[(PredA,UnsetPred),(PredB,SetPred),(PredC,UnsetPred),(PredD,SetPred)]
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
