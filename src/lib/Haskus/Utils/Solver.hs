{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Simple Constraint solver
module Haskus.Utils.Solver
   ( Constraint (..)
   , Rule (..)
   , constraintReduce
   , ruleReduce
   , MatchResult (..)
   , getRuleTerminals
   , getRulePredicates
   , getConstraintPredicates
   , Predicated (..)
   , PredResult (..)
   , initP
   , composeP
   , extractP
   )
where

import Haskus.Utils.Maybe
import Haskus.Utils.Flow
import Haskus.Utils.List

import Control.Arrow (first)

import Prelude hiding (pred)

data Constraint p e a
   = Predicate p
   | Not (Constraint p e a)
   | And [Constraint p e a]
   | Or [Constraint p e a]
   | CBool Bool
   | RuleEval (Rule p e a) a
   deriving (Show,Eq,Ord)

data Rule p e a
   = Terminal a
   | NonTerminal [(Constraint p e a, Rule p e a)]
   | Fail e
   deriving (Show,Eq,Ord)

-- | Reduce a constraint
constraintReduce :: (Eq p, Eq a, Eq e) => (p -> Maybe Bool) -> Constraint p e a -> Constraint p e a
constraintReduce pred c = case c of
   Predicate p  -> case pred p of
                      Nothing -> c
                      Just v  -> CBool v
   Not c'       -> case constraintReduce pred c' of
                      CBool v -> CBool (not v)
                      c''     -> Not c''
   And cs       -> case reduceFilter True cs of
                      []                                     -> CBool True
                      cs' | any (constraintIsBool False) cs' -> CBool False
                      [c']                                   -> c'
                      cs'                                    -> And cs'
   Or cs        -> case reduceFilter False cs of
                      []                                    -> CBool False
                      cs' | any (constraintIsBool True) cs' -> CBool True
                      [c']                                  -> c'
                      cs'                                   -> Or cs'
   CBool _      -> c

   RuleEval r a -> case ruleReduce pred r of
                     Match b          -> CBool (a == b)
                     MatchRule r'     -> RuleEval r' a
                     NoMatch          -> CBool False
                     DivergentMatch _ -> CBool False
                     MatchFail _      -> CBool False

   where
      reduceFilter v = filter (not . constraintIsBool v) . fmap (constraintReduce pred)

-- | Check that a constraint is evaluated to a given boolean value
constraintIsBool :: Bool -> Constraint p e a -> Bool
constraintIsBool v (CBool v') = v == v'
constraintIsBool _ _          = False


-- | Result of a rule reduction
data MatchResult p e a
   = NoMatch                -- ^ No rule leads to a terminal
   | DivergentMatch [a]     -- ^ Several rules match but they return different terminals
   | MatchFail [e]          -- ^ Some matching rules fail
   | Match a                -- ^ A single terminal value is returned
   | MatchRule (Rule p e a) -- ^ The rule may have been reduced but didn't produce a result
   deriving (Show,Eq,Ord)


-- | Reduce a rule
ruleReduce :: (Eq e, Eq p, Eq a) => (p -> Maybe Bool) -> Rule p e a -> MatchResult p e a
ruleReduce pred r = case r of
   Terminal a     -> Match a
   Fail e         -> MatchFail [e]
   NonTerminal rs -> 
      let
         rs' = rs
               -- reduce constraints
               |> fmap (first (constraintReduce pred))
               -- filter non matching rules
               |> filter (not . constraintIsBool False . fst)

         (matchingRules,mayMatchRules) = partition (constraintIsBool True . fst) rs'
         matchingResults               = nub $ fmap snd $ matchingRules


         (failingResults,terminalResults,nonTerminalResults) = go [] [] [] matchingResults
         go fr tr ntr = \case
            []                 -> (fr,tr,ntr)
            (Fail x:xs)        -> go (x:fr) tr ntr xs
            (Terminal x:xs)    -> go fr (x:tr) ntr xs
            (NonTerminal x:xs) -> go fr tr (x:ntr) xs

         divergence = case terminalResults of
            -- results are already "nub"ed.
            -- More than 1 results => divergence
            (_:_:_) -> True
            _       -> False
      in
      case rs' of
         []                                 -> NoMatch
         _  | not (null failingResults)     -> MatchFail failingResults
            | divergence                    -> DivergentMatch terminalResults
            | not (null nonTerminalResults) ->
               -- fold matching nested NonTerminals
               ruleReduce pred
                  <| NonTerminal 
                  <| (fmap (\x -> (CBool True, Terminal x)) terminalResults
                      ++ mayMatchRules
                      ++ concat nonTerminalResults)

            | otherwise                     ->
               case (matchingResults,mayMatchRules) of
                  ([Terminal a], [])    -> Match a
                  _                     -> MatchRule (NonTerminal rs')


-- | Get possible resulting terminals
getRuleTerminals :: Rule p e a -> [a]
getRuleTerminals (Fail _)         = []
getRuleTerminals (Terminal a)     = [a]
getRuleTerminals (NonTerminal xs) = concatMap (getRuleTerminals . snd) xs

-- | Get predicates used in a rule
getRulePredicates :: Rule p e a -> [p]
getRulePredicates (Fail _)         = []
getRulePredicates (Terminal _)     = []
getRulePredicates (NonTerminal xs) = concatMap (getConstraintPredicates . fst) xs

-- | Get predicates used in a constraint
getConstraintPredicates :: Constraint p e a -> [p]
getConstraintPredicates = \case
   Predicate p  -> [p]
   Not c        -> getConstraintPredicates c
   And cs       -> concatMap getConstraintPredicates cs
   Or  cs       -> concatMap getConstraintPredicates cs
   CBool _      -> []
   RuleEval r _ -> getRulePredicates r

-- | Result of a predicate reduction
data PredResult p e a b
   = PredNoMatch   -- ^ No rule leads to a terminal
   | PredDivergent -- ^ Several rules match but they diverge
   | PredFail [e]  -- ^ Some matching rules fail
   | PredMatch a b -- ^ A single terminal value is returned
   | PredCont a    -- ^ Some predicates remain
   deriving (Show,Eq,Ord)

instance Functor (PredResult p e a) where
   fmap f x = case x of
      PredNoMatch   -> PredNoMatch
      PredDivergent -> PredDivergent
      PredFail es   -> PredFail es
      PredMatch a b -> PredMatch a (f b)
      PredCont a    -> PredCont a

-- | A predicated data type reducer
--
-- Example:
--
-- @
-- data PD p e = PD
--    { pA :: Rule p e Int
--    , pB :: Rule p e String
--    }
-- 
-- data UD = UD
--    { uA :: Int
--    , uB :: String
--    }
-- 
-- instance Predicated p e (PD p e) UD where
--    reducePredicates fp (PD a b) = 
--       initP PD UD
--          |> composeP fp a
--          |> composeP fp b
--          |> extractP
-- @
--
class Predicated p e a b where
   reducePredicates :: (p -> Maybe Bool) -> a -> PredResult p e a b

instance (Eq p, Eq a, Eq e) => Predicated p e (Rule p e a) a where
   reducePredicates fp r = case ruleReduce fp r of
      NoMatch           -> PredNoMatch
      DivergentMatch _  -> PredDivergent
      MatchFail es      -> PredFail es
      Match b           -> PredMatch (Terminal b) b
      MatchRule a       -> PredCont a


type P p e a b t1 t2 = Either (PredResult p e a b) (Maybe t1,t2)

-- | Initialize a P
--
-- Typically pass your unpredicated data (UD) and your predicated data (PD) such
-- as: initP PD UD
initP :: t1 -> t2 -> P p e a b t2 t1
initP t1 t2 = Right (Just t2,t1)

-- | Compose some P's
composeP ::
   ( Predicated p e x y 
   ) => (p -> Maybe Bool) -> x -> P p e a b (y -> t1) (x -> t2) -> P p e a b t1 t2
composeP _ _  (Left r)                             = Left r
composeP fp x (Right (retTerminal,retNonTerminal)) = 
   case reducePredicates fp x of
      PredNoMatch    -> Left PredNoMatch
      PredDivergent  -> Left PredDivergent
      PredFail es    -> Left $ PredFail es
      PredCont a'    -> Right $ (Nothing, retNonTerminal a')
      PredMatch nt t -> Right $ (fmap ($ t) retTerminal, retNonTerminal nt)

-- | Extract the resulting P result
extractP :: P p e a b b a -> PredResult p e a b
extractP (Left r)              = r
extractP (Right (Nothing,rnt)) = PredCont rnt
extractP (Right (Just rt,rnt)) = PredMatch rnt rt
