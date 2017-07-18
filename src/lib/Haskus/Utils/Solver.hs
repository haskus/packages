{-# LANGUAGE LambdaCase #-}

-- | Simple Constraint solver
module Haskus.Utils.Solver
   ( Constraint (..)
   , Rule (..)
   , constraintReduce
   , ruleReduce
   , MatchResult (..)
   )
where

import Haskus.Utils.Maybe
import Haskus.Utils.Flow
import Haskus.Utils.List

import Control.Arrow (first)

import Prelude hiding (pred)

data Constraint p a e
   = Predicate p
   | Not (Constraint p a e)
   | And [Constraint p a e]
   | Or [Constraint p a e]
   | CBool Bool
   | RuleEval (Rule p a e) a
   deriving (Show,Eq,Ord)

data Rule p a e
   = Terminal a
   | NonTerminal [(Constraint p a e, Rule p a e)]
   | Fail e
   deriving (Show,Eq,Ord)

-- | Reduce a constraint
constraintReduce :: (Eq p, Eq a, Eq e) => (p -> Maybe Bool) -> Constraint p a e -> Constraint p a e
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
constraintIsBool :: Bool -> Constraint p a e -> Bool
constraintIsBool v (CBool v') = v == v'
constraintIsBool _ _          = False


-- | Result of a rule reduction
data MatchResult p a e
   = NoMatch                -- ^ No rule leads to a terminal
   | DivergentMatch [a]     -- ^ Several rules match but they return different terminals
   | MatchFail [e]          -- ^ Some matching rules fail
   | Match a                -- ^ A single terminal value is returned
   | MatchRule (Rule p a e) -- ^ The rule may have been reduced but didn't produce a result
   deriving (Show,Eq,Ord)

-- | Reduce a rule
ruleReduce :: (Eq e, Eq p, Eq a) => (p -> Maybe Bool) -> Rule p a e -> MatchResult p a e
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


