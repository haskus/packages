{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

-- | Simple Constraint solver
module Haskus.Utils.Solver
   ( Constraint (..)
   , simplifyConstraint
   , Rule (..)
   , orderedNonTerminal
   , mergeRules
   , constraintReduce
   , ruleReduce
   , MatchResult (..)
   , getRuleTerminals
   , getRulePredicates
   , getConstraintPredicates
   , mergeMatchResults
   , Predicated (..)
   , evalsTo
   , createPredicateTable
   )
where

import Haskus.Utils.Maybe
import Haskus.Utils.Flow
import Haskus.Utils.List

import Data.Bits
import Control.Arrow (first,second)

import Prelude hiding (pred)

data Constraint e p
   = Predicate p
   | Not (Constraint e p)
   | And [Constraint e p]
   | Or  [Constraint e p]
   | Xor [Constraint e p]
   | CBool Bool
   deriving (Show,Eq,Ord)

instance Functor (Constraint e) where
   fmap f (Predicate p)  = Predicate (f p)
   fmap _ (CBool b)      = CBool b
   fmap f (Not c)        = Not (fmap f c)
   fmap f (And cs)       = And (fmap (fmap f) cs)
   fmap f (Or cs)        = Or (fmap (fmap f) cs)
   fmap f (Xor cs)       = Xor (fmap (fmap f) cs)

data Rule e p a
   = Terminal a
   | NonTerminal [(Constraint e p, Rule e p a)]
   | Fail e
   deriving (Show,Eq,Ord)

instance Functor (Rule e p) where
   fmap f (Terminal a)     = Terminal (f a)
   fmap f (NonTerminal xs) = NonTerminal (fmap (second (fmap f)) xs)
   fmap _ (Fail e)         = Fail e

-- | NonTerminal whose constraints are evaluated in order
--
-- Earlier constraints must be proven false for the next ones to be considered
orderedNonTerminal :: [(Constraint e p, Rule e p a)] -> Rule e p a
orderedNonTerminal = NonTerminal . go []
   where
      go _  []          = []
      go [] ((c,r):xs)  = (simplifyConstraint c,r) : go [c] xs
      go cs ((c,r):xs)  = (simplifyConstraint (And (c:fmap Not cs)),r) : go (c:cs) xs

-- | Simplify a constraint
simplifyConstraint :: Constraint e p -> Constraint e p
simplifyConstraint x = case x of
   Predicate _       -> x
   CBool _           -> x
   Not (Predicate _) -> x
   Not (CBool v)     -> CBool (not v)
   Not (Not c)       -> simplifyConstraint c
   Not (Or cs)       -> simplifyConstraint (And (fmap Not cs))
   Not (And cs)      -> simplifyConstraint (Or (fmap Not cs))
   Not (Xor cs)      -> case simplifyConstraint (Xor cs) of
                           Xor cs' -> Not (Xor cs')
                           r       -> simplifyConstraint (Not r)
   And [c]           -> simplifyConstraint c
   Or  [c]           -> simplifyConstraint c
   Xor [c]           -> let c' = simplifyConstraint c
                        in if | constraintIsBool True c'  -> CBool True
                              | constraintIsBool False c' -> CBool False
                              | otherwise                 -> c'
   And cs            -> let cs' = fmap simplifyConstraint cs
                        in if any (constraintIsBool False) cs'
                             then CBool False
                             else And cs'
   Or cs             -> let cs' = fmap simplifyConstraint cs
                        in if any (constraintIsBool True) cs'
                             then CBool True
                             else Or cs'
   Xor cs            -> let cs'        = fmap simplifyConstraint cs
                            countTrue  = length (filter (constraintIsBool True) cs')
                            countFalse = length (filter (constraintIsBool False) cs')
                            countAll   = length cs'
                        in if | countTrue > 1                                        -> CBool False
                              | countTrue == 1 && countTrue + countFalse == countAll -> CBool True
                              | otherwise                                            -> Xor cs'

-- | Merge two rules together
mergeRules :: Rule e p a -> Rule e p b -> Rule e p (a,b)
mergeRules = go
   where
      go (Fail e)           _                = Fail e
      go _                  (Fail e)         = Fail e
      go (Terminal a)       (Terminal b)     = Terminal (a,b)
      go (Terminal a)       (NonTerminal bs) = NonTerminal (fl (Terminal a) bs)
      go (NonTerminal as)   (Terminal b)     = NonTerminal (fr (Terminal b) as)
      go (NonTerminal as)   b                = NonTerminal (fr b            as)

      fl x = fmap (second (x `mergeRules`))
      fr x = fmap (second (`mergeRules` x))

-- | Reduce a constraint
constraintReduce :: (Eq p, Eq e) => (p -> Maybe Bool) -> Constraint e p -> Constraint e p
constraintReduce pred c = case simplifyConstraint c of
   Predicate p  -> case pred p of
                      Nothing -> Predicate p
                      Just v  -> CBool v
   Not c'       -> case constraintReduce pred c' of
                      CBool v -> CBool (not v)
                      c''     -> Not c''
   And cs       -> case fmap (constraintReduce pred) cs of
                      []                                     -> error "Empty And constraint"
                      cs' | all (constraintIsBool True)  cs' -> CBool True
                      cs' | any (constraintIsBool False) cs' -> CBool False
                      cs' -> case filter (not . constraintIsBool True) cs' of
                        [c'] -> c'
                        cs'' -> And cs''
   Or cs        -> case fmap (constraintReduce pred) cs of
                      []                                      -> error "Empty Or constraint"
                      cs' | all (constraintIsBool False)  cs' -> CBool False
                      cs' | any (constraintIsBool True)   cs' -> CBool True
                      cs' -> case filter (not . constraintIsBool False) cs' of
                        [c'] -> c'
                        cs'' -> Or cs''
   Xor cs       -> case fmap (constraintReduce pred) cs of
                      []  -> error "Empty Xor constraint"
                      cs' -> simplifyConstraint (Xor cs')
   c'@(CBool _) -> c'

-- | Check that a constraint is evaluated to a given boolean value
constraintIsBool :: Bool -> Constraint e p -> Bool
constraintIsBool v (CBool v') = v == v'
constraintIsBool _ _          = False


-- | Result of a rule reduction
data MatchResult e p a
   = NoMatch                -- ^ No rule leads to a terminal
   | DivergentMatch [a]     -- ^ Several rules match but they return different terminals
   | MatchFail [e]          -- ^ Some matching rules fail
   | Match a                -- ^ A single terminal value is returned
   | MatchRule (Rule e p a) -- ^ The rule may have been reduced but didn't produce a result
   deriving (Show,Eq,Ord)

instance Functor (MatchResult p e) where
   fmap f x = case x of
      NoMatch           -> NoMatch
      DivergentMatch xs -> DivergentMatch (fmap f xs)
      MatchFail es      -> MatchFail es
      Match a           -> Match (f a)
      MatchRule r       -> MatchRule (fmap f r)

instance Applicative (MatchResult e p) where
   pure a  = Match a
   a <*> b = fmap (\(f,x) -> f x) (mergeMatchResults a b)

-- | Reduce a rule
ruleReduce :: (Eq e, Eq p, Eq a) => (p -> Maybe Bool) -> Rule e p a -> MatchResult e p a
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
getRuleTerminals :: Rule e p a -> [a]
getRuleTerminals (Fail _)         = []
getRuleTerminals (Terminal a)     = [a]
getRuleTerminals (NonTerminal xs) = concatMap (getRuleTerminals . snd) xs

-- | Get predicates used in a rule
getRulePredicates :: Eq p => Rule e p a -> [p]
getRulePredicates (Fail _)         = []
getRulePredicates (Terminal _)     = []
getRulePredicates (NonTerminal xs) = nub $ concatMap (\(x,y) -> getConstraintPredicates x ++ getRulePredicates y) xs

-- | Get predicates used in a constraint
getConstraintPredicates :: Constraint e p -> [p]
getConstraintPredicates = \case
   Predicate p  -> [p]
   Not c        -> getConstraintPredicates c
   And cs       -> concatMap getConstraintPredicates cs
   Or  cs       -> concatMap getConstraintPredicates cs
   Xor cs       -> concatMap getConstraintPredicates cs
   CBool _      -> []

-- | Merge match results
mergeMatchResults :: MatchResult e p a -> MatchResult e p b -> MatchResult e p (a,b)
mergeMatchResults = go
   where
      go NoMatch  _                    = NoMatch
      go _        NoMatch              = NoMatch
      go (MatchFail xs) (MatchFail ys) = MatchFail (xs++ys)
      go (MatchFail xs) _              = MatchFail xs
      go _              (MatchFail ys) = MatchFail ys
      go (DivergentMatch xs) (DivergentMatch ys) = DivergentMatch [(x,y) | x <- xs, y <- ys]
      go (DivergentMatch xs) (Match b)           = DivergentMatch [(x,b) | x <- xs]
      go (Match a)           (DivergentMatch ys) = DivergentMatch [(a,y) | y <- ys]
                                                   -- we can't return a
                                                   -- divergent match here. We
                                                   -- transform the
                                                   -- DivergentMatch into a
                                                   -- NonTerminal with True
                                                   -- constraints
      go (DivergentMatch xs) (MatchRule b)       = MatchRule $ mergeRules (makeNT xs) b
      go (MatchRule a)       (DivergentMatch ys) = MatchRule $ mergeRules a (makeNT ys)
      go (MatchRule a)       (MatchRule b)       = MatchRule $ mergeRules a b
      go (MatchRule a)       (Match b)           = MatchRule $ mergeRules a (Terminal b)
      go (Match a)           (MatchRule b)       = MatchRule $ mergeRules (Terminal a) b
      go (Match a)           (Match b)           = Match (a,b)

      -- create a NonTerminal from a DivergentMatch
      makeNT xs = NonTerminal (fmap (\x -> (CBool True,Terminal x)) xs)

-- | A predicated data type reducer
--
-- Example:
--
-- @
-- data PD e p = PD
--    { pA :: Rule e p Int
--    , pB :: Rule e p String
--    }
-- 
-- instance (Eq e, Eq p) => Predicated (PD e p) where
--    type Pred    (PD e p) = p
--    type PredErr (PD e p) = e
--    reducePredicates fp (PD a b) = 
--       PD <$> reducePredicates fp a
--          <*> reducePredicates fp b
-- 
--    getTerminals (PD as bs) = [ PD a b | a <- getTerminals as
--                                       , b <- getTerminals bs
--                              ]
-- 
--    getPredicates (PD a b) = concat
--                               [ getPredicates a
--                               , getPredicates b
--                               ]
-- @
--
class Predicated a where
   type Pred a    :: *
   type PredErr a :: *

   -- | Reduce predicates
   reducePredicates :: (Pred a -> Maybe Bool) -> a -> MatchResult (PredErr a) (Pred a) a

   -- | Is it terminal?
   isTerminal :: a -> Bool
   isTerminal a = case reducePredicates (const Nothing) a of
      Match _ -> True
      _       -> False

   -- | Get possible resulting terminals
   getTerminals :: a -> [a]

   -- | Get used predicates
   getPredicates :: a -> [Pred a]

instance (Eq e, Eq p, Eq a) => Predicated (Rule e p a) where
   type Pred     (Rule e p a) = p
   type PredErr  (Rule e p a) = e
   reducePredicates fp r = fmap Terminal (ruleReduce fp r)

   getTerminals  = fmap Terminal . getRuleTerminals
   getPredicates = getRulePredicates


-- | Constraint checking that a predicated value evaluates to some terminal
evalsTo :: (Ord (Pred a), Eq a, Eq (Pred a), Predicated a) => a -> a -> Constraint e (Pred a)
evalsTo s a = case createPredicateTable s of
   Left x   -> CBool (x == a)
   Right xs -> orConstraints <| fmap andPredicates
                             <| fmap fst
                             <| filter ((== a) . snd)
                             <| xs
   where

      andPredicates []  = CBool True
      andPredicates [x] = makePred x
      andPredicates xs  = And (fmap makePred xs)

      orConstraints []  = CBool True
      orConstraints [x] = x
      orConstraints xs  = Or xs

      makePred (Left p)  = Not (Predicate p)
      makePred (Right p) = Predicate p



-- | Create a table of predicates that return a terminal
--
-- Left p: Not p
-- Right p: p
createPredicateTable :: (Ord (Pred a), Eq (Pred a), Eq a, Predicated a) => a -> Either a [([Either (Pred a) (Pred a)],a)]
createPredicateTable s =
   -- we first check if the predicated value reduces to a terminal without any
   -- additional oracle
   case reducePredicates (const Nothing) s of
      Match x -> Left x
      _       -> Right (mapMaybe matching predSets)
   where
      matching ps = case reducePredicates (makeOracle ps) s of
         Match x -> Just (ps,x)
         _       -> Nothing

      -- create an oracle function from a set of predicates
      makeOracle []           = \_ -> Nothing
      makeOracle (Left  x:xs) = \p -> if p == x then Just False else makeOracle xs p
      makeOracle (Right x:xs) = \p -> if p == x then Just True  else makeOracle xs p

      -- sets of predicates either False (Right p) or True (Left p)
      preds        = sort (getPredicates s)
      predSets     = fmap go ([0..2^(length preds)-1] :: [Word])
      go n         = fmap (setB n) (preds `zip` [0..])
      setB n (p,i) = if testBit n i
         then (Right p)
         else (Left  p)

