{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

-- | Simple Constraint solver
module Haskus.Utils.Solver
   (
   -- * Constraint
     Constraint (..)
   , simplifyConstraint
   , constraintReduce
   -- * Rule
   , Rule (..)
   , orderedNonTerminal
   , mergeRules
   , evalsTo
   , MatchResult (..)
   -- * Predicated data
   , Predicated (..)
   , createPredicateTable
   , initP
   , applyP
   , resultP
   )
where

import Haskus.Utils.Maybe
import Haskus.Utils.Flow
import Haskus.Utils.List

import Data.Bits
import Control.Arrow (first,second)

import Prelude hiding (pred)

-------------------------------------------------------
-- Constraint
-------------------------------------------------------

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

-- | Get predicates used in a constraint
getConstraintPredicates :: Constraint e p -> [p]
getConstraintPredicates = \case
   Predicate p  -> [p]
   Not c        -> getConstraintPredicates c
   And cs       -> concatMap getConstraintPredicates cs
   Or  cs       -> concatMap getConstraintPredicates cs
   Xor cs       -> concatMap getConstraintPredicates cs
   CBool _      -> []

-- | Get constraint terminals
getConstraintTerminals :: Constraint e p -> [Bool]
getConstraintTerminals = \case
   Predicate _  -> [True,False]
   CBool v      -> [v]
   Not c        -> fmap not (getConstraintTerminals c)
   And cs       -> let cs' = fmap getConstraintTerminals cs
                   in if | null cs                -> []
                         | any (False `elem`) cs' -> [False]
                         | all (sing True)    cs' -> [True]
                         | otherwise              -> [True,False]
   Or  cs       -> let cs' = fmap getConstraintTerminals cs
                   in if | null cs                -> []
                         | any (True `elem`) cs'  -> [True]
                         | all (sing False)   cs' -> [False]
                         | otherwise              -> [True,False]
   Xor cs       -> let cs' = fmap getConstraintTerminals cs
                   in if | null cs                -> []
                         | otherwise              -> xo False cs'
   where
      xo t     []           = [t]
      xo False ([True]:xs)  = xo True xs
      xo True  ([True]:_)   = [False]
      xo False ([False]:xs) = xo False xs
      xo True  ([False]:xs) = xo True xs
      xo _     ([]:_)       = []
      xo _     _            = [True,False]

      sing v [v'] = v == v'
      sing _ _    = False


-------------------------------------------------------
-- Rule
-------------------------------------------------------

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
                        in if | any (constraintIsBool False) cs' -> CBool False
                              | all (constraintIsBool True)  cs' -> CBool True
                              | otherwise                        -> And cs'
   Or cs             -> let cs' = fmap simplifyConstraint cs
                        in if | any (constraintIsBool True) cs'  -> CBool True
                              | all (constraintIsBool False) cs' -> CBool False
                              | otherwise                        -> Or cs'
   Xor cs            -> let cs'        = fmap simplifyConstraint cs
                            countTrue  = length (filter (constraintIsBool True) cs')
                            countFalse = length (filter (constraintIsBool False) cs')
                            countAll   = length cs'
                        in if | countTrue > 1                                        -> CBool False
                              | countTrue == 1 && countTrue + countFalse == countAll -> CBool True
                              | countAll == countFalse                               -> CBool False
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


-- | Reduce a rule
ruleReduce :: forall e p a. (Eq e, Eq p, Eq a) => (p -> Maybe Bool) -> Rule e p a -> MatchResult e (Rule e p a) a
ruleReduce pred r = case r of
   Terminal a     -> Match a
   Fail e         -> MatchFail [e]
   NonTerminal rs -> 
      let
         rs' :: [(Constraint e p, Rule e p a)]
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
            | divergence                    -> MatchDiverge (fmap Terminal terminalResults)
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
                  _                     -> DontMatch (NonTerminal rs')


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

-- | Constraint checking that a predicated value evaluates to some terminal
evalsTo :: (Ord (Pred a), Eq a, Eq (PredTerm a), Eq (Pred a), Predicated a) => a -> PredTerm a -> Constraint e (Pred a)
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


-------------------------------------------------------
-- Predicated data
-------------------------------------------------------



-- | Predicated data
--
-- @
-- data T
-- data NT
-- 
-- type family RuleT e p a s :: * where
--    RuleT e p a T   = a
--    RuleT e p a NT  = Rule e p a
--
-- data PD t = PD
--    { p1 :: RuleT () Bool Int t
--    , p2 :: RuleT () Bool String t
--    }
-- 
-- instance Predicated (PD NT) where
--    type PredErr (PD NT) = ()
--    type Pred (PD NT)    = Bool
--    type PredTerm (PD NT)    = PD T
-- 
--    liftTerminal (PD a b) = PD (liftTerminal a) (liftTerminal b)
-- 
--    reducePredicates oracle (PD a b) =
--       initP PD PD
--          |> (`applyP` reducePredicates oracle a)
--          |> (`applyP` reducePredicates oracle b)
--          |> resultP
-- @
class Predicated a where
   -- | Error type
   type PredErr a :: *

   -- | Predicate type
   type Pred a    :: *

   -- | Terminal type
   type PredTerm a    :: *

   -- | Build a non terminal from a terminal
   liftTerminal :: PredTerm a -> a

   -- | Reduce predicates
   reducePredicates :: (Pred a -> Maybe Bool) -> a -> MatchResult (PredErr a) a (PredTerm a)

   -- | Get possible resulting terminals
   getTerminals :: a -> [PredTerm a]

   -- | Get used predicates
   getPredicates :: a -> [Pred a]


instance (Eq e, Eq a, Eq p) => Predicated (Rule e p a) where
   type PredErr  (Rule e p a) = e
   type Pred     (Rule e p a) = p
   type PredTerm (Rule e p a) = a

   reducePredicates = ruleReduce
   liftTerminal     = Terminal
   getTerminals     = getRuleTerminals
   getPredicates    = getRulePredicates

instance (Eq e, Eq p) => Predicated (Constraint e p) where
   type PredErr  (Constraint e p) = e
   type Pred     (Constraint e p) = p
   type PredTerm (Constraint e p) = Bool

   reducePredicates oracle c = case constraintReduce oracle c of
      CBool v -> Match v
      c'      -> DontMatch c'

   liftTerminal     = CBool
   getTerminals     = getConstraintTerminals
   getPredicates    = getConstraintPredicates


-- | Reduction result
data MatchResult e nt t
   = NoMatch
   | Match t
   | DontMatch nt
   | MatchFail [e]
   | MatchDiverge [nt]
   deriving (Show,Eq,Ord)

instance Functor (MatchResult e nt) where
   fmap f x = case x of
      NoMatch    -> NoMatch
      MatchDiverge xs -> MatchDiverge xs
      MatchFail es    -> MatchFail es
      Match a     -> Match (f a)
      DontMatch a       -> DontMatch a

-- | Compose reduction results
--
-- We reuse the MatchResult data type:
--    * a "terminal" on the left can be used to build either a terminal or a non terminal
--    * a "non terminal" on the left can only be used to build a non terminal
applyP ::
   ( Predicated ntb
   ) => MatchResult e (ntb -> nt) (ntb -> nt, PredTerm ntb -> t) -> MatchResult e ntb (PredTerm ntb) -> MatchResult e nt (nt,t)
applyP NoMatch            _                 = NoMatch
applyP _                  NoMatch           = NoMatch

applyP (MatchFail xs)     (MatchFail ys)    = MatchFail (xs++ys)
applyP (MatchFail xs)     _                 = MatchFail xs
applyP _                  (MatchFail ys)    = MatchFail ys

applyP (MatchDiverge fs)  (MatchDiverge ys) = MatchDiverge [f y | f <- fs, y <- ys]
applyP (MatchDiverge fs)  (Match b)         = MatchDiverge [f (liftTerminal b) | f <- fs]
applyP (MatchDiverge fs)  (DontMatch b)     = MatchDiverge [f b | f <- fs]

applyP (DontMatch f)      (MatchDiverge ys) = MatchDiverge [f y | y <- ys]
applyP (DontMatch f)      (DontMatch b)     = DontMatch    (f b)
applyP (DontMatch f)      (Match b)         = DontMatch    (f (liftTerminal b))

applyP (Match (fnt,_))    (MatchDiverge ys) = MatchDiverge [fnt y | y <- ys]
applyP (Match (fnt,_))    (DontMatch b)     = DontMatch    (fnt b)
applyP (Match (fnt,ft))   (Match b)         = Match        (fnt (liftTerminal b), ft b)

-- | Initialise a reduction result (typically with two functions/constructors)
initP :: nt -> t -> MatchResult e nt (nt,t)
initP nt t = Match (nt,t)

-- | Fixup result (see initP and applyP)
resultP :: MatchResult e nt (nt,t) -> MatchResult e nt t
resultP = fmap snd

-- | Create a table of predicates that return a terminal
--
-- Left p: Not p
-- Right p: p
createPredicateTable ::
   ( Ord (Pred a)
   , Eq (Pred a)
   , Eq a
   , Predicated a
   , Predicated a
   , Pred a ~ Pred a
   ) => a -> Either (PredTerm a) [([Either (Pred a) (Pred a)],PredTerm a)]
createPredicateTable s =
   -- we first check if the predicated value reduces to a terminal without any
   -- additional oracle
   case reducePredicates (const Nothing) s of
      Match x -> Left x
      _         -> Right (mapMaybe matching predSets)
   where
      matching ps = case reducePredicates (makeOracle ps) s of
         Match x -> Just (ps,x)
         _         -> Nothing

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



