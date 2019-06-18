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
   -- * Oracle
     PredState (..)
   , PredOracle
   , makeOracle
   , oraclePredicates
   , emptyOracle
   , oracleUnion
   , predIsSet
   , predIsUnset
   , predIsUndef
   , predIsInvalid
   , predIs
   , predState
   , predAdd
   -- * Constraint
   , Constraint (..)
   , constraintOptimize
   , constraintSimplify
   -- * Rule
   , Rule (..)
   , ruleSimplify
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
import Haskus.Utils.Map.Strict (Map)
import qualified Haskus.Utils.Map.Strict as Map

import Control.Arrow (first,second)

import Prelude hiding (pred,length)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies

-------------------------------------------------------
-- Constraint
-------------------------------------------------------

-- | Predicate state
data PredState
   = SetPred       -- ^ Set predicate
   | UnsetPred     -- ^ Unset predicate
   | UndefPred     -- ^ Undefined predicate
   | InvalidPred   -- ^ Invalid predicate (can't be used)
   deriving (Show,Eq,Ord)

-- | Predicate oracle
type PredOracle p = Map p PredState

-- | Ask an oracle if a predicate is set
predIsSet :: Ord p => PredOracle p -> p -> Bool
predIsSet oracle p = predIs oracle p SetPred

-- | Ask an oracle if a predicate is unset
predIsUnset :: Ord p => PredOracle p -> p -> Bool
predIsUnset oracle p = predIs oracle p UnsetPred

-- | Ask an oracle if a predicate is undefined
predIsUndef :: Ord p => PredOracle p -> p -> Bool
predIsUndef oracle p = predIs oracle p UndefPred

-- | Ask an oracle if a predicate is invalid
predIsInvalid :: Ord p => PredOracle p -> p -> Bool
predIsInvalid oracle p = predIs oracle p InvalidPred

-- | Check the state of a predicate
predIs :: Ord p => PredOracle p -> p -> PredState -> Bool
predIs oracle p s = predState oracle p == s

-- | Get predicate state
predState :: Ord p => PredOracle p -> p -> PredState
predState oracle p = case p `Map.lookup` oracle of
   Just s  -> s
   Nothing -> UndefPred

-- | Create an oracle from a list
makeOracle :: Ord p => [(p,PredState)] -> PredOracle p
makeOracle = Map.fromList

-- | Get a list of valid and defined predicates from an oracle
oraclePredicates :: Ord p => PredOracle p -> [(p,PredState)]
oraclePredicates = filter (\(_,s) -> s /= UndefPred) . Map.toList

-- | Combine two oracles
-- TODO: check that there is no contradiction
oracleUnion :: Ord p => PredOracle p -> PredOracle p -> PredOracle p
oracleUnion = Map.union

-- | Add predicates to an oracle
-- TODO: check that there is no contradiction
predAdd :: Ord p => [(p,PredState)] -> PredOracle p -> PredOracle p
predAdd cs = oracleUnion (makeOracle cs)


-- | Oracle that always answer Undef
emptyOracle :: PredOracle p
emptyOracle = Map.empty

-------------------------------------------------------
-- Constraint
-------------------------------------------------------

-- | A constraint is a boolean expression
--
-- `p` is the predicate type
data Constraint e p
   = Predicate p            -- ^ Predicate value
   | IsValid p              -- ^ Is the predicate valid
   | Not (Constraint e p)   -- ^ Logic not
   | And [Constraint e p]   -- ^ Logic and
   | Or  [Constraint e p]   -- ^ Logic or
   | Xor [Constraint e p]   -- ^ Logic xor
   | CBool Bool             -- ^ Constant
   | CErr (Either String e) -- ^ Error
   deriving (Show,Eq,Ord)

instance Functor (Constraint e) where
   fmap f (Predicate p)  = Predicate (f p)
   fmap f (IsValid   p)  = IsValid (f p)
   fmap _ (CBool b)      = CBool b
   fmap f (Not c)        = Not (fmap f c)
   fmap f (And cs)       = And (fmap (fmap f) cs)
   fmap f (Or cs)        = Or (fmap (fmap f) cs)
   fmap f (Xor cs)       = Xor (fmap (fmap f) cs)
   fmap _ (CErr e)       = CErr e

-- | Reduce a constraint
--
-- >>> data P = A | B deriving (Show,Eq,Ord)
-- >>> let c = And [IsValid A, Predicate B]
--
-- >>> let oracle = makeOracle [(A,InvalidPred),(B,SetPred)]
-- >>> constraintSimplify oracle c
-- CBool False
--
-- >>> let oracle = makeOracle [(A,SetPred),(B,SetPred)]
-- >>> constraintSimplify oracle c
-- CBool True
--
-- >>> let oracle = makeOracle [(A,SetPred),(B,UnsetPred)]
-- >>> constraintSimplify oracle c
-- CBool False
constraintSimplify :: (Ord p, Eq p, Eq e) => PredOracle p -> Constraint e p -> Constraint e p
constraintSimplify oracle c = case constraintOptimize c of
   CErr e       -> CErr e
   IsValid p    -> case predState oracle p of
                     UndefPred   -> IsValid p
                     InvalidPred -> CBool False
                     SetPred     -> CBool True
                     UnsetPred   -> CBool True
   Predicate p  -> case predState oracle p of
                      UndefPred   -> Predicate p
                      InvalidPred -> CErr (Left "Invalid predicate")
                      SetPred     -> CBool True
                      UnsetPred   -> CBool False
   Not c'       -> case constraintSimplify oracle c' of
                      CBool v -> CBool (not v)
                      CErr e  -> CErr e
                      c''     -> Not c''
   And cs       -> case fmap (constraintSimplify oracle) cs of
                      []                                     -> CErr (Left "Empty And constraint")
                      cs' | all (constraintIsBool True)  cs' -> CBool True
                      cs' | any (constraintIsBool False) cs' -> CBool False
                      cs' | all constraintIsError cs'        -> CErr (Left "And expression only contains Error constraints")
                      cs' -> case filter (not . constraintIsBool True) cs' of
                        [c'] -> c'
                        cs'' -> And cs''
   Or cs        -> case filter (not . constraintIsError) <| fmap (constraintSimplify oracle) cs of
                      []                                      -> CErr (Left "Empty Or constraint")
                      cs' | all (constraintIsBool False)  cs' -> CBool False
                      cs' | any (constraintIsBool True)   cs' -> CBool True
                      cs' -> case filter (not . constraintIsBool False) cs' of
                        [c'] -> c'
                        cs'' -> Or cs''
   Xor cs       -> case fmap (constraintSimplify oracle) cs of
                      cs' | any constraintIsError cs' -> CErr (Left "Xor expression contains Error constraint")
                      []                              -> CErr (Left "Empty Xor constraint")
                      cs'                             -> constraintOptimize (Xor cs')
   c'@(CBool _) -> c'

-- | Check that a constraint is evaluated to a given boolean value
constraintIsBool :: Bool -> Constraint e p -> Bool
constraintIsBool v (CBool v') = v == v'
constraintIsBool _ _          = False

-- | Check that a constraint is evaluated to an error
constraintIsError :: Constraint e p -> Bool
constraintIsError (CErr _) = True
constraintIsError _        = False

-- | Get predicates used in a constraint
getConstraintPredicates :: Constraint e p -> [p]
getConstraintPredicates = \case
   CErr _       -> []
   IsValid   p  -> [p]
   Predicate p  -> [p]
   Not c        -> getConstraintPredicates c
   And cs       -> concatMap getConstraintPredicates cs
   Or  cs       -> concatMap getConstraintPredicates cs
   Xor cs       -> concatMap getConstraintPredicates cs
   CBool _      -> []

-- | Get constraint terminals
getConstraintTerminals :: Constraint e p -> [Bool]
getConstraintTerminals = \case
   CErr _       -> []
   IsValid   _  -> [True,False]
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

-- | Optimize/simplify a constraint
constraintOptimize :: Constraint e p -> Constraint e p
constraintOptimize x = case x of
   CErr _            -> x
   Not (CErr e)      -> CErr e
   IsValid _         -> x
   Predicate _       -> x
   CBool _           -> x
   Not (IsValid _)   -> x
   Not (Predicate _) -> x
   Not (CBool v)     -> CBool (not v)
   Not (Not c)       -> constraintOptimize c
   Not (Or cs)       -> constraintOptimize (And (fmap Not cs))
   Not (And cs)      -> constraintOptimize (Or (fmap Not cs))
   Not (Xor cs)      -> case constraintOptimize (Xor cs) of
                           Xor cs' -> Not (Xor cs')
                           r       -> constraintOptimize (Not r)
   And [c]           -> constraintOptimize c
   Or  [c]           -> constraintOptimize c
   Xor [c]           -> let c' = constraintOptimize c
                        in if | constraintIsBool True c'  -> CBool True
                              | constraintIsBool False c' -> CBool False
                              | otherwise                 -> c'
   And cs            -> let cs' = fmap constraintOptimize cs
                        in if | any (constraintIsBool False) cs' -> CBool False
                              | all (constraintIsBool True)  cs' -> CBool True
                              | otherwise                        -> And cs'
   Or cs             -> let cs' = fmap constraintOptimize cs
                        in if | any (constraintIsBool True) cs'  -> CBool True
                              | all (constraintIsBool False) cs' -> CBool False
                              | otherwise                        -> Or cs'
   Xor cs            -> let cs'        = fmap constraintOptimize cs
                            countTrue  = length (filter (constraintIsBool True) cs')
                            countFalse = length (filter (constraintIsBool False) cs')
                            countAll   = length cs'
                        in if | countTrue > 1                                        -> CBool False
                              | countTrue == 1 && countTrue + countFalse == countAll -> CBool True
                              | countAll == countFalse                               -> CBool False
                              | otherwise                                            -> Xor cs'


-------------------------------------------------------
-- Rule
-------------------------------------------------------

-- | A rule can produce some "a"s (one or more if it diverges), depending on the
-- constraints.
data Rule e p a
   = Terminal a
   | OrderedNonTerminal [(Constraint e p, Rule e p a)]
   | NonTerminal [(Constraint e p, Rule e p a)]
   | Fail e
   deriving (Show,Eq,Ord)

instance Functor (Rule e p) where
   fmap f (Terminal a)            = Terminal (f a)
   fmap f (NonTerminal xs)        = NonTerminal (fmap (second (fmap f)) xs)
   fmap f (OrderedNonTerminal xs) = OrderedNonTerminal (fmap (second (fmap f)) xs)
   fmap _ (Fail e)                = Fail e


-- | Simplify a rule given an oracle
ruleSimplify ::
   ( Ord p, Eq e
   ) => PredOracle p -> Rule e p a -> Rule e p a
ruleSimplify oracle r = case r of
   Terminal a            -> Terminal a
   Fail e                -> Fail e
   OrderedNonTerminal rs -> OrderedNonTerminal (simplifyNonTerminal rs)
   NonTerminal rs        -> NonTerminal (concatMap foldNonTerminal (simplifyNonTerminal rs))
   where
      -- Simplify non-terminal rule constraints. Remove rules whose constraint is False
      simplifyNonTerminal xs = xs
         -- reduce constraints
         |> fmap (first (constraintSimplify oracle))
         -- recursively simplify nested rules
         |> fmap (second (ruleSimplify oracle))
         -- filter non matching rules
         |> filter (not . constraintIsBool False . fst)

      -- non terminal sub-rules whose constraints are True can be folded into the
      -- upper non-terminal rule. We rely on this to perform rule reduction.
      foldNonTerminal (c, NonTerminal rs)
         | constraintIsBool True c = rs
      foldNonTerminal x = [x]


-- | Reduce a rule
ruleReduce :: forall e p a.
   ( Ord p, Eq e, Eq p, Eq a) => PredOracle p -> Rule e p a -> MatchResult e (Rule e p a) a
ruleReduce oracle r = case ruleSimplify oracle r of
   Terminal a            -> Match a
   Fail e                -> MatchFail [e]
   NonTerminal []        -> NoMatch
   OrderedNonTerminal [] -> NoMatch
   OrderedNonTerminal ((c,x):xs)
      | constraintIsBool True c  -> ruleReduce oracle x
      | constraintIsBool False c -> ruleReduce oracle (OrderedNonTerminal xs)
      | otherwise                -> DontMatch (OrderedNonTerminal ((c,x):xs))
   NonTerminal rs -> 
      let
         (matchingRules,mayMatchRules) = partition (constraintIsBool True . fst) rs
         matchingResults               = nub $ fmap snd $ matchingRules


         (failingResults,terminalResults,hasNonTerminalResults) = go [] [] False matchingResults
         go fr tr ntr = \case
            []                        -> (fr,tr,ntr)
            (Fail x:xs)               -> go (x:fr) tr ntr  xs
            (Terminal x:xs)           -> go fr (x:tr) ntr  xs
            (NonTerminal _:xs)        -> go fr tr     True xs
            (OrderedNonTerminal _:xs) -> go fr tr     True xs

         divergence = case terminalResults of
            -- results are already "nub"ed.
            -- More than 1 results => divergence
            (_:_:_) -> True
            _       -> False
      in
      if | not (null failingResults)     -> MatchFail failingResults
         | divergence                    -> MatchDiverge (fmap Terminal terminalResults)
         | hasNonTerminalResults         -> DontMatch (NonTerminal rs)
         | otherwise                     ->
            case (terminalResults,mayMatchRules) of
               ([a], []) -> Match a
               _         -> DontMatch (NonTerminal rs)


-- | Get possible resulting terminals
getRuleTerminals :: Rule e p a -> [a]
getRuleTerminals (Fail _)                = []
getRuleTerminals (Terminal a)            = [a]
getRuleTerminals (NonTerminal xs)        = concatMap (getRuleTerminals . snd) xs
getRuleTerminals (OrderedNonTerminal xs) = concatMap (getRuleTerminals . snd) xs

-- | Get predicates used in a rule
getRulePredicates :: Eq p => Rule e p a -> [p]
getRulePredicates (Fail _)                = []
getRulePredicates (Terminal _)            = []
getRulePredicates (NonTerminal xs)        = nub $ concatMap (\(x,y) -> getConstraintPredicates x ++ getRulePredicates y) xs
getRulePredicates (OrderedNonTerminal xs) = nub $ concatMap (\(x,y) -> getConstraintPredicates x ++ getRulePredicates y) xs

-- | Constraint checking that a predicated value evaluates to some terminal
evalsTo :: (Ord (Pred a), Eq a, Eq (PredTerm a), Eq (Pred a), Predicated a) => a -> PredTerm a -> Constraint e (Pred a)
evalsTo s a = case createPredicateTable s (const True) of
   Left x   -> CBool (x == a)
   Right xs -> orConstraints <| fmap andPredicates
                             <| fmap oraclePredicates
                             <| fmap fst
                             <| filter ((== a) . snd)
                             <| xs
   where

      andPredicates []  = CBool True
      andPredicates xs  = And (concatMap makePred xs)

      orConstraints []  = CBool True
      orConstraints [x] = x
      orConstraints xs  = Or xs

      makePred (p, UnsetPred)   = [IsValid p, Not (Predicate p)]
      makePred (p, SetPred)     = [IsValid p, Predicate p]
      makePred (p, InvalidPred) = [Not (IsValid p)]
      makePred (_, UndefPred)   = undefined -- shouldn't be possible given we use
                                            -- get the predicates from the oracle itself


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
-- deriving instance Eq (PD T)
-- deriving instance Show (PD T)
-- deriving instance Ord (PD T)
-- deriving instance Eq (PD NT)
-- deriving instance Show (PD NT)
-- deriving instance Ord (PD NT)
--
-- 
-- instance Predicated (PD NT) where
--    type PredErr (PD NT)  = ()
--    type Pred (PD NT)     = Bool
--    type PredTerm (PD NT) = PD T
-- 
--    liftTerminal (PD a b) = PD (liftTerminal a) (liftTerminal b)
-- 
--    reducePredicates oracle (PD a b) =
--       initP PD PD
--          |> (`applyP` reducePredicates oracle a)
--          |> (`applyP` reducePredicates oracle b)
--          |> resultP
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
   reducePredicates :: PredOracle (Pred a) -> a -> MatchResult (PredErr a) a (PredTerm a)

   -- | Simplify predicates
   simplifyPredicates :: PredOracle (Pred a) -> a -> a

   -- | Get possible resulting terminals
   getTerminals :: a -> [PredTerm a]

   -- | Get used predicates
   getPredicates :: a -> [Pred a]


instance (Ord p, Eq e, Eq a, Eq p) => Predicated (Rule e p a) where
   type PredErr  (Rule e p a) = e
   type Pred     (Rule e p a) = p
   type PredTerm (Rule e p a) = a

   reducePredicates   = ruleReduce
   simplifyPredicates = ruleSimplify
   liftTerminal     = Terminal
   getTerminals     = getRuleTerminals
   getPredicates    = getRulePredicates

instance (Ord p, Eq e, Eq p) => Predicated (Constraint e p) where
   type PredErr  (Constraint e p) = e
   type Pred     (Constraint e p) = p
   type PredTerm (Constraint e p) = Bool

   reducePredicates oracle c = case constraintSimplify oracle c of
      CBool v -> Match v
      c'      -> DontMatch c'

   simplifyPredicates oracle c = constraintSimplify oracle c

   liftTerminal     = CBool
   getTerminals     = getConstraintTerminals
   getPredicates    = getConstraintPredicates

instance forall x y.
   ( Predicated x
   , Predicated y
   , PredErr x ~ PredErr y
   , Pred x ~ Pred y
   ) => Predicated (x,y)
   where
   type PredErr  (x,y) = PredErr x
   type Pred     (x,y) = Pred x
   type PredTerm (x,y) = (PredTerm x, PredTerm y)

   reducePredicates oracle (x,y) =
      initP (,) (,)
         |> (`applyP` reducePredicates oracle x)
         |> (`applyP` reducePredicates oracle y)
         |> resultP

   simplifyPredicates oracle (x,y) = (simplifyPredicates oracle x, simplifyPredicates oracle y)

   liftTerminal (x,y)  = (liftTerminal x, liftTerminal y)
   getTerminals (x,y)  = [ (x',y') | x' <- getTerminals x
                                   , y' <- getTerminals y
                         ]
   getPredicates (x,y) = getPredicates x ++ getPredicates y

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
      NoMatch         -> NoMatch
      MatchDiverge xs -> MatchDiverge xs
      MatchFail es    -> MatchFail es
      Match a         -> Match (f a)
      DontMatch a     -> DontMatch a

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
createPredicateTable ::
   ( Ord (Pred a)
   , Eq (Pred a)
   , Eq a
   , Predicated a
   , Predicated a
   , Pred a ~ Pred a
   ) => a -> (PredOracle (Pred a) -> Bool) -> Either (PredTerm a) [(PredOracle (Pred a),PredTerm a)]
createPredicateTable s oracleChecker =
   -- we first check if the predicated value reduces to a terminal without any
   -- additional oracle
   case reducePredicates emptyOracle s of
      Match x -> Left x
      _       -> Right (mapMaybe matching oracles)
   where
      matching oracle = case reducePredicates oracle s of
         Match x -> Just (oracle,x)
         _       -> Nothing

      oracles = filter oracleChecker (fmap makeOracle predSets)

      preds = nub (sort (getPredicates (simplifyPredicates emptyOracle s)))

      predSets = makeSets preds [[]] 

      -- make predicate sets (each predicate is either Set, Unset or Undef)
      makeSets []     os  = os
      makeSets (p:ps) os = let ns = [(p,SetPred),(p,UnsetPred),(p,UndefPred)]
                           in makeSets ps [(n:o) | o <- os, n <- ns]
