{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Haskus.Maths.Logic.Propositional where

import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intersperse)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> :set -XPatternSynonyms
-- >>> :set -XDeriveFunctor
-- >>> :set -XGADTs
-- >>> :set -XPolyKinds
-- >>> :set -XLambdaCase
--
-- >>> type F a = EADT '[FalseF,TrueF,AtomF a,NotF,AndF,OrF,ImpF,IffF]

data FalseF  e = FalseF     deriving (Show,Functor)
data TrueF   e = TrueF      deriving (Show,Functor)
data AtomF a e = AtomF a    deriving (Show,Functor)
data NotF    e = NotF e     deriving (Show,Functor)
data AndF    e = AndF e e   deriving (Show,Functor)
data OrF     e = OrF e e    deriving (Show,Functor)
data ImpF    e = ImpF e e   deriving (Show,Functor)
data IffF    e = IffF e e   deriving (Show,Functor)

eadtPattern 'FalseF "Fals"
eadtPattern 'TrueF  "Tru"
eadtPattern 'AtomF  "Atom"
eadtPattern 'NotF   "Not"
eadtPattern 'AndF   "And"
eadtPattern 'OrF    "Or"
eadtPattern 'ImpF   "Imp"
eadtPattern 'IffF   "Iff"

class Atoms a f where
   atoms' :: f (Set a) -> Set a

instance Atoms a FalseF where
   atoms' _ = Set.empty

instance Atoms a TrueF where
   atoms' _ = Set.empty

instance Atoms a (AtomF a) where
   atoms' (AtomF a) = Set.singleton a

instance Atoms a NotF where
   atoms' (NotF s) = s

instance Ord a => Atoms a AndF where
   atoms' (AndF s1 s2) = Set.union s1 s2

instance Ord a => Atoms a OrF where
   atoms' (OrF s1 s2) = Set.union s1 s2

instance Ord a => Atoms a ImpF where
   atoms' (ImpF s1 s2) = Set.union s1 s2

instance Ord a => Atoms a IffF where
   atoms' (IffF s1 s2) = Set.union s1 s2

-- | Retrieve atoms of a formula
--
-- >>> let x = (Atom 'a' `Or` Atom 'b') `Imp` Atom 'c' :: F Char
-- >>> atoms @Char x
-- fromList "abc"
--
atoms :: forall a xs.
   BottomUp (Atoms a) xs (Set a)
   => EADT xs -> Set a
atoms e = bottomUp (toBottomUp @(Atoms a) atoms') e


class Eval a f where
   eval' :: (a -> Bool) -> f Bool -> Bool

instance Eval a FalseF where
   eval' _ _ = False

instance Eval a TrueF where
   eval' _ _ = True

instance Eval a (AtomF a) where
   eval' v (AtomF a) = v a

instance Eval a NotF where
   eval' _ (NotF p) = not p

instance Eval a AndF where
   eval' _ (AndF p q) = p && q

instance Eval a OrF where
   eval' _ (OrF p q) = p || q

instance Eval a ImpF where
   eval' _ (ImpF p q) = not p || q

instance Eval a IffF where
   eval' _ (IffF p q) = p == q

-- | Evaluate a formula
--
-- >>> let x = (Atom 'a' `Or` Atom 'b') `Imp` Atom 'c' :: F Char
-- >>> eval @Char (const True) x
-- True
--
-- >>> :{
-- >>> eval @Char (\case
--                   'a' -> True
--                   'b' -> True
--                   _   -> False) x
-- >>> :}
-- False
--
eval :: forall a xs.
   BottomUp (Eval a) xs Bool
   => (a -> Bool) -> EADT xs -> Bool
eval v e = bottomUp (toBottomUp @(Eval a) (eval' v)) e

-- | Return all the valuation functions for a list of atoms
--
-- i.e. all the combinations of True/False values for each atom.
atomsValuations :: Eq a => [a] -> [a -> Bool]
atomsValuations = go (const False)
   where
      go v []     = [v]
      go v (x:xs) = go (\u -> if u == x then False else v u) xs
                    ++ go (\u -> if u == x then True else v u) xs

-- | Return all the valuation functions for a list of atoms
--
-- i.e. all the combinations of True/False values for each atom.
valuations :: forall a xs.
   ( Eq a
   , BottomUp (Atoms a) xs (Set a)
   ) => EADT xs -> [a -> Bool]
valuations f = atomsValuations (Set.toList (atoms @a f))

-- | Show the truth table of a formula
--
-- >>> let x = (Atom 'a' `Or` Atom 'b') `Imp` Atom 'c' :: F Char
-- >>> putStr (truthTableStr @Char x)
-- 'a' 'b' 'c' formula
-- 0   0   0   1
-- 0   0   1   1
-- 0   1   0   0
-- 0   1   1   1
-- 1   0   0   0
-- 1   0   1   1
-- 1   1   0   0
-- 1   1   1   1
truthTableStr :: forall a xs.
   ( Eq a
   , Show a
   , BottomUp (Atoms a) xs (Set a)
   , BottomUp (Eval a) xs Bool
   ) => EADT xs -> String
truthTableStr f = mconcat (hdr:fmap row (atomsValuations as))
   where
      tabs  = concat . intersperse " "
      as    = Set.toList (atoms @a f)
      hdr   = tabs (fmap show as ++ ["formula"]) ++ "\n"
      lens  = fmap (length . show) as ++ [0]
      row v = concat (zipWith apLen lens (fmap v as ++ [eval @a v f])) ++ "\n"
      apLen l r = shB r:replicate l ' '
      shB False = '0'
      shB True  = '1'

-- | Return True for all for all valuations?
--
-- >>> let x = (Atom 'a' `Or` Tru) :: F Char
-- >>> putStr (truthTableStr @Char x)
-- 'a' formula
-- 0   1
-- 1   1
-- >>> tautology @Char x
-- True
tautology :: forall a xs.
   ( Eq a
   , BottomUp (Eval a) xs Bool
   , BottomUp (Atoms a) xs (Set a)
   ) => EADT xs -> Bool
tautology f = and [ eval @a v f | v <- valuations f]

-- | Return False for all for all valuations?
--
-- >>> let x = (Atom 'a' `And` Fals) :: F Char
-- >>> putStr (truthTableStr @Char x)
-- 'a' formula
-- 0   0
-- 1   0
-- >>> unsatisfiable @Char x
-- True
unsatisfiable :: forall a xs.
   ( Eq a
   , BottomUp (Eval a) xs Bool
   , BottomUp (Atoms a) xs (Set a)
   ) => EADT xs -> Bool
unsatisfiable f = not (satisfiable @a f)

-- | Return True for at least one valuation?
--
-- >>> let x = (Atom 'a' `Or` Fals) :: F Char
-- >>> putStr (truthTableStr @Char x)
-- 'a' formula
-- 0   0
-- 1   1
-- >>> satisfiable @Char x
-- True
satisfiable :: forall a xs.
   ( Eq a
   , BottomUp (Eval a) xs Bool
   , BottomUp (Atoms a) xs (Set a)
   ) => EADT xs -> Bool
satisfiable f = or [ eval @a v f | v <- valuations f]
