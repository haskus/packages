{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Haskus.Calculus.Concept.Bind
   ( Bind (..)
   , BindScc (..)
   )
where

import Haskus.Calculus.FreeVars
import Data.Set as Set

-- | Binding (name to expression)
data Bind n e = Bind n e deriving (Functor)

-- | Strongly-connected bindings
data BindScc n e
   = NonRec (Bind n e)
   | Rec    [Bind n e]
   deriving (Functor)

instance Ord n => FreeVarsF n (Bind n) where
   freeVarsF (Bind n s) = Set.delete n s

instance Ord n => FreeVarsF n (BindScc n) where
   freeVarsF = \case
      NonRec (Bind _ s) -> s
      Rec bs            -> Set.difference ms ns
         where
            names = fmap (\(Bind n _) -> n) bs
            ns    = Set.fromList names
            sets  = fmap (\(Bind _ s) -> s) bs
            ms    = Set.unions sets
