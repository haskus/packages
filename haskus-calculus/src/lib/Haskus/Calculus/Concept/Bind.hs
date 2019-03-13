{-# LANGUAGE DeriveFunctor #-}

module Haskus.Calculus.Concept.Bind
   ( Bind (..)
   , BindScc (..)
   )
where

-- | Binding (name to expression)
data Bind n e = Bind n e deriving (Functor)

-- | Strongly-connected bindings
data BindScc n e
   = NonRec (Bind n e)
   | Rec    [Bind n e]
   deriving (Functor)
