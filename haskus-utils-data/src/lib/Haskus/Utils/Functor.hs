-- | Functor helpers
module Haskus.Utils.Functor
   ( module Data.Functor.Foldable
   , Algebra
   , CoAlgebra
   , RAlgebra
   )
where

import Data.Functor.Foldable hiding (ListF(..))

type Algebra   f a   = f a -> a
type CoAlgebra f a   = a -> f a
type RAlgebra  f t a = f (t, a) -> a
