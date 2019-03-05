-- | Functor helpers
module Haskus.Utils.Functor
   ( module Data.Functor.Foldable
   , Algebra
   , RAlgebra
   )
where

import Data.Functor.Foldable hiding (ListF(..))

type Algebra  f a   = f a -> a
type RAlgebra f t a = f (t, a) -> a
