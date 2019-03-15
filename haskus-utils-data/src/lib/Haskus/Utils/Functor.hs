-- | Functor helpers
module Haskus.Utils.Functor
   ( module Data.Functor.Foldable
   , Algebra
   , CoAlgebra
   , RAlgebra
   , RCoAlgebra
   , TopDownStop
   , topDownStop
   )
where

import Data.Functor.Foldable hiding (ListF(..))

type Algebra    f a   = f a -> a
type CoAlgebra  f a   = a -> f a
type RAlgebra   f t a = f (t, a) -> a
type RCoAlgebra f t a = a -> f (Either t a)

-- | Perform a top-down traversal
--
-- Right: stop the traversal ("right" value obtained)
-- Left: continue the traversal recursively on the new value
type TopDownStop a f = f a -> Either (f a) a

topDownStop :: (Recursive t, Corecursive t) => TopDownStop t (Base t) -> t -> t
topDownStop f t = go t
   where
      go w = case f (project w) of
         Right x -> x                 -- stop here
         Left  x -> embed (fmap go x) -- continue recursively
