-- | Functor helpers
module Haskus.Utils.Functor
   ( module Data.Functor.Foldable
   , Algebra
   )
where

import Data.Functor.Foldable hiding (ListF(..))

type Algebra f a = f a -> a
