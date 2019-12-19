{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Functor helpers
module Haskus.Utils.Functor
   ( module Data.Functor.Foldable
   , module Data.Functor.Classes
   , BottomUpT
   , bottomUp
   , BottomUpOrigT
   , bottomUpOrig
   , TopDownStopT
   , topDownStop
   -- * Others
   , Algebra
   , CoAlgebra
   , RAlgebra
   , RCoAlgebra
   )
where

import Data.Functor.Foldable hiding (ListF(..))
import Data.Functor.Classes

type Algebra    f a   = f a -> a
type CoAlgebra  f a   = a -> f a
type RAlgebra   f t a = f (t, a) -> a
type RCoAlgebra f t a = a -> f (Either t a)

type BottomUpT       a f = f a -> a
type BottomUpOrigT t a f = f (t,a) -> a
type TopDownStopT    a f = f a -> Either (f a) a

-- | Bottom-up traversal (catamorphism)
bottomUp :: (Recursive t) => (Base t a -> a) -> t -> a
bottomUp f t = cata f t

-- | Bottom-up traversal with original value (paramorphism)
bottomUpOrig :: (Recursive t) => (Base t (t,a) -> a) -> t -> a
bottomUpOrig f t = para f t

-- | Perform a top-down traversal
--
-- Right: stop the traversal ("right" value obtained)
-- Left: continue the traversal recursively on the new value
topDownStop :: (Recursive t, Corecursive t) => (Base t t -> Either (Base t t) t) -> t -> t
topDownStop f t = go t
   where
      go w = case f (project w) of
         Right x -> x                 -- stop here
         Left  x -> embed (fmap go x) -- continue recursively
