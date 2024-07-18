{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functor and recursion schemes
--
-- Simple API is intended to be easier to understand (e.g. they don't use
-- xxmorphism and xxxalgebra jargon but tree-traversal-like terms).
module Haskus.Utils.Functor
   ( -- * Simple API
     BottomUpT
   , bottomUp
   , BottomUpOrigT
   , bottomUpOrig
   , TopDownStopT
   , topDownStop
   -- * Recursion schemes
   , module Data.Functor.Classes
   , module Data.Functor.Foldable
   , Algebra
   , CoAlgebra
   , RAlgebra
   , RCoAlgebra
   -- * Higher-order recursion schemes
   , type (~>)
   , type NatM
   , HBase
   , HAlgebra
   , HAlgebraM
   , HGAlgebra
   , HGAlgebraM
   , HCoalgebra
   , HCoalgebraM
   , HGCoalgebra
   , HGCoalgebraM
   , HFunctor (..)
   , HFoldable (..)
   , HTraversable (..)
   , HRecursive (..)
   , HCorecursive (..)
   , hhylo
   , hcataM
   , hlambek
   , hpara
   , hparaM
   , hanaM
   , hcolambek
   , hapo
   , hapoM
   , hhyloM
   )
where

import Data.Functor.Foldable hiding (ListF(..))
import Data.Functor.Classes
import Data.Functor.Sum
import Data.Functor.Product
import Control.Monad

import Haskus.Utils.Types (Type)

-------------------------------------------
-- Simple API
-------------------------------------------

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


-------------------------------------------
-- Recursion schemes
-------------------------------------------

type Algebra    f a   = f a -> a
type CoAlgebra  f a   = a -> f a
type RAlgebra   f t a = f (t, a) -> a
type RCoAlgebra f t a = a -> f (Either t a)


-------------------------------------------
-- Higher-order
-------------------------------------------


type f ~> g = forall a. f a -> g a
type NatM m f g = forall a. f a -> m (g a)

type family HBase (h :: k -> Type) :: (k -> Type) -> (k -> Type)

type HAlgebra h f = h f ~> f
type HAlgebraM m h f = NatM m (h f) f
type HGAlgebra w h a = h (w a) ~> a
type HGAlgebraM w m h a = NatM m (h (w a)) a

type HCoalgebra h f = f ~> h f
type HCoalgebraM m h f = NatM m f (h f)
type HGCoalgebra m h a = a ~> h (m a)
type HGCoalgebraM n m h a = NatM m a (h (n a))

class HFunctor (h :: (k -> Type) -> (k -> Type)) where
  hfmap :: (f ~> g) -> h f ~> h g

class HFunctor h => HFoldable (h :: (k -> Type) -> (k -> Type)) where
  hfoldMap :: Monoid m => (forall b. f b -> m) -> h f a -> m

class HFoldable h => HTraversable (h :: (k -> Type) -> (k -> Type))  where
  htraverse :: Applicative e => NatM e f g -> NatM e (h f) (h g)

class HFunctor (HBase h) => HRecursive (h :: k -> Type) where
  hproject :: HCoalgebra (HBase h) h

  hcata :: HAlgebra (HBase h) f -> h ~> f
  hcata algebra = algebra . hfmap (hcata algebra) . hproject

class HFunctor (HBase h) => HCorecursive (h :: k -> Type) where
  hembed :: HAlgebra (HBase h) h

  hana :: HCoalgebra (HBase h) f -> f ~> h
  hana coalgebra = hembed . hfmap (hana coalgebra) . coalgebra

hhylo :: HFunctor f => HAlgebra f b -> HCoalgebra f a -> a ~> b
hhylo f g = f . hfmap (hhylo f g) . g

hcataM :: (Monad m, HTraversable (HBase h), HRecursive h) => HAlgebraM m (HBase h) f -> h a -> m (f a)
hcataM f = f <=< htraverse (hcataM f) . hproject

hlambek :: (HRecursive h, HCorecursive h) => HCoalgebra (HBase h) h
hlambek = hcata (hfmap hembed)

hpara :: (HFunctor (HBase h), HRecursive h) => HGAlgebra (Product h) (HBase h) a -> h ~> a
hpara phi = phi . hfmap (\a -> Pair a (hpara phi a)) . hproject

hparaM :: (HTraversable (HBase h), HRecursive h, Monad m) => HGAlgebraM (Product h) m (HBase h) a -> NatM m h a
hparaM phiM = phiM <=< htraverse (\a -> liftA2 Pair (pure a) (hparaM phiM a)) . hproject

hanaM :: (Monad m, HTraversable (HBase h), HCorecursive h) => HCoalgebraM m (HBase h) f -> f a -> m (h a)
hanaM f = fmap hembed . htraverse (hanaM f) <=< f

hcolambek :: HRecursive h => HCorecursive h => HAlgebra (HBase h) h
hcolambek = hana (hfmap hproject)

hapo :: HCorecursive h => HGCoalgebra (Sum h) (HBase h) a -> a ~> h
hapo psi = hembed . hfmap (coproduct id (hapo psi)) . psi
  where
    coproduct f _ (InL a) = f a
    coproduct _ g (InR a) = g a

hapoM :: (HCorecursive h, HTraversable (HBase h), Monad m) => HGCoalgebraM (Sum h) m (HBase h) a -> NatM m a h
hapoM psiM = fmap hembed . htraverse (coproduct pure (hapoM psiM)) <=< psiM
  where
    coproduct f _ (InL a) = f a
    coproduct _ g (InR a) = g a

hhyloM :: (HTraversable t, Monad m) => HAlgebraM m t h -> HCoalgebraM m t f -> f a -> m (h a)
hhyloM f g = f <=< htraverse(hhyloM f g) <=< g
