{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}

-- | VariantF functor
module Haskus.Utils.VariantF
   ( VariantF (..)
   , ApplyAll
   , pattern FV
   , appendVariantF
   , prependVariantF
   , toVariantFHead
   , toVariantFTail
   , popVariantFHead
   , variantFToValue
   , MapVariantF
   , mapVariantF
   , PopVariantF
   , popVariantF
   , LiftVariantF
   , liftVariantF
   , SplitVariantF
   , splitVariantF
   , variantFToCont
   , variantFToContM
   , contToVariantF
   , contToVariantFM
   -- * Algebras
   -- $algebras
   , AlgebraC (..)
   , FromAlgebraC
   , fromAlgebraC
   , Algebras (..)
   , FromAlgebras
   , fromAlgebras
   -- * R-Algebras
   , RAlgebraC (..)
   , FromRAlgebraC
   , fromRAlgebraC
   , RAlgebras (..)
   , FromRAlgebras
   , fromRAlgebras
   -- * TopDownStop
   , TopDownStopC (..)
   , FromTopDownStopC
   , fromTopDownStopC
   , TopDownStops (..)
   , FromTopDownStops
   , fromTopDownStops
   -- * Reexport
   , NoConstraint
   , module Haskus.Utils.Functor
   )
where

import Haskus.Utils.Variant
import Haskus.Utils.Functor
import Haskus.Utils.Types.List
import Haskus.Utils.Types.Constraint
import Haskus.Utils.ContFlow
import Haskus.Utils.Tuple
import Haskus.Utils.Types
import Haskus.Utils.HList

import Data.Bifunctor
import Control.DeepSeq
import Data.Functor.Classes

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> :set -XPatternSynonyms
-- >>> :set -XDeriveFunctor
-- >>> import Data.Functor.Classes
-- >>>
-- >>> data ConsF a e = ConsF a e deriving (Functor)
-- >>> data NilF    e = NilF      deriving (Functor)
-- >>> type ListF   a = VariantF '[NilF,ConsF a]
-- >>>
-- >>> instance Eq a => Eq1 (ConsF a) where liftEq cmp (ConsF a e1) (ConsF b e2) = a == b && cmp e1 e2
-- >>> instance Eq1 NilF where liftEq _ _ _ = True
-- >>>
-- >>> instance Ord a => Ord1 (ConsF a) where liftCompare cmp (ConsF a e1) (ConsF b e2) = compare a b <> cmp e1 e2
-- >>> instance Ord1 NilF where liftCompare _ _ _ = EQ
-- >>>
-- >>> instance Show a => Show1 (ConsF a) where liftShowsPrec shw _ p (ConsF a e) = showString "ConsF " . showsPrec 10 a . showString " " . shw 10 e
-- >>> instance Show1 NilF where liftShowsPrec _ _ _ _ = showString "NilF"
-- >>>
-- >>> liftEq (==) NilF (NilF :: NilF Int)
-- True
-- >>> liftEq (==) (ConsF 10 "Test") (ConsF 10 "Test" :: ConsF Int String)
-- True
-- >>> liftEq (==) (ConsF 10 "Test") (ConsF 8 "Test" :: ConsF Int String)
-- False
-- >>> liftEq (==) (ConsF 10 "Test") (ConsF 10 "XXX" :: ConsF Int String)
-- False

-- | Recursive Functor-like Variant
newtype VariantF (xs :: [* -> *]) e
   = VariantF (V (ApplyAll e xs))

-- | Apply its first argument to every element of the 2nd arg list
--
-- > ApplyAll e '[f,g,h] ==> '[f e, g e, h e]
--
type family ApplyAll e (xs :: [* -> k]) :: [k] where
   ApplyAll e '[]       = '[]
   ApplyAll e (f ': fs) = f e ': ApplyAll e fs

type instance Base (VariantF xs a) = VariantF xs

-- | Eq instance for VariantF
--
-- >>> let a = FV (ConsF 'a' "Test") :: VariantF '[ConsF Char,NilF] String
-- >>> let a' = FV (ConsF 'a' "XXX") :: VariantF '[ConsF Char,NilF] String
-- >>> let b = FV (ConsF 'b' "Test") :: VariantF '[ConsF Char,NilF] String
-- >>> a == a
-- True
-- >>> a == a'
-- False
-- >>> a == b
-- False
--
-- >>> let c = FV (ConsF 'c' b) :: VariantF '[ConsF Char,NilF] (VariantF '[ConsF Char, NilF] String)
-- >>> c == c
-- True
--
-- >>> let n1 = FV (NilF :: NilF ()) :: VariantF '[ConsF Char,NilF] ()
-- >>> let n2 = FV (NilF :: NilF ()) :: VariantF '[ConsF Char,NilF] ()
-- >>> n1 == n2
-- True
--
instance
   ( Eq1 (VariantF xs)
   , ConstraintAll1 Eq1 xs
   , Eq e
   ) => Eq (VariantF xs e)
   where
   (==) = eq1

-- | Ord instance for VariantF
--
-- >>> let a = FV (ConsF 'a' "Test") :: VariantF '[ConsF Char,NilF] String
-- >>> let a' = FV (ConsF 'a' "XXX") :: VariantF '[ConsF Char,NilF] String
-- >>> let b = FV (ConsF 'b' "Test") :: VariantF '[ConsF Char,NilF] String
-- >>> compare a a
-- EQ
-- >>> compare a a'
-- LT
-- >>> compare a b
-- LT
instance
   ( Ord1 (VariantF xs)
   , ConstraintAll1 Ord1 xs
   , ConstraintAll1 Eq1 xs
   , Ord e
   ) => Ord (VariantF xs e)
   where
   compare = compare1


instance Eq1 (VariantF '[]) where
   liftEq = undefined

instance
   ( Eq1 f
   , Eq1 (VariantF fs)
   ) => Eq1 (VariantF (f:fs)) where
   liftEq cmp x y = case (popVariantFHead x, popVariantFHead y) of
      (Right a, Right b) -> liftEq cmp a b
      (Left a, Left b)   -> liftEq cmp a b
      _                  -> False

instance Ord1 (VariantF '[]) where
   liftCompare = undefined

instance
   ( Ord1 f
   , Ord1 (VariantF fs)
   ) => Ord1 (VariantF (f:fs)) where
   liftCompare cmp x@(VariantF v1) y@(VariantF v2) =
      case (popVariantFHead x, popVariantFHead y) of
         (Right a, Right b) -> liftCompare cmp a b
         (Left  a, Left  b) -> liftCompare cmp a b
         _                  -> compare (variantIndex v1) (variantIndex v2)


instance Show1 (VariantF '[]) where
   liftShowsPrec = undefined

instance
   ( Show1 f
   , Show1 (VariantF fs)
   ) => Show1 (VariantF (f:fs)) where
   liftShowsPrec shw shwl p x = case popVariantFHead x of
         Right a -> liftShowsPrec shw shwl p a
         Left  a -> liftShowsPrec shw shwl p a

-- | Show instance for VariantF
--
-- >>> let a = FV (ConsF 'a' "Test") :: VariantF '[ConsF Char,NilF] String
-- >>> let b = FV (NilF :: NilF String) :: VariantF '[ConsF Char,NilF] String
-- >>> print a
-- ConsF 'a' "Test"
-- >>> print b
-- NilF
instance
   ( Show1 (VariantF xs)
   , ConstraintAll1 Show1 xs
   , Show e
   ) => Show (VariantF xs e)
   where
   showsPrec = showsPrec1

instance Functor (VariantF '[]) where
   fmap _ = undefined

instance (Functor (VariantF fs), Functor f) => Functor (VariantF (f ': fs)) where
   fmap f (VariantF v) = case popVariantHead v of
      Right x -> toVariantFHead (fmap f x)
      Left xs -> toVariantFTail (fmap f (VariantF xs))



-- | Pattern-match in a VariantF
--
-- >>> FV (NilF :: NilF String) :: VariantF '[ConsF Char,NilF] String
-- NilF
pattern FV :: forall c cs e. c :< (ApplyAll e cs) => c -> VariantF cs e
pattern FV x = VariantF (V x)

-- | Retrieve a single value
variantFToValue :: VariantF '[f] e -> f e
variantFToValue (VariantF v) = variantToValue v

appendVariantF :: forall (ys :: [* -> *]) (xs :: [* -> *]) e.
   ( ApplyAll e (Concat xs ys) ~ Concat (ApplyAll e xs) (ApplyAll e ys)
   ) => VariantF xs e -> VariantF (Concat xs ys) e
appendVariantF (VariantF v) = VariantF (appendVariant @(ApplyAll e ys) v)

prependVariantF :: forall (xs :: [* -> *]) (ys :: [* -> *]) e.
   ( ApplyAll e (Concat xs ys) ~ Concat (ApplyAll e xs) (ApplyAll e ys)
   , KnownNat (Length (ApplyAll e xs))
   ) => VariantF ys e -> VariantF (Concat xs ys) e
prependVariantF (VariantF v) = VariantF (prependVariant @(ApplyAll e xs) v)


-- | Set the first value
toVariantFHead :: forall x xs e. x e -> VariantF (x ': xs) e
{-# INLINABLE toVariantFHead #-}
toVariantFHead v = VariantF (toVariantHead @(x e) @(ApplyAll e xs) v)

-- | Set the tail
toVariantFTail :: forall x xs e. VariantF xs e -> VariantF (x ': xs) e
{-# INLINABLE toVariantFTail #-}
toVariantFTail (VariantF v) = VariantF (toVariantTail @(x e) @(ApplyAll e xs) v)

-- | Pop VariantF head
popVariantFHead :: forall x xs e. VariantF (x ': xs) e -> Either (VariantF xs e) (x e)
{-# INLINABLE popVariantFHead #-}
popVariantFHead (VariantF v) = case popVariantHead v of
   Right x -> Right x
   Left xs -> Left (VariantF xs)

type PopVariantF x xs e =
   ( x e :< ApplyAll e xs
   , Remove (x e) (ApplyAll e xs) ~ ApplyAll e (Remove x xs)
   )

-- | Pop VariantF
popVariantF :: forall x xs e.
   ( PopVariantF x xs e
   ) => VariantF xs e -> Either (VariantF (Remove x xs) e) (x e)
{-# INLINABLE popVariantF #-}
popVariantF (VariantF v) = case popVariant v of
   Right x -> Right x
   Left xs -> Left (VariantF xs)

type MapVariantF a b cs ds e =
   ( MapVariant (a e) (b e) (ApplyAll e cs)
   , ds ~ ReplaceNS (IndexesOf a cs) b cs
   , ApplyAll e ds ~ ReplaceNS (IndexesOf (a e) (ApplyAll e cs)) (b e) (ApplyAll e cs)
   )

-- | Map the matching types of a variant
mapVariantF :: forall a b cs ds e.
   ( MapVariantF a b cs ds e
   ) => (a e -> b e) -> VariantF cs e -> VariantF ds e
mapVariantF f (VariantF v) = VariantF (mapVariant @(a e) @(b e) @(ApplyAll e cs) f v)

-- | xs is liftable in ys
type LiftVariantF xs ys e =
   ( LiftVariant (ApplyAll e xs) (ApplyAll e ys)
   )

-- | Lift a VariantF into another
liftVariantF :: forall as bs e.
   ( LiftVariantF as bs e
   ) => VariantF as e -> VariantF bs e
liftVariantF (VariantF v) = VariantF (liftVariant' v)

type SplitVariantF as xs e =
   ( Complement (ApplyAll e xs) (ApplyAll e as) ~ ApplyAll e (Complement xs as)
   , SplitVariant (ApplyAll e as) (ApplyAll e (Complement xs as)) (ApplyAll e xs)
   )

-- | Split a VariantF in two
splitVariantF :: forall as xs e.
   ( SplitVariantF as xs e
   ) => VariantF xs e
     -> Either (VariantF (Complement xs as) e) (VariantF as e)
splitVariantF (VariantF v) = bimap VariantF VariantF (splitVariant v)

-- | Convert a VariantF into a multi-continuation
variantFToCont :: ContVariant (ApplyAll e xs)
   => VariantF xs e -> ContFlow (ApplyAll e xs) r
variantFToCont (VariantF v) = variantToCont v

-- | Convert a VariantF into a multi-continuation
variantFToContM ::
   ( ContVariant (ApplyAll e xs)
   , Monad m
   ) => m (VariantF xs e) -> ContFlow (ApplyAll e xs) (m r)
variantFToContM f = variantToContM (unvariantF <$> f)
   where
      unvariantF (VariantF v) = v

-- | Convert a multi-continuation into a VariantF
contToVariantF :: forall xs e.
   ( ContVariant (ApplyAll e xs)
   ) => ContFlow (ApplyAll e xs) (V (ApplyAll e xs)) -> VariantF xs e
contToVariantF c = VariantF (contToVariant c)

-- | Convert a multi-continuation into a VariantF
contToVariantFM :: forall xs e m.
   ( ContVariant (ApplyAll e xs)
   , Monad m
   ) => ContFlow (ApplyAll e xs) (m (V (ApplyAll e xs))) -> m (VariantF xs e)
contToVariantFM f = VariantF <$> contToVariantM f

instance ContVariant (ApplyAll e xs) => MultiCont (VariantF xs e) where
   type MultiContTypes (VariantF xs e) = ApplyAll e xs
   toCont  = variantFToCont
   toContM = variantFToContM

deriving newtype instance (NFData (V (ApplyAll e xs))) => NFData (VariantF xs e)

----------------------------------------
-- Algebras
----------------------------------------

-- $algebras
--
-- Consider the following example:
--
-- >   data ListF a b = Nil | Cons a b deriving (Functor)
--
-- Typically with this type we would use a F-Algebra:
--
-- >   Algebra (ListF a) b = ListF a b -> b
--
-- Cf https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/
--
--
--
-- But with VariantF we have the following types:
--
-- >   data NilF b    = NilF       deriving Functor
-- >   data ConsF a b = ConsF a b  deriving Functor
-- >   type ListF a   = VariantF [NilF, ConsF a] 
--
-- Hence to implement the F-Algebra of ListF, we would need an F-Algebra like:
--
-- >   Algebras (ListF a) b = (Algebra NilF b, Algebra (ConsF a) b)
--
-- (Note that the size of the tuple depends on the number of constructors)
--
-- A more Haskell-ish approach is to use a type-class "c" to provide the
-- f-algebras of the constructors:
--
-- >   AlgebraC c a = forall f. c f => f a -> a
--
-- We can build the following conversions functions between these algebras:
--
-- >   fromAlgebraC :: AlgebraC c a -> Algebras (VariantF xs) a
-- >   fromAlgebras :: Algebras (VariantF xs) a -> Algebra (VariantF xs) a
--
-- By using these conversions functions we can obtain an Algebra that can be
-- used with the standard recursion-schemes

-------------------

-- | Algebra with a constraint
newtype AlgebraC c a = AlgebraC (forall f. c f => Algebra f a)

-- | Algebras of a VariantF
newtype Algebras v a = Algebras (VariantFAlgebras v a)

type family VariantFAlgebras v a where
   VariantFAlgebras v a = ListToTuple (VariantFAlgebraList v a)

type family VariantFAlgebraList v a where
   VariantFAlgebraList (VariantF fs) a = MakeAlgebras fs a

type family MakeAlgebras (fs :: [Type -> Type]) a where
   MakeAlgebras '[]      a = '[]
   MakeAlgebras (f : fs) a = Algebra f a : MakeAlgebras fs a

type FromAlgebraC xs c a =
   ( FromAlgebraC' (VariantF xs) c a
   , ConstraintAll1 c xs
   , HTuple (MakeAlgebras xs a)
   )

-- | Convert an AlgebraC into Algebras for the specified VariantF
fromAlgebraC :: forall v c xs a.
   ( FromAlgebraC xs c a
   , v ~ VariantF xs
   ) => AlgebraC c a -> Algebras v a
fromAlgebraC a = Algebras (hToTuple (fromAlgebraC' @(VariantF xs) a))

class FromAlgebraC' v c a where
   fromAlgebraC' :: AlgebraC c a -> HList (VariantFAlgebraList v a)

instance FromAlgebraC' (VariantF '[]) c a where
   fromAlgebraC' _ = HNil

instance (FromAlgebraC' (VariantF fs) c a, c f) => FromAlgebraC' (VariantF (f : fs)) c a where
   fromAlgebraC' a@(AlgebraC g) = g `HCons` fromAlgebraC' @(VariantF fs) a


type FromAlgebras fs a =
   ( FromAlgebras' fs
   , HTuple (MakeAlgebras fs a)
   )

-- | Create an Algebra from some VariantF algebras
fromAlgebras :: forall fs a.
   ( FromAlgebras fs a
   ) => Algebras (VariantF fs) a -> Algebra (VariantF fs) a
fromAlgebras (Algebras fs) v = fromAlgebras' @fs (hFromTuple fs) v

class FromAlgebras' fs where
   fromAlgebras' :: HList (VariantFAlgebraList (VariantF fs) a) -> Algebra (VariantF fs) a

instance FromAlgebras' '[] where
   fromAlgebras' _ _ = undefined

instance FromAlgebras' fs => FromAlgebras' (f:fs) where
   {-# INLINABLE fromAlgebras' #-}
   fromAlgebras' (f `HCons` fs) v = case popVariantFHead v of
      Right x -> f x
      Left xs -> fromAlgebras' fs xs


----------------------------------------
-- R-Algebras
----------------------------------------

-- | RAlgebra with a constraint
newtype RAlgebraC c t a = RAlgebraC (forall f. c f => RAlgebra f t a)

-- | RAlgebras of a VariantF
newtype RAlgebras v t a = RAlgebras (VariantFRAlgebras v t a)

type family VariantFRAlgebras v t a where
   VariantFRAlgebras v t a = ListToTuple (VariantFRAlgebraList v t a)

type family VariantFRAlgebraList v t a where
   VariantFRAlgebraList (VariantF fs) t a = MakeRAlgebras fs t a

type family MakeRAlgebras (fs :: [Type -> Type]) t a where
   MakeRAlgebras '[]      t a = '[]
   MakeRAlgebras (f : fs) t a = RAlgebra f t a : MakeRAlgebras fs t a

type FromRAlgebraC xs t c a =
   ( FromRAlgebraC' (VariantF xs) t c a
   , ConstraintAll1 c xs
   , HTuple (MakeRAlgebras xs t a)
   )

-- | Convert an RAlgebraC into RAlgebras for the specified VariantF
fromRAlgebraC :: forall v c t xs a.
   ( FromRAlgebraC xs t c a
   , v ~ VariantF xs
   ) => RAlgebraC c t a -> RAlgebras v t a
fromRAlgebraC a = RAlgebras (hToTuple (fromRAlgebraC' @(VariantF xs) @t a))

class FromRAlgebraC' v t c a where
   fromRAlgebraC' :: RAlgebraC c t a -> HList (VariantFRAlgebraList v t a)

instance FromRAlgebraC' (VariantF '[]) t c a where
   fromRAlgebraC' _ = HNil

instance (FromRAlgebraC' (VariantF fs) t c a, c f) => FromRAlgebraC' (VariantF (f : fs)) t c a where
   fromRAlgebraC' a@(RAlgebraC g) = g `HCons` fromRAlgebraC' @(VariantF fs) a


type FromRAlgebras fs t a =
   ( FromRAlgebras' fs t
   , HTuple (MakeRAlgebras fs t a)
   )

-- | Create an RAlgebra from some VariantF algebras
fromRAlgebras :: forall fs t a.
   ( FromRAlgebras fs t a
   ) => RAlgebras (VariantF fs) t a -> RAlgebra (VariantF fs) t a
fromRAlgebras (RAlgebras fs) v = fromRAlgebras' @fs @t (hFromTuple fs) v

class FromRAlgebras' fs t where
   fromRAlgebras' :: HList (MakeRAlgebras fs t a) -> RAlgebra (VariantF fs) t a

instance FromRAlgebras' '[] t where
   fromRAlgebras' _ _ = undefined

instance FromRAlgebras' fs t => FromRAlgebras' (f:fs) t where
   {-# INLINABLE fromRAlgebras' #-}
   fromRAlgebras' (f `HCons` fs) v = case popVariantFHead v of
      Right x -> f x
      Left xs -> fromRAlgebras' fs xs

----------------------------------------
-- TopDownStop
----------------------------------------

newtype TopDownStopC c a = TopDownStopC (forall f. c f => TopDownStop a f)

newtype TopDownStops v a = TopDownStops (VariantFTopDownStops v a)

type family VariantFTopDownStops v a where
   VariantFTopDownStops v a = ListToTuple (VariantFTopDownStopList v a)

type family VariantFTopDownStopList v a where
   VariantFTopDownStopList (VariantF fs) a = MakeTopDownStops fs a

type family MakeTopDownStops (fs :: [Type -> Type]) a where
   MakeTopDownStops '[]      a = '[]
   MakeTopDownStops (f : fs) a = TopDownStop a f : MakeTopDownStops fs a

type FromTopDownStopC xs c a =
   ( FromTopDownStopC' (VariantF xs) c a
   , ConstraintAll1 c xs
   , HTuple (MakeTopDownStops xs a)
   )

-- | Convert an TopDownStopC into TopDownStops for the specified VariantF
fromTopDownStopC :: forall v c xs a.
   ( FromTopDownStopC xs c a
   , v ~ VariantF xs
   ) => TopDownStopC c a -> TopDownStops v a
fromTopDownStopC a = TopDownStops (hToTuple (fromTopDownStopC' @(VariantF xs) a))

class FromTopDownStopC' v c a where
   fromTopDownStopC' :: TopDownStopC c a -> HList (VariantFTopDownStopList v a)

instance FromTopDownStopC' (VariantF '[]) c a where
   fromTopDownStopC' _ = HNil

instance (FromTopDownStopC' (VariantF fs) c a, c f) => FromTopDownStopC' (VariantF (f : fs)) c a where
   fromTopDownStopC' a@(TopDownStopC g) = g `HCons` fromTopDownStopC' @(VariantF fs) a


type FromTopDownStops fs a =
   ( FromTopDownStops' fs
   , HTuple (MakeTopDownStops fs a)
   )

-- | Create an TopDownStop from some VariantF algebras
fromTopDownStops :: forall fs a.
   ( FromTopDownStops fs a
   ) => TopDownStops (VariantF fs) a -> TopDownStop a (VariantF fs)
fromTopDownStops (TopDownStops fs) v = fromTopDownStops' @fs (hFromTuple fs) v

class FromTopDownStops' fs where
   fromTopDownStops' :: HList (MakeTopDownStops fs a) -> TopDownStop a (VariantF fs)

instance FromTopDownStops' '[] where
   fromTopDownStops' _ _ = undefined

instance
   ( FromTopDownStops' fs
   ) => FromTopDownStops' (f:fs) where
   {-# INLINABLE fromTopDownStops' #-}
   fromTopDownStops' (f `HCons` fs) v = case popVariantFHead v of
      Right x -> first toVariantFHead (f x)
      Left xs -> first (prependVariantF @'[f]) (fromTopDownStops' fs xs)
