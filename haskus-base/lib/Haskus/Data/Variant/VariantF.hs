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
module Haskus.Data.Variant.VariantF
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
   , BottomUpF
   , BottomUp (..)
   , BottomUpOrig (..)
   , BottomUpOrigF
   , TopDownStop (..)
   , TopDownStopF
   -- * Reexport
   , NoConstraint
   , module Haskus.Utils.Functor
   )
where

import Haskus.Data.Variant
import Haskus.Utils.Functor
import Haskus.Utils.Types.List
import Haskus.Utils.Types.Constraint
import Haskus.Utils.ContFlow
import Haskus.Utils.Types

import Data.Bifunctor
import Control.DeepSeq

-- $setup
-- >>> :seti -XDataKinds
-- >>> :seti -XTypeApplications
-- >>> :seti -XTypeOperators
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeFamilies
-- >>> :seti -XPatternSynonyms
-- >>> :seti -XDeriveFunctor
-- >>> import Data.Functor.Classes
-- >>>
-- >>> data ConsF a e = ConsF a e deriving (Eq,Ord,Show,Functor)
-- >>> data NilF    e = NilF      deriving (Eq,Ord,Show,Functor)
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
newtype VariantF (xs :: [t -> Type]) (e :: t)
   = VariantF (V (ApplyAll e xs))

-- | Apply its first argument to every element of the 2nd arg list
--
-- > ApplyAll e '[f,g,h] ==> '[f e, g e, h e]
--
type family ApplyAll (e :: t) (xs :: [t -> k]) :: [k] where
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
   , ConstraintAll1 Eq1 fs
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
   , ConstraintAll1 Eq1 fs
   , ConstraintAll1 Ord1 fs
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
   , ConstraintAll1 Show1 fs
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

appendVariantF :: forall (ys :: [Type -> Type]) (xs :: [Type -> Type]) e.
   ( ApplyAll e (Concat xs ys) ~ Concat (ApplyAll e xs) (ApplyAll e ys)
   ) => VariantF xs e -> VariantF (Concat xs ys) e
appendVariantF (VariantF v) = VariantF (appendVariant @(ApplyAll e ys) v)

prependVariantF :: forall (xs :: [Type -> Type]) (ys :: [Type -> Type]) e.
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
-- BottomUp
----------------------------------------

type family BottomUpF c fs :: Constraint where
   BottomUpF c fs = (Functor (VariantF fs), BottomUp c fs)

class BottomUp c fs where
   toBottomUp :: (forall f. c f => f a -> b) -> (VariantF fs a -> b)

instance BottomUp c '[] where
   {-# INLINABLE toBottomUp #-}
   toBottomUp _f = undefined

instance forall c fs f.
   ( BottomUp c fs
   , c f
   ) => BottomUp c (f ':fs) where
   {-# INLINABLE toBottomUp #-}
   toBottomUp f v = case popVariantFHead v of
      Right x -> f x
      Left xs -> toBottomUp @c f xs

----------------------------------------
-- BottomUpOrig
----------------------------------------

type family BottomUpOrigF c fs :: Constraint where
   BottomUpOrigF c fs = (Functor (VariantF fs), BottomUpOrig c fs)

class BottomUpOrig c fs where
   toBottomUpOrig :: (forall f. c f => f (t,a) -> b) -> (VariantF fs (t,a) -> b)

instance BottomUpOrig c '[] where
   {-# INLINABLE toBottomUpOrig #-}
   toBottomUpOrig _f = undefined

instance forall c fs f.
   ( BottomUpOrig c fs
   , c f
   ) => BottomUpOrig c (f ': fs) where
   {-# INLINABLE toBottomUpOrig #-}
   toBottomUpOrig f v = case popVariantFHead v of
      Right x -> f x
      Left xs -> toBottomUpOrig @c f xs


----------------------------------------
-- TopDownStop
----------------------------------------

type family TopDownStopF c fs :: Constraint where
   TopDownStopF c fs = (Functor (VariantF fs), TopDownStop c fs)

class TopDownStop c fs where
   toTopDownStop :: (forall f. c f => TopDownStopT a f) -> TopDownStopT a (VariantF fs)

instance TopDownStop c '[] where
   {-# INLINABLE toTopDownStop #-}
   toTopDownStop _f = undefined

instance forall c fs f.
   ( TopDownStop c fs
   , Functor f
   , c f
   ) => TopDownStop c (f ':fs) where
   {-# INLINABLE toTopDownStop #-}
   toTopDownStop f v = case popVariantFHead v of
      Right x -> first toVariantFHead (f x)
      Left xs -> first (prependVariantF @'[f]) (toTopDownStop @c f xs)
