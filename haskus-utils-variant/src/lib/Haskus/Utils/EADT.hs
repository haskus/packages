{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Extensible ADT
module Haskus.Utils.EADT
   ( VariantF (..)
   , ApplyAll
   , pattern FV
   , appendVariantF
   , toVariantFHead
   , toVariantFTail
   , popVariantFHead
   , popVariantF
   , mapVariantF
   , variantFToValue
   , LiftableF
   , liftVariantF
   , AlterVariantF
   , alterVariantF
   , AlgVariantF
   , algVariantF
   , splitVariantF
   , variantFToCont
   , variantFToContM
   , contToVariantF
   , contToVariantFM
   -- * Extensible ADT
   , EADT
   , (:<:)
   , pattern VF
   , appendEADT
   , liftEADT
   , popEADT
   , AlterEADT
   , alterEADT
   , AlgEADT
   , algEADT
   , eadtToCont
   , eadtToContM
   , contToEADT
   , contToEADTM
   -- * Reexport
   , NoConstraint
   , module Haskus.Utils.Functor
   )
where

import Haskus.Utils.Variant
import Haskus.Utils.Functor
import Haskus.Utils.Types.List
import Haskus.Utils.Types
import Haskus.Utils.ContFlow

import Unsafe.Coerce
import Data.Bifunctor
import GHC.Exts (Any,Constraint)

-- | Recursive Functor-like Variant
newtype VariantF (xs :: [* -> *]) e
   = VariantF (V (ApplyAll e xs))

-- | Apply its first argument to every element of the 2nd arg list
--
-- > ApplyAll e '[f,g,h] ==> '[f e, g e, h e]
--
type family ApplyAll e (xs :: [* -> *]) :: [*] where
   ApplyAll e '[]       = '[]
   ApplyAll e (f ': fs) = f e ': ApplyAll e fs

instance (Show (V (ApplyAll e xs))) => Show (VariantF xs e) where
   show (VariantF x) = show x
deriving instance (Eq (V (ApplyAll e xs))) => Eq (VariantF xs e)
deriving instance (Ord (V (ApplyAll e xs))) => Ord (VariantF xs e)

instance Functor (VariantF '[]) where
   fmap _ = undefined

instance (Functor (VariantF fs), Functor f) => Functor (VariantF (f ': fs)) where
   fmap f (VariantF v) = case popVariantHead v of
      Right x -> toVariantFHead (fmap f x)
      Left xs -> toVariantFTail (fmap f (VariantF xs))

-- | Pattern-match in a VariantF
pattern FV :: forall c cs e. c :< (ApplyAll e cs) => c -> VariantF cs e
pattern FV x = VariantF (V x)

-- | Retrieve a single value
variantFToValue :: VariantF '[f] e -> f e
variantFToValue (VariantF v) = variantToValue v

appendVariantF :: forall (ys :: [* -> *]) (xs :: [* -> *]) e.
   ( ApplyAll e (Concat xs ys) ~ Concat (ApplyAll e xs) (ApplyAll e ys)
   ) => VariantF xs e -> VariantF (Concat xs ys) e
appendVariantF (VariantF v) = VariantF (appendVariant @(ApplyAll e ys) v)

-- | Set the first value
toVariantFHead :: forall x xs e. x e -> VariantF (x ': xs) e
{-# INLINE toVariantFHead #-}
toVariantFHead v = VariantF (toVariantHead @(x e) @(ApplyAll e xs) v)

-- | Set the tail
toVariantFTail :: forall x xs e. VariantF xs e -> VariantF (x ': xs) e
{-# INLINE toVariantFTail #-}
toVariantFTail (VariantF v) = VariantF (toVariantTail @(x e) @(ApplyAll e xs) v)

-- | Pop VariantF head
popVariantFHead :: forall x xs e. VariantF (x ': xs) e -> Either (VariantF xs e) (x e)
{-# INLINE popVariantFHead #-}
popVariantFHead (VariantF v) = case popVariantHead v of
   Right x -> Right x
   Left xs -> Left (VariantF xs)

-- | Pop VariantF
popVariantF :: forall x xs ys e.
   ( x e :< ApplyAll e xs
   , Filter (x e) (ApplyAll e xs) ~ ApplyAll e ys
   ) => VariantF xs e -> Either (VariantF ys e) (x e)
{-# INLINE popVariantF #-}
popVariantF (VariantF v) = case popVariant v of
   Right x -> Right x
   Left xs -> Left (VariantF xs)

-- | Map the matching types of a variant
mapVariantF :: forall a b cs e ds as.
   ( MapVariant (a e) (b e) as
   , ds ~ ReplaceNS (IndexesOf a cs) b cs
   , as ~ ApplyAll e cs
   , ApplyAll e ds ~ ReplaceNS (IndexesOf (a e) as) (b e) as
   ) => (a e -> b e) -> VariantF cs e -> VariantF ds e
mapVariantF f (VariantF v) = VariantF (mapVariant @(a e) @(b e) @as f v)

-- | xs is liftable in ys
type LiftableF e xs ys =
   ( IsSubset xs ys ~ 'True
   , LiftVariant (ApplyAll e xs) (ApplyAll e ys)
   )

-- | Lift a VariantF into another
liftVariantF :: forall e as bs.
   ( LiftableF e as bs
   ) => VariantF as e -> VariantF bs e
liftVariantF (VariantF v) = VariantF (liftVariant' v)

class AlterVariantF (c :: (* -> *) -> Constraint) e (xs :: [* -> *]) where
   alterVariantF' :: (forall (f :: * -> *). c f => f e -> f e) -> Word -> Any -> Any

instance AlterVariantF c e '[] where
   {-# INLINE alterVariantF' #-}
   alterVariantF' _ = undefined

instance
   ( AlterVariantF c e xs
   , c x
   ) => AlterVariantF c e (x ': xs)
   where
      {-# INLINE alterVariantF' #-}
      alterVariantF' f t v =
         case t of
            0 -> unsafeCoerce (f (unsafeCoerce v :: x e))
            n -> alterVariantF' @c @e @xs f (n-1) v

-- | Alter a variant. You need to specify the constraints required by the
-- modifying function.
--
-- Usage:
--
-- >   alterVariantF @NoConstraint id         v
-- >   alterVariantF @Resizable    (resize 4) v
-- >
-- >   -- Multiple constraints:
-- >   class (Ord a, Num a) => OrdNum a
-- >   instance (Ord a, Num a) => OrdNum a
-- >   alterVariantF @OrdNum foo v
--
alterVariantF :: forall c e (xs :: [* -> *]).
   ( AlterVariantF c e xs
   ) => (forall (f :: * -> *). c f => f e -> f e) -> VariantF xs e -> VariantF xs e
{-# INLINABLE alterVariantF #-}
alterVariantF f (VariantF (Variant t a)) =
   VariantF (Variant t (alterVariantF' @c @e @xs f t a))


class AlgVariantF (c :: (* -> *) -> Constraint) e (xs :: [* -> *]) where
   algVariantF' :: (forall (f :: * -> *). c f => f e -> e) -> Word -> Any -> e

instance AlgVariantF c e '[] where
   {-# INLINE algVariantF' #-}
   algVariantF' _ = undefined

instance
   ( AlgVariantF c e xs
   , c x
   ) => AlgVariantF c e (x ': xs)
   where
      {-# INLINE algVariantF' #-}
      algVariantF' f t v =
         case t of
            0 -> f (unsafeCoerce v :: x e)
            n -> algVariantF' @c @e @xs f (n-1) v

-- | Apply an algebra to a VariantF. You need to specify the constraints
-- required by the modifying function.
--
-- Usage:
--
-- >  algVariantF @NoConstraint id         v
-- >  algVariantF @Resizable    (resize 4) v
--
algVariantF :: forall c e (xs :: [* -> *]).
   ( AlgVariantF c e xs
   ) => (forall (f :: * -> *). c f => f e -> e) -> VariantF xs e -> e
{-# INLINABLE algVariantF #-}
algVariantF f (VariantF (Variant t a)) = algVariantF' @c @e @xs f t a


-- | Split a VariantF in two
splitVariantF :: forall as xs e.
   ( Complement (ApplyAll e xs) (ApplyAll e as) ~ ApplyAll e (Complement xs as)
   , SplitVariant (ApplyAll e as) (ApplyAll e xs) (ApplyAll e xs)
   ) => VariantF xs e
     -> Either (VariantF as e) (VariantF (Complement xs as) e)
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

--------------------------------------------
-- Extensible ADT
--------------------------------------------

-- | An extensible ADT
type EADT xs = Fix (VariantF xs)

type family f :<: xs where
   f :<: xs = EADTF' f (EADT xs) xs

type EADTF' f e cs =
   ( Member' f cs
   , Index (IndexOf (f e) (ApplyAll e cs)) (ApplyAll e cs) ~ f e
   , PopVariant (f e) (ApplyAll e cs)
   , KnownNat (IndexOf (f e) (ApplyAll e cs))
   )

-- | Pattern-match in an extensible ADT
pattern VF :: forall e f cs.
   ( e ~ EADT cs  -- allow easy use of TypeApplication to set the EADT type
   , f :<: cs     -- constraint synonym ensuring `f` is in `cs`
   ) => f (EADT cs) -> EADT cs
pattern VF x = Fix (VariantF (VSilent x))
   -- `VSilent` matches a variant value without checking the membership: we
   -- already do it with :<:

-- | Append new "constructors" to the EADT
appendEADT :: forall ys xs zs.
   ( zs ~ Concat xs ys
   , ApplyAll (EADT zs) zs ~ Concat (ApplyAll (EADT zs) xs) (ApplyAll (EADT zs) ys)
   , Functor (VariantF xs)
   ) => EADT xs -> EADT zs
appendEADT (Fix v) = Fix (appendVariantF @ys (fmap (appendEADT @ys) v))

-- | Lift an EADT into another
liftEADT :: forall e as bs.
   ( e ~ Fix (VariantF bs)
   , LiftableF e as bs
   , Functor (VariantF as)
   ) => EADT as -> EADT bs
liftEADT = cata (Fix . liftVariantF)

-- | Pop an EADT value
popEADT :: forall xs f e.
   ( f :<: xs
   , e ~ EADT xs
   , f e :< ApplyAll e xs
   , Filter (f e) (ApplyAll e xs) ~ ApplyAll e (Filter f xs)
   ) => EADT xs -> Either (VariantF (Filter f xs) (EADT xs)) (f (EADT xs))
popEADT (Fix v) = popVariantF v

type AlterEADT c xs = AlterVariantF c (EADT xs) xs

-- | Alter an EADT value
alterEADT :: forall c xs.
   ( AlterEADT c xs
   ) => (forall f. c f => f (EADT xs) -> f (EADT xs)) -> EADT xs -> EADT xs
alterEADT f (Fix v) = Fix (alterVariantF @c @(EADT xs) f v)

type AlgEADT c xs = AlgVariantF c (EADT xs) xs

-- | Apply an algebra to an EADT value
algEADT :: forall c xs.
   ( AlgEADT c xs
   ) => (forall f. c f => f (EADT xs) -> EADT xs) -> EADT xs -> EADT xs
algEADT f (Fix v) = algVariantF @c @(EADT xs) f v

-- | Convert an EADT into a multi-continuation
eadtToCont ::
   ( ContVariant (ApplyAll (Fix (VariantF xs)) xs)
   ) => Fix (VariantF xs) -> ContFlow (ApplyAll (Fix (VariantF xs)) xs) r
eadtToCont (Fix v) = variantFToCont v

-- | Convert an EADT into a multi-continuation
eadtToContM ::
   ( ContVariant (ApplyAll (Fix (VariantF xs)) xs)
   , Monad m
   ) => m (Fix (VariantF xs))
     -> ContFlow (ApplyAll (Fix (VariantF xs)) xs) (m r)
eadtToContM f = variantFToContM (unfix <$> f)

-- | Convert a multi-continuation into an EADT
contToEADT ::
   ( ContVariant (ApplyAll (Fix (VariantF xs)) xs)
   ) => ContFlow (ApplyAll (Fix (VariantF xs)) xs)
                 (V (ApplyAll (Fix (VariantF xs)) xs))
     -> Fix (VariantF xs)
contToEADT c = Fix (contToVariantF c)

-- | Convert a multi-continuation into an EADT
contToEADTM ::
   ( ContVariant (ApplyAll (Fix (VariantF xs)) xs)
   , Monad f
   ) => ContFlow (ApplyAll (Fix (VariantF xs)) xs)
                 (f (V (ApplyAll (Fix (VariantF xs)) xs)))
     -> f (Fix (VariantF xs))
contToEADTM f = Fix <$> contToVariantFM f
