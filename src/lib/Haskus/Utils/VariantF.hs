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

-- | Functor Variant
module Haskus.Utils.VariantF
   ( VariantF (..)
   , FixV
   , pattern FV
   , appendVariantF
   , toVariantFHead
   , toVariantFTail
   , popVariantFHead
   , mapVariantF
   , LiftableF
   , liftVariantF
   -- * Extensible ADT
   , EADT
   , (:<:)
   , pattern VF
   , appendEADT
   , liftEADT
   -- * Reexport
   , module Haskus.Utils.Functor
   )
where

import Haskus.Utils.Variant
import Haskus.Utils.Functor
import Haskus.Utils.Types.List
import Haskus.Utils.Types

-- | Recursive Functor-like Variant
newtype VariantF (xs :: [* -> *]) e
   = VariantF (V (FixV e xs))

type family FixV e (xs :: [* -> *]) :: [*] where
   FixV e '[]       = '[]
   FixV e (f ': fs) = f e ': FixV e fs

instance (Show (V (FixV e xs))) => Show (VariantF xs e) where
   show (VariantF x) = show x
deriving instance (Eq (V (FixV e xs))) => Eq (VariantF xs e)
deriving instance (Ord (V (FixV e xs))) => Ord (VariantF xs e)

instance Functor (VariantF '[]) where
   fmap _ = undefined

instance (Functor (VariantF fs), Functor f) => Functor (VariantF (f ': fs)) where
   fmap f (VariantF v) = case popVariantHead v of
      Right x -> toVariantFHead (fmap f x)
      Left xs -> toVariantFTail (fmap f (VariantF xs))

-- | Pattern-match in a VariantF
pattern FV :: forall c cs e. Popable c (FixV e cs) => c -> VariantF cs e
pattern FV x = VariantF (V x)

appendVariantF :: forall (ys :: [* -> *]) (xs :: [* -> *]) e.
   ( FixV e (Concat xs ys) ~ Concat (FixV e xs) (FixV e ys)
   ) => VariantF xs e -> VariantF (Concat xs ys) e
appendVariantF (VariantF v) = VariantF (appendVariant @(FixV e ys) v)

-- | Set the first value
toVariantFHead :: forall x xs e. x e -> VariantF (x ': xs) e
{-# INLINE toVariantFHead #-}
toVariantFHead v = VariantF (toVariantHead @(x e) @(FixV e xs) v)

-- | Set the tail
toVariantFTail :: forall x xs e. VariantF xs e -> VariantF (x ': xs) e
{-# INLINE toVariantFTail #-}
toVariantFTail (VariantF v) = VariantF (toVariantTail @(x e) @(FixV e xs) v)

popVariantFHead :: forall x xs e. VariantF (x ': xs) e -> Either (VariantF xs e) (x e)
{-# INLINE popVariantFHead #-}
popVariantFHead (VariantF v) = case popVariantHead v of
   Right x -> Right x
   Left xs -> Left (VariantF xs)

-- | Map the matching types of a variant
mapVariantF :: forall a b cs e ds as.
   ( MappableVariant (a e) (b e) as
   , ds ~ ReplaceNS (IndexesOf a cs) b cs
   , as ~ FixV e cs
   , FixV e ds ~ ReplaceNS (IndexesOf (a e) as) (b e) as
   ) => (a e -> b e) -> VariantF cs e -> VariantF ds e
mapVariantF f (VariantF v) = VariantF (mapVariant @(a e) @(b e) @as f v)

-- | xs is liftable in ys
type LiftableF e xs ys =
   ( IsSubset xs ys ~ 'True
   , LiftVariant (FixV e xs) (FixV e ys)
   )

-- | Lift a VariantF into another
liftVariantF :: forall e as bs.
   ( LiftableF e as bs
   ) => VariantF as e -> VariantF bs e
liftVariantF (VariantF v) = VariantF (liftVariant' v)

--------------------------------------------
-- Extensible ADT
--------------------------------------------

-- | An extensible ADT
type EADT xs = Fix (VariantF xs)

type family f :<: xs where
   f :<: xs = EADTF' f (EADT xs) xs

type EADTF' f e cs =
   ( Member' f cs
   , Index (IndexOf (f e) (FixV e cs)) (FixV e cs) ~ f e
   , PopVariant (f e) (FixV e cs)
   , KnownNat (IndexOf (f e) (FixV e cs))
   )

-- | Pattern-match in an extensible ADT
pattern VF :: forall e f cs.
   ( e ~ EADT cs  -- allow easy use of TypeApplication to set the EADT type
   , f :<: cs     -- constraint synonym ensuring `f` is in `cs`
   ) => f (EADT cs) -> EADT cs
pattern VF x = Fix (VariantF (V' x))   -- `V'` match a variant value (without
                                       -- checking the membership: we already
                                       -- do it with :<:)

-- | Append new "constructors" to the EADT
appendEADT :: forall ys xs zs.
   ( zs ~ Concat xs ys
   , FixV (EADT zs) zs ~ Concat (FixV (EADT zs) xs) (FixV (EADT zs) ys)
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
