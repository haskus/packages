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
   -- * Extensible ADT
   , EADT
   , (:<:)
   , pattern VF
   , appendEADT
   , liftEADT
   , popEADT
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
   = VariantF (V (ApplyAll e xs))

-- | `ApplyAll e '[f,g,h] ==> '[f e, g e, h e]`
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
pattern FV :: forall c cs e. Popable c (ApplyAll e cs) => c -> VariantF cs e
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
   ( Popable (x e) (ApplyAll e xs)
   , Filter (x e) (ApplyAll e xs) ~ ApplyAll e ys
   ) => VariantF xs e -> Either (VariantF ys e) (x e)
{-# INLINE popVariantF #-}
popVariantF (VariantF v) = case popVariant v of
   Right x -> Right x
   Left xs -> Left (VariantF xs)

-- | Map the matching types of a variant
mapVariantF :: forall a b cs e ds as.
   ( MappableVariant (a e) (b e) as
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
pattern VF x = Fix (VariantF (V' x))   -- `V'` match a variant value (without
                                       -- checking the membership: we already
                                       -- do it with :<:)

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
   , Popable (f e) (ApplyAll e xs)
   , Filter (f e) (ApplyAll e xs) ~ ApplyAll e (Filter f xs)
   ) => EADT xs -> Either (VariantF (Filter f xs) (EADT xs)) (f (EADT xs))
popEADT (Fix v) = popVariantF v
