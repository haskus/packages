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

-- | Functor Variant
module Haskus.Utils.VariantF
   ( VariantF (..)
   , FixV
   , appendVariantF
   , toVariantFHead
   , toVariantFTail
   , popVariantFHead
   , mapVariantF
   -- * Extensible recursive ADT
   , EADT
   , pattern VF
   , appendEADT
   )
where

import Haskus.Utils.Variant
import Haskus.Utils.Fix
import Haskus.Utils.Types.List

-- | Recursive Functor-like Variant
newtype VariantF (xs :: [* -> *]) e = VariantF (V (FixV e xs))

type family FixV e (xs :: [* -> *]) :: [*] where
   FixV e '[]       = '[]
   FixV e (f ': fs) = f e ': FixV e fs

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

instance Functor (VariantF '[]) where
   fmap _ = undefined

instance (Functor (VariantF fs), Functor f) => Functor (VariantF (f ': fs)) where
   fmap f (VariantF v) = case popVariantHead v of
      Right x -> toVariantFHead (fmap f x)
      Left xs -> toVariantFTail (fmap f (VariantF xs))


--------------------------------------------
-- Extensible ADT
--------------------------------------------

-- | An extensible ADT
type EADT xs = Fix (VariantF xs)

-- | Pattern-match in an extensible ADT
pattern VF :: forall c cs. Popable c (FixV (EADT cs) cs) => c -> EADT cs
pattern VF x = Fix (VariantF (V x))

-- | Append new "constructors" to the EADT
appendEADT :: forall ys xs zs.
   ( zs ~ Concat xs ys
   , FixV (EADT zs) zs ~ Concat (FixV (EADT zs) xs) (FixV (EADT zs) ys)
   , Functor (VariantF xs)
   ) => EADT xs -> EADT zs
appendEADT (Fix v) = Fix (appendVariantF @ys (fmap (appendEADT @ys) v))
