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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Extensible ADT
module Haskus.Utils.EADT
   ( EADT
   , (:<:)
   , (:<<:)
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
   , module Haskus.Utils.Functor
   , module Haskus.Utils.VariantF
   )
where

import Haskus.Utils.VariantF
import Haskus.Utils.Variant
import Haskus.Utils.Functor
import Haskus.Utils.Types.List
import Haskus.Utils.Types
import Haskus.Utils.ContFlow

import GHC.Exts (Constraint)
import Control.DeepSeq

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> :set -XPatternSynonyms
-- >>> :set -XDeriveFunctor
-- >>> import Data.Functor.Classes
-- >>> data ConsF a e = ConsF a e deriving (Functor)
-- >>> data NilF    e = NilF      deriving (Functor)
-- >>> instance Eq a => Eq1 (ConsF a) where liftEq cmp (ConsF a e1) (ConsF b e2) = a == b && cmp e1 e2
-- >>> instance Eq1 NilF where liftEq _ _ _ = True
-- >>> :{
-- >>> pattern Cons :: ConsF a :<: xs => a -> EADT xs -> EADT xs
-- >>> pattern Cons a l = VF (ConsF a l)
-- >>> pattern Nil :: NilF :<: xs => EADT xs
-- >>> pattern Nil = VF NilF
-- >>> type List a = EADT '[ConsF a, NilF]
-- >>> :}


-- | An extensible ADT
--
-- >>> VF NilF == (VF NilF :: EADT '[ConsF Int, NilF])
-- True
-- >>> let a = Cons "Hello" (Cons "World" Nil) :: List String
-- >>> let b = Cons "Bonjour" (Cons "Monde" Nil) :: List String
-- >>> a == b
-- False
-- >>> a == a
-- True
type EADT xs = Fix (VariantF xs)

-- TODO: GHC 8.6
-- Replace EADT with a newtype isomorphic to Fix.
-- Use "DerivingVia" to derive instances from "Fix"
deriving newtype instance NFData (VariantF xs (EADT xs)) => NFData (EADT xs)

-- | Constructor `f` is in `xs`
type family f :<: xs where
   f :<: xs = EADTF' f (EADT xs) xs

-- | Forall `x` in `xs`, `x :<: ys`
type family (:<<:) xs ys :: Constraint where
   '[] :<<: ys       = ()
   (x ': xs) :<<: ys = (x :<: ys, xs :<<: ys)

type EADTF' f e cs =
   ( Member f cs
   , Index (IndexOf (f e) (ApplyAll e cs)) (ApplyAll e cs) ~ f e
   , PopVariant (f e) (ApplyAll e cs)
   , KnownNat (IndexOf (f e) (ApplyAll e cs))
   , Remove (f e) (ApplyAll e cs) ~ ApplyAll e (Remove f cs)
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
   , LiftVariantF as bs e
   , Functor (VariantF as)
   ) => EADT as -> EADT bs
liftEADT = cata (Fix . liftVariantF)

-- | Pop an EADT value
popEADT :: forall f xs e.
   ( f :<: xs
   , e ~ EADT xs
   , f e :< ApplyAll e xs
   ) => EADT xs -> Either (VariantF (Remove f xs) (EADT xs)) (f (EADT xs))
popEADT (Fix v) = popVariantF v

type AlterEADT c xs = AlterVariantF c (EADT xs) xs

-- | Alter an EADT value
alterEADT :: forall c xs.
   ( AlterEADT c xs
   ) => (forall f. c f => f (EADT xs) -> f (EADT xs)) -> EADT xs -> EADT xs
alterEADT f (Fix v) = Fix (alterVariantF @c @(EADT xs) f v)

type AlgEADT c r xs = AlgVariantF c (EADT xs) r xs

-- | Apply an algebra to an EADT value
algEADT :: forall c r xs.
   ( AlgEADT c r xs
   ) => (forall f. c f => f (EADT xs) -> r) -> EADT xs -> r
algEADT f (Fix v) = algVariantF @c @(EADT xs) @r f v

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

-- Orphan instance...
-- instance ContVariant (ApplyAll (EADT xs) xs) => MultiCont (EADT xs) where
--    type MultiContTypes (EADT xs) = ApplyAll (EADT xs) xs
--    toCont  = eadtToCont
--    toContM = eadtToContM

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
