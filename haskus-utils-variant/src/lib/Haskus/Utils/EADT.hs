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

import Data.Functor.Classes
import GHC.Exts (Constraint)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> :set -XPatternSynonyms
-- >>> :set -XDeriveFunctor
-- >>>
-- >>> import Data.Functor.Classes
-- >>>
-- >>> data ConsF a e = ConsF a e deriving (Functor)
-- >>> data NilF    e = NilF      deriving (Functor)
-- >>>
-- >>> instance Eq a => Eq1 (ConsF a) where liftEq cmp (ConsF a e1) (ConsF b e2) = a == b && cmp e1 e2
-- >>> instance Eq1 NilF where liftEq _ _ _ = True
-- >>>
-- >>> :{
-- >>> pattern Cons :: ConsF a :<: xs => a -> EADT xs -> EADT xs
-- >>> pattern Cons a l = VF (ConsF a l)
-- >>> pattern Nil :: NilF :<: xs => EADT xs
-- >>> pattern Nil = VF NilF
-- >>> type ListF a = VariantF '[NilF, ConsF a]
-- >>> type List a = EADT '[NilF, ConsF a]
-- >>> :}
--
-- >>>
-- >>> let a = Cons "Hello" (Cons "World" Nil) :: List String
-- >>> let b = Cons "Bonjour" (Cons "Monde" Nil) :: List String
-- >>> a == b
-- False
-- >>> a == a
-- True
--
-- >>> :{
-- >>> nilHeight :: Algebra NilF Int
-- >>> nilHeight _ = 0
-- >>> consHeight :: Algebra (ConsF a) Int
-- >>> consHeight (ConsF _ b) = 1 + b
-- >>> heightAlgebras :: Algebras (ListF a) Int
-- >>> heightAlgebras = Algebras (nilHeight,consHeight)
-- >>> heightAlgebra :: Algebra (ListF a) Int
-- >>> heightAlgebra = fromAlgebras heightAlgebras
-- >>> :}
--
-- >>> cata heightAlgebra a
-- 2
--
-- >>> :{
-- >>> class MyShow (f :: Type -> Type) where myShow :: f String -> String
-- >>> instance MyShow NilF where myShow _ = "[]"
-- >>> instance Show a => MyShow (ConsF a) where myShow (ConsF a b) = show a ++ " : " ++ b
-- >>> showAlgebra :: Show a => Algebra (ListF a) String
-- >>> showAlgebra = fromAlgebras (fromAlgebraC (AlgebraC @MyShow myShow))
-- >>> :}
--
-- >>> putStrLn (cata showAlgebra a)
-- "Hello" : "World" : []


-- | An extensible ADT
newtype EADT fs
   = EADT (VariantF fs (EADT fs))

type instance Base (EADT fs) = VariantF fs
instance Functor (VariantF fs) => Recursive (EADT fs) where
  project (EADT a) = a
instance Functor (VariantF fs) => Corecursive (EADT fs) where
  embed = EADT

instance Eq1 (VariantF fs) => Eq (EADT fs) where
  EADT a == EADT b = eq1 a b

instance Ord1 (VariantF fs) => Ord (EADT fs) where
  compare (EADT a) (EADT b) = compare1 a b

instance Show1 (VariantF fs) => Show (EADT fs) where
  showsPrec d (EADT a) =
    showParen (d >= 11)
      $ showString "EADT "
      . showsPrec1 11 a

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
pattern VF x = EADT (VariantF (VSilent x))
   -- `VSilent` matches a variant value without checking the membership: we
   -- already do it with :<:

-- | Append new "constructors" to the EADT
appendEADT :: forall ys xs zs.
   ( zs ~ Concat xs ys
   , ApplyAll (EADT zs) zs ~ Concat (ApplyAll (EADT zs) xs) (ApplyAll (EADT zs) ys)
   , Functor (VariantF xs)
   ) => EADT xs -> EADT zs
appendEADT (EADT v) = EADT (appendVariantF @ys (fmap (appendEADT @ys) v))

-- | Lift an EADT into another
liftEADT :: forall e as bs.
   ( e ~ EADT bs
   , LiftVariantF as bs e
   , Functor (VariantF as)
   ) => EADT as -> EADT bs
liftEADT = cata (EADT . liftVariantF)

-- | Pop an EADT value
popEADT :: forall f xs e.
   ( f :<: xs
   , e ~ EADT xs
   , f e :< ApplyAll e xs
   ) => EADT xs -> Either (VariantF (Remove f xs) (EADT xs)) (f (EADT xs))
popEADT (EADT v) = popVariantF v

-- | Convert an EADT into a multi-continuation
eadtToCont ::
   ( ContVariant (ApplyAll (EADT xs) xs)
   ) => EADT xs -> ContFlow (ApplyAll (EADT xs) xs) r
eadtToCont (EADT v) = variantFToCont v

-- | Convert an EADT into a multi-continuation
eadtToContM ::
   ( ContVariant (ApplyAll (EADT xs) xs)
   , Monad m
   , Functor (VariantF xs)
   ) => m (EADT xs)
     -> ContFlow (ApplyAll (EADT xs) xs) (m r)
eadtToContM f = variantFToContM (project <$> f)

-- Orphan instance...
-- instance ContVariant (ApplyAll (EADT xs) xs) => MultiCont (EADT xs) where
--    type MultiContTypes (EADT xs) = ApplyAll (EADT xs) xs
--    toCont  = eadtToCont
--    toContM = eadtToContM

-- | Convert a multi-continuation into an EADT
contToEADT ::
   ( ContVariant (ApplyAll (EADT xs) xs)
   ) => ContFlow (ApplyAll (EADT xs) xs)
                 (V (ApplyAll (EADT xs) xs))
     -> EADT xs
contToEADT c = EADT (contToVariantF c)

-- | Convert a multi-continuation into an EADT
contToEADTM ::
   ( ContVariant (ApplyAll (EADT xs) xs)
   , Monad f
   ) => ContFlow (ApplyAll (EADT xs) xs)
                 (f (V (ApplyAll (EADT xs) xs)))
     -> f (EADT xs)
contToEADTM f = EADT <$> contToVariantFM f
