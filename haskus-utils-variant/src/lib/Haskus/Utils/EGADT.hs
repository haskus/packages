{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Haskus.Utils.EGADT where

import Unsafe.Coerce

import Haskus.Utils.Monad
import Haskus.Utils.Variant
import Haskus.Utils.VariantF
import Haskus.Utils.Types

-- $setup
-- >>> :seti -XDataKinds
-- >>> :seti -XTypeApplications
-- >>> :seti -XTypeOperators
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeFamilies
-- >>> :seti -XPatternSynonyms
-- >>> :seti -XDeriveFunctor
-- >>> :seti -XGADTs
-- >>> :seti -XPolyKinds
-- >>> :seti -XPartialTypeSignatures
-- >>>
-- >>> :{
-- >>> data LamF (ast :: Type -> Type) t where
-- >>>   LamF :: ( ast a -> ast b ) -> LamF ast ( a -> b )
-- >>> 
-- >>> data AppF ast t where
-- >>>   AppF :: ast ( a -> b ) -> ast a -> AppF ast b
-- >>>
-- >>> data VarF ast t where
-- >>>   VarF :: String -> VarF ast Int
-- >>>
-- >>> type AST a = EGADT '[LamF,AppF,VarF] a
-- >>>
-- >>> :}
--
-- >>> let y = VF @(AST Int) (VarF "a")
-- >>> :t y
-- y :: EGADT [LamF, AppF, VarF] Int
--
-- >>> :{
-- >>> case y of
-- >>>   VF (VarF x) -> print x
-- >>>   _           -> putStrLn "Not a VarF"
-- >>> :}
-- "a"
--
-- >>> :{
-- >>> f :: AST Int -> AST Int
-- >>> f (VF (VarF x)) = VF (VarF "zz")
-- >>> f _             = error "Unhandled case"
-- >>> :}
--
-- >>> let z = VF (AppF (VF (LamF f)) (VF (VarF "a")))
-- >>> :t z
-- z :: EGADT [LamF, AppF, VarF] Int
--


-- | An EADT with an additional type parameter
newtype EGADT fs t = EGADT (HVariantF fs (EGADT fs) t)

newtype HVariantF (fs :: [ (k -> Type) -> ( k -> Type) ]) (ast :: k -> Type) (t :: k)
  = HVariantF (VariantF (ApplyAll ast fs) t)

toHVariantAt
  :: forall i fs ast a
  .  KnownNat i
  => (Index i fs) ast a -> VariantF (ApplyAll ast fs) a
{-# INLINABLE toHVariantAt #-}
toHVariantAt a = VariantF (Variant (natValue' @i) (unsafeCoerce a))

fromHVariantAt
  :: forall i fs ast a
  .  KnownNat i
  => VariantF (ApplyAll ast fs) a -> Maybe ((Index i fs) ast a)
{-# INLINABLE fromHVariantAt #-}
fromHVariantAt (VariantF (Variant t a)) = do
  guard (t == natValue' @i)
  return (unsafeCoerce a)

type instance HBase (EGADT xs) = HVariantF xs

instance HFunctor (HVariantF xs) => HRecursive (EGADT xs) where
  hproject (EGADT a) = a

instance HFunctor (HVariantF xs) => HCorecursive (EGADT xs) where
  hembed = EGADT

type family f :<! fs :: Constraint where
  f :<! fs = ( MemberAtIndex (IndexOf f fs) f fs )

type family MemberAtIndex (i :: Nat) f fs :: Constraint where
  MemberAtIndex i f fs = ( KnownNat i, Index i fs ~ f )

type family (:<<!) xs ys :: Constraint where
  '[]       :<<! ys = ()
  (x ': xs) :<<! ys = (x :<! ys, xs :<<! ys)

-- | Pattern-match in an extensible GADT
pattern VF :: forall e a f fs.
  ( e ~ EGADT fs a  -- allow easy use of TypeApplication to set the EGADT type
  , f :<! fs
  ) => f (EGADT fs) a -> EGADT fs a
pattern VF x <- ( ( \ ( EGADT (HVariantF v) ) -> fromHVariantAt @(IndexOf f fs) @fs v ) -> Just x )
  where
    VF x = EGADT (HVariantF (toHVariantAt @(IndexOf f fs) @fs x))
