{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}

module Haskus.Utils.EGADT where

import Haskus.Utils.Variant
import Haskus.Utils.Types

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> :set -XPatternSynonyms
-- >>> :set -XDeriveFunctor
-- >>> :set -XGADTs
-- >>> :set -XPolyKinds
-- >>> :set -XPartialTypeSignatures
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
-- >>> let x = (EGADT (HVariantF (V (VarF @(EGADT '[LamF,AppF,VarF]) "a"))) :: AST Int)
-- >>> :t x
-- x :: AST Int
--
-- >>> let y = VF @(AST Int) (VarF "a")
-- >>> :t y
-- y :: EGADT '[LamF, AppF, VarF] Int
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
-- z :: EGADT '[LamF, AppF, VarF] Int
--


-- | An EADT with an additional type parameter
newtype EGADT fs (t :: k) = EGADT (HVariantF fs (EGADT fs) t)

newtype HVariantF xs ast t = HVariantF (V (HApplyAll xs ast t))

type family HApplyAll (xs :: [(k -> Type) -> (k -> Type)]) (ast :: k -> Type) (t :: k) :: [Type] where
  HApplyAll '[]       _   _ = '[]
  HApplyAll (x ': xs) ast t = (x ast t ': HApplyAll xs ast t)


-- Recursion schemes for higher-order functors

type (~>) (f :: k -> Type) (g :: k -> Type) = forall (x :: k). f x -> g x
infixr 0 ~>

type family HBase (h :: k -> Type) :: (k -> Type) -> (k -> Type)

class HFunctor (h :: (k -> Type) -> (k -> Type)) where
  hmap :: (f ~> g) -> h f ~> h g

class HFunctor (HBase h) => HRecursive (h :: k -> Type) where
  hproject :: h ~> (HBase h) h

class HFunctor (HBase h) => HCorecursive (h :: k -> Type) where
  hembed :: (HBase h) h ~> h


type instance HBase (EGADT xs) = HVariantF xs

instance HFunctor (HVariantF xs) => HRecursive (EGADT xs) where
  hproject (EGADT a) = a

instance HFunctor (HVariantF xs) => HCorecursive (EGADT xs) where
  hembed = EGADT

-- | Pattern-match in an extensible GADT
pattern VF :: forall e a f cs i fsa.
   ( e ~ EGADT cs a  -- allow easy use of TypeApplication to set the EGADT type
   , fsa ~ (HApplyAll cs (EGADT cs) a)
   , i ~ IndexOf (f (EGADT cs) a) fsa
   , Index i fsa ~ f (EGADT cs) a
   , KnownNat i
   , PopVariant (f (EGADT cs) a) fsa
   ) => f (EGADT cs) a -> EGADT cs a
pattern VF x = EGADT (HVariantF (VSilent x))
