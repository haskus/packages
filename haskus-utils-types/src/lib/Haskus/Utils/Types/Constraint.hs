{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type-level Constraint
--
-- Use it with `ConstraintKinds` LANGUAGE pragma
module Haskus.Utils.Types.Constraint
   ( Constraint
   , ConstraintAll1
   )
where

import GHC.Exts (Constraint)

-- | Build a list of constraints
-- e.g., ConstraintAll1 Eq '[A,B,C] ==> (Eq A, Eq B, Eq C)
type family ConstraintAll1 (f :: k -> Constraint) (xs :: [k]) :: Constraint where
   ConstraintAll1 f '[]       = ()
   ConstraintAll1 f (x ': xs) = (f x, ConstraintAll1 f xs)
