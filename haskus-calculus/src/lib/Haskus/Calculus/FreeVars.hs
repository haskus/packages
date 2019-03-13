{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Haskus.Calculus.FreeVars
   ( FreeVarsF (..)
   , freeVars
   )
where

import Haskus.Utils.Types
import Haskus.Utils.EADT
import Data.Set

class FreeVarsF n (f :: Type -> Type) where
   freeVarsF :: f (Set n) -> Set n

-- | Get the free variables of an expression
freeVars :: forall n t xs.
   ( Base t ~ VariantF xs
   , Recursive t
   , FromAlgebras xs t
   , FromAlgebraC xs (FreeVarsF n) (Set n)
   ) => t -> Set n
freeVars x = cata alg x
   where
      alg :: Algebra (Base t) (Set n)
      alg = fromAlgebras (fromAlgebraC (AlgebraC @(FreeVarsF n) freeVarsF))

