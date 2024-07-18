{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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
   , BottomUpF (FreeVarsF n) xs
   ) => t -> Set n
freeVars x = bottomUp (toBottomUp @(FreeVarsF n) freeVarsF) x
