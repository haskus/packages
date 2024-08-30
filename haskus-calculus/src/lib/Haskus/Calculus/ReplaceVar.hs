{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskus.Calculus.ReplaceVar
   ( ReplaceVarF (..)
   , replaceVar
   )
where

import Haskus.Utils.Types
import Haskus.Data.Variant.EADT

class ReplaceVarF n fs (f :: Type -> Type) where
   replaceVarF :: n -> EADT fs -> TopDownStopT (EADT fs) f

-- | Default instance just traverse
instance
   {-# OVERLAPPABLE #-}
   f :<: fs => ReplaceVarF n fs f
   where
   replaceVarF _n _e v = Left v

-- | Replace a variable
replaceVar :: forall n t fs.
   ( Base t ~ VariantF fs
   , t ~ EADT fs
   , Recursive t
   , Corecursive t
   , TopDownStopF (ReplaceVarF n fs) fs
   ) => n -> t -> t -> t
replaceVar n e x = topDownStop (toTopDownStop @(ReplaceVarF n fs) (replaceVarF n e)) x
