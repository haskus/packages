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
import Haskus.Utils.EADT

class ReplaceVarF n fs (f :: Type -> Type) where
   replaceVarF :: n -> EADT fs -> TopDownStop (EADT fs) f

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
   , FromTopDownStops fs t
   , FromTopDownStopC fs (ReplaceVarF n fs) t
   ) => n -> t -> t -> t
replaceVar n e x = topDownStop alg x
   where
      alg :: TopDownStop t (Base t)
      alg = fromTopDownStops (fromTopDownStopC (TopDownStopC @(ReplaceVarF n fs) (replaceVarF n e)))

