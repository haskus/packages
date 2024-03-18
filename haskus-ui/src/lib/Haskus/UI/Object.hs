{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Haskus.UI.Object
   ( Object (..)
   , module Haskus.UI.Ray
   )
where

import Haskus.Utils.VariantF
import Haskus.Utils.EADT

import Haskus.UI.Ray

-- | Object of the world
class Object a where
   -- | Test if an object is hit by a ray
   hit :: Ray -> a -> Maybe Hit

instance Object (VariantF f (EADT f)) => Object (EADT f) where
   {-# INLINE hit #-}
   hit r (EADT vf) = hit r vf


instance Object (VariantF '[] e) where
   {-# INLINE hit #-}
   hit = undefined

instance
   ( Object (x e)
   , Object (VariantF xs e)
   ) => Object (VariantF (x ': xs) e) where
   {-# INLINE hit #-}
   hit r v = case popVariantFHead v of
      Right x -> hit r x
      Left xs -> hit r xs
