{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Haskus.UI.Object.Plane
   ( PlaneF (..)
   , pattern Plane
   )
where

import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH

import Haskus.UI.Maths.Linear
import Haskus.UI.Object
import Haskus.UI.Common

-- | Plane
data PlaneF e = PlaneF
   { planePoint  :: Point3D -- ^ A point on the plane
   , planeNormal :: Normal  -- ^ Normal of the plane
   }
   deriving (Show,Functor)

eadtPattern 'PlaneF "Plane"

instance Object (PlaneF e) where
   hit Ray{..} PlaneF{..} =
      let
         dist = ((planePoint ^-^ rayOrigin) `dot` planeNormal) / (rayDirection `dot` planeNormal)
                -- we don't check that we divide by zero: if we do, we expect +INF
         h    = Hit
                  { hitPoint    = rayOrigin ^+^ (dist *^ rayDirection)
                  , hitNormal   = planeNormal
                  , hitDistance = dist
                  , hitColor    = Nothing
                  }
      in if nearZero dist || dist < 0  -- we use `nearZero` instead of "== 0" which isn't right for floats
            then Nothing
            else Just h
