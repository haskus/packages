{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}

module Haskus.UI.Object.Sphere
   ( SphereF (..)
   , pattern Sphere
   )
where

import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH

import Haskus.UI.Maths.Linear
import Haskus.UI.Object
import Haskus.UI.Common

-- | Sphere
data SphereF e = SphereF
   { sphereCenter :: Point3D  -- ^ Sphere center
   , sphereRadius :: Dist     -- ^ Sphere radius
   }
   deriving (Show)

eadtPattern 'SphereF "Sphere"

instance Object (SphereF e) where
   hit Ray{..} SphereF{..} =
      let
         temp = rayOrigin ^-^ sphereCenter
         a    = rayDirection `dot` rayDirection
         b    = 2.0 * (temp `dot` rayDirection)
         c    = temp `dot` temp - sphereRadius * sphereRadius
         disc = b*b - 4.0*a*c
         e     = sqrt disc
         denom = 2.0 * a
         dist1 = (-1 * b - e) / denom -- smaller root
         dist2 = (-1 * b + e) / denom -- larger root
      in if | disc < 0.0           -> Nothing
            | not (nearZero dist1) -> Just (Hit --smaller root first
                                       { hitPoint    = rayOrigin + dist1 *^ rayDirection
                                       , hitNormal   = (temp ^+^ dist1 *^ rayDirection) ^/ sphereRadius
                                       , hitDistance = dist1
                                       , hitColor    = Nothing
                                       })
            | not (nearZero dist2) -> Just (Hit
                                       { hitPoint    = rayOrigin + dist2 *^ rayDirection
                                       , hitNormal   = (temp ^+^ dist2 *^ rayDirection) ^/ sphereRadius
                                       , hitDistance = dist2
                                       , hitColor    = Nothing
                                       })
            | otherwise            -> Nothing
         

