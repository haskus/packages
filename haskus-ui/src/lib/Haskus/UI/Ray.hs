module Haskus.UI.Ray
   ( Ray (..)
   , Hit (..)
   )
where

import Haskus.UI.Common

-- | Ray
data Ray = Ray
   { rayOrigin    :: Point3D  -- ^ Ray origin
   , rayDirection :: Vector3D -- ^ Ray direction
   }
   deriving (Show)

-- | Hit information
data Hit = Hit
   { hitPoint    :: Point3D     -- ^ World coordinates of hit point
   , hitNormal   :: Normal      -- ^ Normal at hit point
   , hitDistance :: Dist        -- ^ Distance from ray origin
   , hitColor    :: Maybe Color -- ^ Color at hit point
   }
   deriving (Show)

