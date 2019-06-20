module Haskus.UI.World
   ( World (..)
   , defaultWorld
   , Viewport (..)
   , defaultViewport
   )
where

import Haskus.UI.Maths.Linear
import Haskus.UI.Common
import Haskus.UI.Picture
import Haskus.UI.Color

-- | Virtual world
data World px obj = World
   { worldObjects         :: [obj]
   , worldBackgroundColor :: px
   }

defaultWorld :: World PixelRGB8 obj
defaultWorld = World
   { worldObjects         = []
   , worldBackgroundColor = toPixelRGB8 white
   }

-- | Viewport
data Viewport = Viewport
   { viewportCenter      :: Point3D -- ^ Center
   , viewportNormal      :: Normal  -- ^ Normal
   , viewportResolutionX :: Word    -- ^ Horizontal resolution
   , viewportResolutionY :: Word    -- ^ Vertical resolution
   , viewportPixelSize   :: Dist    -- ^ Size of a pixel (in world unit)
   }
   deriving (Show)

defaultViewport :: Viewport
defaultViewport = Viewport
   { viewportCenter      = V3 0 0 100
   , viewportNormal      = V3 0 0 (-1)
   , viewportResolutionX = 200
   , viewportResolutionY = 200
   , viewportPixelSize   = 1.0
   }

