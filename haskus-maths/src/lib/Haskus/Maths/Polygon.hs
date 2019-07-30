module Haskus.Maths.Polygon
   ( Polygon (..)
   , polygonSurface
   , polygonGravityCenter
   )
where

import Linear.V2

-- $setup
-- >>> import Haskus.Utils.Flow

-- | A polygon
data Polygon a = Polygon
   { polygonPoints :: [V2 a]
   }
   deriving (Show)

-- | Surface of an irregular polygon
polygonSurface :: (Fractional a) => Polygon a -> a
polygonSurface = go . polygonPoints
   where
      go []    = error "Invalid empty polygon"
      go [_]   = 0.0
      go [_,_] = 0.0
      go rs    = abs (compute 0.0 rs / 2)

      compute n (V2 x1 y1:V2 x2 y2:rs) = compute (n + (x1*y2 - x2*y1)) (V2 x2 y2:rs)
      compute n _ = n

-- | Get the gravity center of the polygon
polygonGravityCenter :: Fractional a => Polygon a -> (a,a)
polygonGravityCenter = go . polygonPoints
   where
      go []                  = error "Invalid empty polygon"
      go [V2 x y]            = (x,y)
      go [V2 x1 y1,V2 x2 y2] = (x1+(x2-x1), y1+(y2-y1))
      go rs                  = compute 0.0 0.0 0.0 (rs ++ [head rs])

      -- ax2 is the surface times 2
      -- we compute it at the same time because it follows the same traversal of
      -- the list of points than computing the gravity center (i.e. manual
      -- stream fusion)
      compute x y ax2 (V2 x1 y1:V2 x2 y2:rs) =
         let k = x1*y2 - x2*y1
         in compute (x + (x1+x2)*k) (y + (y1+y2)*k) (ax2+k) (V2 x2 y2:rs)
      compute x y ax2 _ =
         let k = 3 * ax2
         in (x / k, y / k)
