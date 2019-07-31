module Haskus.Maths.Polygon
   ( Polygon (..)
   , polygonArea
   , polygonGravityCenterArea
   , polygonGravityCenter
   )
where

import Linear.V2

-- $setup
-- >>> import Haskus.Utils.Flow
-- >>> :set -XTypeApplications

-- | A polygon
data Polygon a = Polygon
   { polygonPoints :: [V2 a]
   }
   deriving (Show)

-- | Surface of an irregular polygon
--
-- >>> let p = Polygon [V2 0 0, V2 0 10, V2 5 0]
-- >>> polygonArea @Rational p
-- 25 % 1
polygonArea :: (Fractional a) => Polygon a -> a
polygonArea = go . polygonPoints
   where
      go []    = error "Invalid empty polygon"
      go [_]   = 0.0
      go [_,_] = 0.0
      go rs    = abs (compute 0.0 rs / 2)

      compute n (V2 x1 y1:V2 x2 y2:rs) = compute (n + (x1*y2 - x2*y1)) (V2 x2 y2:rs)
      compute n _ = n

-- | Get the coordinates of gravity center of the polygon and its area.
--
-- Return (x,y,area)
--
-- >>> let p = Polygon [V2 0 0, V2 0 10, V2 5 0]
-- >>> polygonGravityCenterArea @Rational p
-- (5 % 3,10 % 3,25 % 1)
polygonGravityCenterArea :: Fractional a => Polygon a -> (a,a,a)
polygonGravityCenterArea = go . polygonPoints
   where
      go []                  = error "Invalid empty polygon"
      go [V2 x y]            = (x,y,0)
      go [V2 x1 y1,V2 x2 y2] = (x1+(x2-x1), y1+(y2-y1),0)
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
         in (x / k, y / k, abs (ax2 / 2))

-- | Get the coordinates of the gravity center of the polygon.
--
-- If you also need the area, use `polygonGravityCenterArea`
--
-- >>> let p = Polygon [V2 0 0, V2 0 10, V2 5 0]
-- >>> polygonGravityCenter @Rational p
-- (5 % 3,10 % 3)
polygonGravityCenter :: Fractional a => Polygon a -> (a,a)
polygonGravityCenter p = case polygonGravityCenterArea p of
   (x,y,_) -> (x,y)
