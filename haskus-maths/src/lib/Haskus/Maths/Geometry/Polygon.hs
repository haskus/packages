module Haskus.Maths.Geometry.Polygon
   ( Polygon (..)
   , polygonEdges
   , polygonMapPoints
   , polygonArea
   , polygonCentroidArea
   , polygonCentroid
   , polygonWindingNumber
   , polygonPointInside
   , polygonPointNearest
   , polygonPointNearestBorder
   )
where

import Linear.V2
import Linear.Affine

import Haskus.Maths.Geometry.Segment
import Haskus.Maths.Geometry.Point
import Haskus.Utils.List
import Haskus.Utils.Flow

-- $setup
-- >>> import Haskus.Utils.Flow
-- >>> :set -XTypeApplications

-- | A polygon
newtype Polygon a = Polygon
   { polygonPoints :: [Point V2 a]
   }
   deriving (Show)

-- | Map polygon points
--
-- >>> let p = Polygon [P2 0 0, P2 0 10, P2 5 0]
-- >>> polygonMapPoints (*5) p
-- Polygon {polygonPoints = [P (V2 0 0),P (V2 0 50),P (V2 25 0)]}
polygonMapPoints :: (Point V2 a -> Point V2 a) -> Polygon a -> Polygon a
polygonMapPoints f (Polygon ps) = Polygon (fmap f ps)

-- | Edges of the polygon
--
-- >>> let p = Polygon [P2 0 0, P2 0 10, P2 5 0]
-- >>> polygonEdges @Word p
-- [(P (V2 0 0),P (V2 0 10)),(P (V2 0 10),P (V2 5 0)),(P (V2 5 0),P (V2 0 0))]
polygonEdges :: Polygon a -> [(Point V2 a,Point V2 a)]
polygonEdges (Polygon vs) = zip vs (tail vs ++ [head vs])

-- | Surface of an irregular polygon
--
-- >>> let p = Polygon [P2 0 0, P2 0 10, P2 5 0]
-- >>> polygonArea @Rational p
-- 25 % 1
--
-- >>> let p = Polygon [P2 (-5) (-5), P2 5 (-5), P2 5 5, P2 (-5) 5]
-- >>> polygonArea @Rational p
-- 100 % 1
polygonArea :: (Fractional a) => Polygon a -> a
polygonArea = abs . (/2) . sum . fmap compute . polygonEdges
   where
      compute (P2 x1 y1, P2 x2 y2) = (x1*y2 - x2*y1)

-- | Get the coordinates of centroid (gravity center) of the polygon and the
-- area of the polygon (needed to compute the centroid)
--
-- Return (x,y,area)
--
-- >>> let p = Polygon [P2 0 0, P2 0 10, P2 5 0]
-- >>> polygonCentroidArea @Rational p
-- (5 % 3,10 % 3,25 % 1)
polygonCentroidArea :: Fractional a => Polygon a -> (a,a,a)
polygonCentroidArea = go . polygonPoints
   where
      go []                  = error "Invalid empty polygon"
      go [P2 x y]            = (x,y,0)
      go [P2 x1 y1,P2 x2 y2] = (x1+(x2-x1), y1+(y2-y1),0)
      go rs                  = compute 0.0 0.0 0.0 (rs ++ [head rs])

      -- ax2 is the surface times 2
      -- we compute it at the same time because it follows the same traversal of
      -- the list of points than computing the gravity center (i.e. manual
      -- stream fusion)
      compute x y ax2 (P2 x1 y1:P2 x2 y2:rs) =
         let k = x1*y2 - x2*y1
         in compute (x + (x1+x2)*k) (y + (y1+y2)*k) (ax2+k) (P2 x2 y2:rs)
      compute x y ax2 _ =
         let k = 3 * ax2
         in (x / k, y / k, abs (ax2 / 2))

-- | Get the coordinates of the centroid (gravity center) of the polygon.
--
-- If you also need the area, use `polygonCentroidArea`
--
-- >>> let p = Polygon [P2 0 0, P2 0 10, P2 5 0]
-- >>> polygonCentroid @Rational p
-- (5 % 3,10 % 3)
polygonCentroid :: Fractional a => Polygon a -> (a,a)
polygonCentroid p = case polygonCentroidArea p of
   (x,y,_) -> (x,y)


-- | Count the number of times a polygon encloses a given point.
--
-- This can be used to know if a point is in a polygon:
--    wn = 0   => the point isn't inside th polygon
--    wn > 0   => the polygon winds wn times around the point, counterclockwise
--    wn < 0   => ditto, clockwise
--
-- Method:
--    - we translate the origin in P (the given point)
--    - for each polygon edge, we check if and how it crosses the *positive* x-axis:
--       - both vertices on x-axis: wn unchanged
--       - bottom-up start/end vertice on x-axis: wn+=1/2
--       - top-down start/end vertice on x-axis: wn-=1/2
--       - bottom-up: wn+=1
--       - top-down: wn-=1
--
-- >>> let p = Polygon [P2 (-5) (-5), P2 5 (-5), P2 5 5, P2 (-5) 5]
-- >>> polygonWindingNumber (P2 0 0) p
-- -1
--
-- >>> let p = Polygon [P2 0 0, P2 0 10, P2 5 0]
-- >>> polygonWindingNumber (P2 1 1) p
-- 1
polygonWindingNumber :: (Fractional a, Num a, Ord a) => Point V2 a -> Polygon a -> Int
polygonWindingNumber (P p) = (`div` 2) . sum . fmap crossX . polygonEdges . polygonMapPoints (.-^ p)
   where
      crossX cs@(P2 x1 y1, P2 x2 y2)
         | signum y1 == signum y2   = 0 -- don't cross x-axis
         | x1 >= 0 && x2 >= 0       = crossPosX cs
         | x1 < 0 && x2 < 0         = 0 -- cross negative x-axis
         | let k  = (y2-y1)/(x2-x1)
         , let x0 = (x1*k  - y1)/k
         , x0 < 0                   = 0 -- cross negative x-axis
         | otherwise                = crossPosX cs


      -- we compute 2*wn to avoid dealing with 1/2
      crossPosX (P2 _x1 y1, P2 _x2 y2)
         | y1 == 0 && y2 == 0 = 0
         | y1 == 0 && y2 > 0  = -1
         | y1 < 0  && y2 == 0 = -1
         | y1 == 0 && y2 < 0  = 1
         | y1 > 0  && y2 == 0 = 1
         | y1 > 0             = 2
         | otherwise          = -2

-- | Indicate if the given point is inside the (right-hand inside) polygon
--
-- >>> let p = Polygon [P2 (-5) (-5), P2 5 (-5), P2 5 5, P2 (-5) 5]
-- >>> polygonPointInside (P2 0 0) p
-- False
--
-- >>> let p = Polygon [P2 0 0, P2 0 10, P2 5 0]
-- >>> polygonPointInside (P2 1 1) p
-- True
--
-- >>> let p = Polygon [P2 0 0, P2 0 10, P2 5 0]
-- >>> polygonPointInside (P2 20 20) p
-- False
polygonPointInside :: (Fractional a, Num a, Ord a) => Point V2 a -> Polygon a -> Bool
polygonPointInside xy p = polygonWindingNumber xy p > 0


-- | Get the point inside the polygon nearest to the given point
--
-- >>> let p = Polygon [P2 (-5) (-5), P2 (-5) 5, P2 5 5, P2 5 (-5)]
-- >>> polygonPointNearest p (P2 0 0)
-- P (V2 0.0 0.0)
--
-- >>> polygonPointNearest p (P2 10 10)
-- P (V2 5.0 5.0)
--
-- >>> polygonPointNearest p (P2 3 6)
-- P (V2 3.0 5.0)
polygonPointNearest :: (Fractional a, Num a, Ord a, Floating a) => Polygon a -> Point V2 a -> Point V2 a
polygonPointNearest poly p
   | polygonPointInside p poly = p
   | otherwise = polygonPointNearestBorder poly p

-- | Get the point on the border of the polygon nearest to the given point
polygonPointNearestBorder :: (Ord a,Floating a) => Polygon a -> Point V2 a -> Point V2 a
polygonPointNearestBorder poly p = head (sortOn (distanceA p) cs)
   where
      cs = polygonEdges poly
            ||> uncurry Segment
            ||> flip segmentPointNearest p
