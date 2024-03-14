module Haskus.Maths.Geometry.Line
   ( Line (..)
   , linePointNearest
   , linePointOn
   )
where

import Linear.V2
import Linear.Affine
import Linear.Metric

-- $setup
-- >>> import Linear.V2
-- >>> import Linear.Affine

-- | A line
data Line a
   = Line !a !a -- y = ax+b
   | VLine !a   -- { (a,y) }
   deriving (Show,Eq,Ord)

-- | Get the point of the line nearest to the given point
--
-- >>> linePointNearest (VLine (2 :: Double)) (P (V2 5 5))
-- P (V2 2.0 5.0)
--
-- >>> linePointNearest (Line (2 :: Double) 2) (P (V2 5 5))
-- P (V2 2.2 6.4)
--
-- >>> let l = Line (2 :: Double) 2
-- >>> linePointOn l (linePointNearest l (P (V2 5 5)))
-- True
linePointNearest :: (Fractional a, Num a) => Line a -> Point V2 a -> Point V2 a
linePointNearest (VLine xo) (P (V2 _x y)) = P (V2 xo y)
linePointNearest (Line a b) (P p) = P (o + (project k (p - o)))
   where
      o = V2 0 b
      k = V2 1 a

-- | Test if a point is one the line
--
-- >>> linePointOn (Line 2.0 3.0) (P (V2 5 5))
-- False
--
-- >>> linePointOn (Line 2.0 3.0) (P (V2 2 7))
-- True
linePointOn :: (Num a, Eq a) => Line a -> Point V2 a -> Bool
linePointOn (VLine xo) (P (V2 x _y)) = xo == x
linePointOn (Line a b) (P (V2 x y)) = y == a*x+b
