{-# LANGUAGE MultiWayIf #-}

module Haskus.Maths.Geometry.Segment
   ( Segment (..)
   , makeSegment
   , segmentLine
   , segmentPointNearest
   )
where

import Linear.V2
import Linear.Affine
import Linear.Metric
import Linear.Vector

import Haskus.Maths.Geometry.Line

-- $setup
-- >>> import Haskus.Maths.Geometry.Point
-- >>> import Linear.V2
-- >>> import Linear.Affine

-- | A segment between two points
data Segment a
   = Segment !(Point V2 a) !(Point V2 a)
   deriving (Eq,Ord,Show)

-- | Build a segment
makeSegment :: Eq a => Point V2 a -> Point V2 a -> Segment a
makeSegment p1 p2
   | p1 == p2  = error "Can't build a segment with two equal points"
   | otherwise = Segment p1 p2

-- | Get the line defined by the segment
--
-- >>> segmentLine (Segment (P (V2 0 2)) (P (V2 1 4)))
-- Line 2.0 2.0
--
-- >>> segmentLine (Segment (P (V2 0 2)) (P (V2 0 4)))
-- VLine 0.0
segmentLine :: (Eq a, Fractional a) => Segment a -> Line a
segmentLine (Segment (P (V2 x1 y1)) (P (V2 x2 y2)))
   | x1 == x2  = VLine x1
   | otherwise = Line a b
      where
         a = (y2-y1)/(x2-x1)
         b = y1 - x1*a


-- | Get the point of the segment nearest to the given point
--
-- >>> let s = Segment (P2 0 2) (P2 1 4)
-- >>> segmentPointNearest s (P2 3 3)
-- P (V2 1.0 4.0)
--
-- >>> segmentPointNearest s (P2 (-1) (-3))
-- P (V2 0.0 2.0)
--
-- >>> segmentPointNearest (Segment (P2 (-5) 5) (P2 5 5)) (P2 0 6)
-- P (V2 0.0 5.0)
--
-- >>> segmentPointNearest (Segment (P2 (-5) 5) (P2 5 5)) (P2 3 6)
-- P (V2 3.0 5.0)
segmentPointNearest :: (Fractional a, Num a, Ord a, Floating a) => Segment a -> Point V2 a -> Point V2 a
segmentPointNearest (Segment (P p1) (P p2)) (P p) = P p'
   where
      v12  = p2 ^-^ p1
      v1p  = p  ^-^ p1
      d12 = norm v12
      d1p' = (v12 `dot` v1p) / d12
      p' = if | d1p' <= 0   -> p1
              | d1p' >= d12 -> p2
              | otherwise   -> p1 + d1p' *^ signorm v12
