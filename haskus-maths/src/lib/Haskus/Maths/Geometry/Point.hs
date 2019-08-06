{-# LANGUAGE PatternSynonyms #-}

module Haskus.Maths.Geometry.Point
   ( pattern P2
   )
where

import Linear.V2
import Linear.Affine

-- | Match a 2D point
pattern P2 :: a -> a -> Point V2 a
{-# COMPLETE P2 #-}
pattern P2 x y = P (V2 x y)
