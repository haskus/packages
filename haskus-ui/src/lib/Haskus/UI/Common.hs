module Haskus.UI.Common
   ( Dist
   , Point3D
   , Vector3D
   , Normal
   , Color
   , ColorUnit
   )
where

import Data.Colour
import Haskus.UI.Maths.Linear

type Dist = Double    -- expect support for +INF when divided by zero

type Point3D   = V3 Dist
type Vector3D  = V3 Dist
type Normal    = Vector3D
type ColorUnit = Float
type Color     = Colour ColorUnit

