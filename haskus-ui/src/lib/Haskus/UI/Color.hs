module Haskus.UI.Color
   ( toPixelRGB8
   , module Data.Colour.Names
   , module Data.Colour
   )
where

import Haskus.UI.Common
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Codec.Picture.Types

-- | Convert a color into a PixelRGB8
toPixelRGB8 :: Color -> PixelRGB8
toPixelRGB8 x = let rgb = toSRGB24 x
                in PixelRGB8 (channelRed rgb)
                             (channelGreen rgb)
                             (channelBlue rgb)
