{-# LANGUAGE RoleAnnotations #-}

module Haskus.Graphics.Picture
   ( Picture (..)
   )
where

import Data.Ratio
import Haskus.Memory.Buffer

-- | A picture (a 2D array of pixels)
data Picture p = Picture
   { pictureWidth      :: !Word      -- ^ Width in pixels
   , pictureheight     :: !Word      -- ^ Height in pixels
   , pictureData       :: Buffer     -- ^ Pixel data
   , picturePixelRatio :: Ratio Word -- ^ Pixel width/height ratio (= 1 if square pixels)
   }

type role Picture representational
