{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Haskus.UI.Canvas
   ( CGraph(..)
   , colorsAt
   , Color (..)
   , Name (..)
   , Canvas (..)
   )
where

import Haskus.Utils.Monad
import Haskus.Utils.Maybe

data CGraph d c n
   = Rectangle d d                          -- ^ Rectangle (current X,current Y, width, height)
   | Group [CGraph d c n]                   -- ^ Group of child objects
   | Move d d (CGraph d c n)                -- ^ Move the sub-graph
   | ZOrder d (CGraph d c n)                -- ^ Move Z-order (relative value, not absolute)
   | Colorize c (CGraph d c n)              -- ^ Set the default color for the sub-graph
   | Named n (CGraph d c n)                 -- ^ Named sub-graph
   | ClipWith (CGraph d c n) (CGraph d c n) -- ^ Object clipping

-- | Return a color and a Z-order
colorsAt :: (Num d, Ord d) => d -> d -> CGraph d c n -> [(d,c)]
colorsAt atx aty g = go Nothing 0 atx aty g
   where
      go currentColor z x y = \case
         Rectangle w h
            | x < 0 || y < 0 -> []
            | x < w && y < h -> maybeToList (fmap (z,) currentColor)
            | otherwise      -> []
         Group xs       -> join (fmap (go currentColor z x y) xs)
         Move mx my g'  -> go currentColor z (x-mx) (y-my) g'
         ZOrder mz  g'  -> go currentColor (z+mz) x y g'
         Colorize c g'  -> go (Just c) z x y g'
         Named _ g'     -> go currentColor z x y g'
         ClipWith g' cg
            -- we check that the clipper graph returns a color
            | null (go currentColor z x y cg) -> []
            | otherwise                       -> go currentColor z x y g'

data Color = Color Float Float Float

data Name
   = Root
   | CustomName String

data Canvas = Canvas
   { canvasGraph :: CGraph Float Color Name
   }
