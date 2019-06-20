{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Haskus.UI.Canvas
   ( CGraph(..)
   , colorsAt
   , Color (..)
   , Name (..)
   , Canvas (..)
   , modifyByName
   , findByName
   )
where

import Haskus.Utils.Monad
import Haskus.Utils.Maybe
import Control.Applicative

data CGraph d c n
   = Rectangle d d                          -- ^ Rectangle (width,height) (top = current Y, left = current X)
   | Group [CGraph d c n]                   -- ^ Group of child objects
   | Move d d (CGraph d c n)                -- ^ Move the sub-graph
   | ZOrder d (CGraph d c n)                -- ^ Move Z-order (relative value, not absolute)
   | Colorize c (CGraph d c n)              -- ^ Set the default color for the sub-graph
   | Named n (CGraph d c n)                 -- ^ Named sub-graph
   | ClipWith (CGraph d c n) (CGraph d c n) -- ^ Object clipping
   -- | RefNamed n                             -- ^ Reference a sub-graph by its name


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

-- | Modify a node by name
modifyByName :: Eq n => (CGraph d c n -> CGraph d c n) -> n -> CGraph d c n -> CGraph d c n
modifyByName f n g = case g of
   Rectangle _ _  -> g
   Group xs       -> Group (fmap (modifyByName f n) xs)
   Move mx my g'  -> Move mx my (modifyByName f n g')
   ZOrder mz  g'  -> ZOrder mz (modifyByName f n g')
   Colorize c g'  -> Colorize c (modifyByName f n g')
   Named n' g'
      | n == n'   -> Named n' (f g')
      | otherwise -> Named n' (modifyByName f n g')
   ClipWith g' cg -> ClipWith (modifyByName f n g') (modifyByName f n cg)

-- | Find a node by name
findByName :: Eq n => n -> CGraph d c n -> Maybe (CGraph d c n)
findByName n g = case g of
   Rectangle _ _  -> Nothing
   Group xs       -> headMaybe (catMaybes (fmap (findByName n) xs))
   Move _ _ g'    -> findByName n g'
   ZOrder _   g'  -> findByName n g'
   Colorize _ g'  -> findByName n g'
   Named n' g'
      | n == n'   -> Just g'
      | otherwise -> findByName n g'
   ClipWith g' cg -> findByName n g' <|> findByName n cg

data Color = Color Float Float Float

data Name
   = Root
   | CustomName String

data Canvas = Canvas
   { canvasGraph :: CGraph Float Color Name
   }
