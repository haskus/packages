{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Haskus.UI.Canvas
   ( SceneGraph(..)
   , RenderObject (..)
   , Transform (..)
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
import Haskus.Utils.Flow
import Control.Applicative

data RenderObject d c
   = Rectangle d d -- ^ Rectangle (width,height,1) with top-left at current (x,y,z)

data Transform d g
   = Translate d d d g -- ^ Translate (x,y,z) the sub-graph in the space

data SceneGraph d c n
   = NodeObject (RenderObject d c)              -- ^ Renderable objects
   | NodeGroup [SceneGraph d c n]               -- ^ NodeGroup of nodes
   | NodeTransform (Transform d (SceneGraph d c n)) -- ^ Transformations
   | Colorize c (SceneGraph d c n)              -- ^ Set the default color for the sub-graph
   | Named n (SceneGraph d c n)                 -- ^ Named sub-graph
   | NodeClip (SceneGraph d c n) (SceneGraph d c n) -- ^ Object clipping
   -- | RefNamed n                             -- ^ Reference a sub-graph by its name


-- | Return a color and a Z-order
colorsAt :: (Num d, Ord d) => d -> d -> SceneGraph d c n -> [(d,c)]
colorsAt atx aty g = go Nothing 0 atx aty g
   where
      go currentColor z x y = \case
         NodeObject o -> case o of
            Rectangle w h
               | x < 0 || y < 0   -> []
               | x < w && y < h   -> maybeToList (fmap (z,) currentColor)
               | otherwise        -> []
         NodeGroup xs          -> join (fmap (go currentColor z x y) xs)
         NodeTransform t -> case t of
            Translate mx my mz g' -> go currentColor (z+mz) (x-mx) (y-my) g'
         Colorize c g'         -> go (Just c) z x y g'
         Named _ g'            -> go currentColor z x y g'
         NodeClip g' cg
            -- we check that the clipper graph returns a color
            | null (go currentColor z x y cg) -> []
            | otherwise                       -> go currentColor z x y g'

-- | Modify a node by name
modifyByName :: Eq n => (SceneGraph d c n -> SceneGraph d c n) -> n -> SceneGraph d c n -> SceneGraph d c n
modifyByName f n g = case g of
   NodeObject o -> case o of
      Rectangle _ _      -> g
   NodeGroup xs          -> NodeGroup (fmap (modifyByName f n) xs)
   NodeTransform t -> case t of
      Translate mx my mz g' -> NodeTransform <| Translate mx my mz (modifyByName f n g')
   Colorize c g'         -> Colorize c (modifyByName f n g')
   Named n' g'
      | n == n'   -> Named n' (f g')
      | otherwise -> Named n' (modifyByName f n g')
   NodeClip g' cg -> NodeClip (modifyByName f n g') (modifyByName f n cg)

-- | Find a node by name
findByName :: Eq n => n -> SceneGraph d c n -> Maybe (SceneGraph d c n)
findByName n g = case g of
   NodeObject o -> case o of
      Rectangle _ _      -> Nothing
   NodeGroup xs           -> headMaybe (catMaybes (fmap (findByName n) xs))
   NodeTransform t -> case t of
      Translate _ _ _ g' -> findByName n g'
   Colorize _ g'      -> findByName n g'
   Named n' g'
      | n == n'   -> Just g'
      | otherwise -> findByName n g'
   NodeClip g' cg -> findByName n g' <|> findByName n cg

data Color = Color Float Float Float

data Name
   = Root
   | CustomName String

data Canvas = Canvas
   { canvasGraph :: SceneGraph Float Color Name
   }
