{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Scene graph and canvas inspired by
-- * EFL Evas
-- * Qt QML: https://doc.qt.io/qt-5/qtquick-visualcanvas-scenegraph.html
module Haskus.UI.Canvas
   ( SceneGraph(..)
   , RenderObject (..)
   , Transform (..)
   , colorsAt
   , Color (..)
   , Name (..)
   , Canvas (..)
   , nodeModifyByName
   , nodeByName
   , rootNode
   )
where

import Haskus.Utils.Monad
import Haskus.Utils.Maybe
import qualified Haskus.Utils.Map as Map
import Haskus.Utils.Map (Map)

data RenderObject d c
   = Rectangle d d -- ^ Rectangle (width,height,1) with top-left at current (x,y,z)

data Transform d g
   = Translate d d d g -- ^ Translate (x,y,z) the sub-graph in the space
   -- TODO: use a 4x4 matrix

data SceneGraph d c n = SceneGraph
   { sceneGraphs   :: Map n (SceneNode d c n)
   , sceneRootName :: n
   }

data SceneNode d c n
   = NodeObject (RenderObject d c)                 -- ^ Renderable objects
   | NodeGroup [SceneNode d c n]                   -- ^ NodeGroup of nodes
   | NodeTransform (Transform d (SceneNode d c n)) -- ^ Transformations
   | Colorize c (SceneNode d c n)                  -- ^ Set the default color for the sub-graph
   | NodeClip (SceneNode d c n) (SceneNode d c n)  -- ^ Object clipping
   | NodeDisable (SceneNode d c n)                 -- ^ Disable the sub-graph (won't be rendered)
   | NodeRef n                                     -- ^ Reference a sub-graph by its name


-- | Return a color and a Z-order
colorsAt :: (Num d, Ord d, Ord n) => d -> d -> SceneGraph d c n -> [(d,c)]
colorsAt atx aty g = go Nothing 0 atx aty (rootNode g)
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
         NodeClip g' cg
            -- we check that the clipper graph returns a color
            | null (go currentColor z x y cg) -> []
            | otherwise                       -> go currentColor z x y g'
         NodeDisable _ -> []
         NodeRef n     -> fromMaybe [] (go currentColor z x y <$> nodeByName n g)

-- | Modify a node by name
nodeModifyByName :: (Ord n, Eq n) => (SceneNode d c n -> SceneNode d c n) -> n -> SceneGraph d c n -> SceneGraph d c n
nodeModifyByName f n g = case nodeByName n g of
   Nothing -> g
   Just g' -> g { sceneGraphs = Map.insert n (f g') (sceneGraphs g) }

-- | Find a node by name
nodeByName :: (Ord n, Eq n) => n -> SceneGraph d c n -> Maybe (SceneNode d c n)
nodeByName n g = Map.lookup n (sceneGraphs g)

-- | Get the root node
rootNode :: (Ord n) => SceneGraph d c n -> SceneNode d c n
rootNode g = fromMaybe (NodeGroup []) (nodeByName (sceneRootName g) g)

data Color = Color Float Float Float

data Name
   = Root
   | CustomName String

data Canvas = Canvas
   { canvasGraph :: SceneGraph Float Color Name
   }
