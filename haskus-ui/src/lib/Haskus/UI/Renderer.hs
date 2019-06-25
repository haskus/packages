{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module Haskus.UI.Renderer
   ( Tracer
   , Renderer (..)
   , defaultRenderer
   , blackTracer
   , renderScene
   )
where

import Haskus.UI.Ray
import Haskus.UI.Object
import Haskus.UI.World
import Haskus.UI.Common
import Haskus.UI.Color
import Haskus.UI.Maths.Linear
import Haskus.UI.Picture
import Haskus.UI.Sampler

import Haskus.Utils.Flow
import Haskus.Utils.List
import Haskus.Utils.Maybe

import Codec.Picture.Png
import Foreign.Storable
import Control.Concurrent.Async
import Control.Applicative

type Tracer px obj = World px obj -> Ray -> Maybe Hit


data Renderer px obj = Renderer
   { rendererTracer          :: Tracer px obj             -- ^ Ray tracer
   , rendererSquareSampler   :: SamplerT IO [Sample]      -- ^ Centered square sampler
   , rendererBlender         :: [(Sample,Color)] -> Color -- ^ Color blender
   , rendererConvert         :: Color -> px               -- ^ Color converter
   , rendererBackgroundColor :: Color                     -- ^ Color when no hit
   }

defaultRenderer ::
   ( Object obj
   ) => Renderer PixelRGB8 obj
defaultRenderer = Renderer
   { rendererTracer          = blackTracer
   , rendererSquareSampler   = singleSampler
   , rendererBlender         = constantBlender
   , rendererConvert         = toPixelRGB8
   , rendererBackgroundColor = black
   }



-- | Draw objects in black if they have no color
blackTracer ::
   ( Object obj
   ) => Tracer px obj
blackTracer World{..} ray =
   worldObjects
      ||> hit ray
      |> catMaybes
      |> sortOn hitDistance
      |> headMaybe
      ||> (\x -> x { hitColor = hitColor x <|> Just black})
         

renderScene :: forall px obj.
   ( Pixel px
   , Storable (PixelBaseComponent px)
   , PngSavable px
   ) => Renderer px obj -> World px obj -> Viewport -> Maybe FilePath -> IO (Image px)
renderScene Renderer{..} world@World{..} Viewport{..} mpath = do
   mutImg <- createMutableImage
               (fromIntegral viewportResolutionX)
               (fromIntegral viewportResolutionY)
               worldBackgroundColor

   let
      -- Convert coordinates (x,y) that are in unit square (-0.5,+0.5)
      -- into pixel coordinates for viewport pixel at (px,py)
      toPixelSpace px py x y =
         let
            -- pixel center
            cx = viewportPixelSize * (fromIntegral px - 0.5 * (fromIntegral viewportResolutionX - 1.0))
            cy = -1 * viewportPixelSize * (fromIntegral py - 0.5 * (fromIntegral viewportResolutionY - 1.0))


         in ( cx + viewportPixelSize * x
            , cy + viewportPixelSize * y
            )
      
   forM_ [0..viewportResolutionY-1] <| \py ->
      forConcurrently_ [0..viewportResolutionX-1] <| \px -> do
         samples <- evalSamplerT rendererSquareSampler
         sampleColors <- forM samples <| \sample@(V2 x y) -> do
            let
               (cx,cy) = toPixelSpace px py x y
               ray = Ray
                       { rayOrigin    = viewportCenter ^+^ V3 cx cy 0
                       , rayDirection = viewportNormal
                       }
               h = rendererTracer world ray
            case h of
               Just (Hit{..})
                  | Just c <- hitColor -> return (sample,c)
               _                       -> return (sample,rendererBackgroundColor)
         let finalColor = rendererBlender sampleColors
         writePixel mutImg
            (fromIntegral px)
            (fromIntegral py)
            (rendererConvert finalColor)

   img <- unsafeFreezeImage mutImg

   forM_ mpath \path -> writePng path img
   return img
