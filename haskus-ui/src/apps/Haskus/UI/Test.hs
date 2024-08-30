{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}

import Haskus.UI.RayTracer
import Haskus.UI.Color
import Haskus.UI.Picture
import Haskus.UI.Canvas

import Haskus.Data.Variant.EADT
import Haskus.Utils.Flow


main :: IO ()
main = do
   void demo76_79


-- demo from page 76-77 of "Ray-tracing from the ground-up"
demo76_79 :: IO ()
demo76_79 = do

   let
      world :: World PixelRGB8 (EADT '[ColoredF, PlaneF, SphereF])
      world = defaultWorld
         { worldObjects =
            [ Colored red       <| Sphere (V3 0 (-25) 0) 80
            , Colored yellow    <| Sphere (V3 0 30 0)    60
            , Colored darkgreen <| Plane  (V3 0 0 0) (V3 0 1 1)
            ]
         , worldBackgroundColor = toPixelRGB8 black
         }

      viewport = Viewport
         { viewportCenter      = V3 0 0 100
         , viewportNormal      = V3 0 0 (-1)
         , viewportResolutionX = 200
         , viewportResolutionY = 200
         , viewportPixelSize   = 1.0
         }

      viewport2 = viewport
         { viewportResolutionX = 300
         , viewportResolutionY = 300
         }

   void <| renderScene defaultRenderer world viewport  (Just "suffern_76.png")
   void <| renderScene defaultRenderer world viewport2 (Just "suffern_79.png")

   let reg4 = defaultRenderer
               { rendererSquareSampler = regularSquareSampler 4
               }
   void <| renderScene reg4 world viewport  (Just "suffern_76_regular4constant.png")

   let reg4lin = defaultRenderer
               { rendererSquareSampler = regularSquareSampler 4
               , rendererBlender       = linearBlender
               }
   void <| renderScene reg4lin world viewport  (Just "suffern_76_regular4linear.png")


   let rand4 = defaultRenderer
               { rendererSquareSampler = randomSquareSampler 4
               }
   void <| renderScene rand4 world viewport  (Just "suffern_76_rand4.png")

   let jittered4 = defaultRenderer
               { rendererSquareSampler = jitteredSquareSampler 4
               }
   void <| renderScene jittered4 world viewport  (Just "suffern_76_jittered4.png")

   let nRooks16 = defaultRenderer
               { rendererSquareSampler = nRooksSquareSampler 16
               }
   void <| renderScene nRooks16 world viewport  (Just "suffern_76_nrooks16.png")

   let multijittered4 = defaultRenderer
               { rendererSquareSampler = multiJitteredSquareSampler 4
               }
   void <| renderScene multijittered4 world viewport  (Just "suffern_76_multijittered4.png")

   let hammersley4 = defaultRenderer
               { rendererSquareSampler = hammersleySquareSampler 4
               }
   void <| renderScene hammersley4 world viewport  (Just "suffern_76_hammersley4.png")

   -----------------------------------------
   -- Rendering scene graphs

   let
      sceneGraph :: SceneGraph Dist Color NodeName
      sceneGraph = defaultSceneGraph
         |> sceneInsertNode (NodeName "Clipper")
               (Colorize yellow <| NodeObject (Disc 8))
         |> sceneInsertNode RootNode
               (NodeGroup
                  [NodeClip (NodeRef (NodeName "Clipper"))
                     <| Colorize red  <| NodeObject (Rectangle 10 5)
                  , Colorize blue <| NodeTransform (Translate (-5) (-2) 1) <| NodeObject (Rectangle 10 5)
                  , Colorize yellow
                     -- <| NodeTransform (Translate 10 10 2)
                     <| NodeRef (NodeName "Clipper")
                  ])

      rectWorld :: World PixelRGB8 (EADT '[CanvasF Dist Color NodeName, ColoredF, PlaneF, SphereF])
      rectWorld = defaultWorld
         { worldObjects         = [Canvas sceneGraph]
         , worldBackgroundColor = toPixelRGB8 black
         }

      sceneRenderer = defaultRenderer
               { rendererSquareSampler = hammersleySquareSampler 4
               }

   void <| renderScene sceneRenderer rectWorld viewport  (Just "scene_rects.png")
