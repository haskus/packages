{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

import Haskus.System

import qualified Haskus.Binary.Buffer as B

import Haskus.System.Linux.Info
import Haskus.System.Linux.Graphics.State
import Haskus.System.Linux.Graphics.Mode
import qualified Haskus.System.Linux.Internals.Input as Key
import Haskus.System.Graphics.Drawing
import Haskus.System.Graphics.Diagrams (mkWidth, rasterizeDiagram)
import Haskus.Utils.Embed.ByteString
import Haskus.Utils.STM
import Haskus.Data.Variant.EADT
import Haskus.Utils.Maybe
import Haskus.Format.String
import qualified Haskus.Utils.Map as Map

import Haskus.UI.RayTracer
import Haskus.UI.Canvas
import Haskus.UI.Color

import Data.Char
import Codec.Picture.Types
import Graphics.Text.TrueType
import qualified Data.ByteString.Lazy as LBS

import Demo.Diagrams
import Demo.Graphics
import Demo.Art

rawlogo :: B.Buffer
rawlogo = B.Buffer $(embedBSFile "src/image/logo_transparent.png")

data Page
   = PageNone
   | PageInfo
   | PageGraphics
   | PageTerminal
   | PageArt
   | PageCanvas
   deriving (Show,Eq)

main :: IO ()
main = runSys' <| do

   let logo = loadPng rawlogo

   let
   -- fonts
      loadFont             = decodeFont . LBS.fromStrict
      Right fontNormal     = loadFont $(embedBSFile "src/demo/VeraMono.ttf")
      Right fontBold       = loadFont $(embedBSFile "src/demo/VeraMoBd.ttf")
      Right fontBoldItalic = loadFont $(embedBSFile "src/demo/VeraMoBI.ttf")
      Right fontItalic     = loadFont $(embedBSFile "src/demo/VeraMoIt.ttf")
   
   let blackTexture = Just . uniformTexture <| PixelRGBA8 0 0 0 255
       redTexture   = Just . uniformTexture <| PixelRGBA8 255 0 0 255
       gradDef      = [ (0  , PixelRGBA8 0 0x86 0xc1 255)
                      , (0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
                      , (1  , PixelRGBA8 0xFF 0x53 0x73 255)
                      ]
       rainbowTexture   = Just (linearGradientTexture gradDef (V2 40 40) (V2 130 130))

   term <- defaultTerminal
   sys  <- defaultSystemInit
   let dm = systemDeviceManager sys

   info <- runE systemInfo

   -- wait for mouse driver to be loaded (FIXME: use plug-and-play detection)
   threadDelaySec 2

   -- Terminal writer thread (can be used to print things into STM context)
   (termBufWrite,termBufRead) <- atomically do
      w <- newBroadcastTChan
      r <- dupTChan w
      return (w,r)
   sysFork "Terminal write thread" <| do
      forever (writeStrLn term =<< atomically (readTChan termBufRead))

   -- write on the terminal in STM context
   let termWriteSTM x = writeTChan termBufWrite x

   -------------------------------------------------
   -- redrawing management
   -------------------------------------------------
   needRedrawVar <- newTVarIO True
   let needRedraw   = writeTVar needRedrawVar True
       redrawNeeded = readTVar needRedrawVar
       redrawing    = writeTVar needRedrawVar False
   -------------------------------------------------


   -------------------------------------------------
   -- system exit management
   -------------------------------------------------
   quitKey <- newTVarIO False

   let mustQuit     = writeTVar quitKey True
       quitRequired = readTVar quitKey

   -------------------------------------------------


   -------------------------------------------------
   -- page management
   -------------------------------------------------
   page    <- newTVarIO PageNone

   let changePage newp = do
         oldp <- readTVar page
         when (oldp /= newp) $ do
            writeTVar page newp
            needRedraw
       currentPage   = readTVar page
       currentPageIO = readTVarIO page
   -------------------------------------------------


   -------------------------------------------------
   -- mouse management
   -------------------------------------------------
   mousePos <- newTVarIO (250.0,250.0)

   let
      wf = 1024 / 0x7FFF :: Float
      hf = 768  / 0x7FFF :: Float
      changeMousePos f     = do
         modifyTVar mousePos f
         needRedraw
      getMousePosIO        = readTVarIO mousePos
      updateMouseRel dx dy = changeMousePos (\(x,y) -> (x+fromIntegral dx,y+fromIntegral dy))
      updateMouseAbsX v    = changeMousePos (\(_,y) -> (fromIntegral v * wf,y))
      updateMouseAbsY v    = changeMousePos (\(x,_) -> (x,fromIntegral v * hf))
   -------------------------------------------------


   -------------------------------------------------
   -- terminal management
   -------------------------------------------------
   termContents <- newTVarIO [[TextRange fontBold (PointSize 12) "Enter commands in the terminal:" redTexture]]
   termStr <- newTVarIO ""

   let
      termAppend s = do
         modifyTVar termStr (++ s)
         needRedraw
      termModifyContents f = do
         modifyTVar termContents f
         needRedraw
      termModifyCurrent f = do
         modifyTVar termStr f
         needRedraw
      termAppendStyled s = termModifyContents $ \case
            []     -> [[s]]
            (x:xs) -> (x ++ [s]):xs
      termAppendStyledLn s = do
         termAppendStyled s
         termNewLine
      termNewLine = termModifyContents ([]:)
      termPrompt = termAppendStyled (TextRange fontBoldItalic (PointSize 12) "$> " blackTexture)
      termBackErase = termModifyCurrent $ \case
            "" -> ""
            s  -> init s
         
      termEnter  = do
         s <- readTVar termStr
         termAppendStyledLn (TextRange fontNormal (PointSize 12) s blackTexture)
         case s of
            "exit" -> do
               termAppendStyledLn (TextRange fontBoldItalic (PointSize 12) "Exiting..." blackTexture)
               mustQuit
            "help" -> do
               termAppendStyledLn (TextRange fontBoldItalic (PointSize 16) "You are on your own for now..." rainbowTexture)
            "uname" -> do
               
               let res = case info of
                     VRight info' -> fromCStringBuffer (systemName info') ++ " " 
                           ++ fromCStringBuffer (systemRelease info')
                           ++ " (" ++ fromCStringBuffer (systemMachine info') ++ ") - "
                           ++ fromCStringBuffer (systemVersion info')
                     VLeft _ -> "Information unavailable"
               termAppendStyled (TextRange fontItalic (PointSize 12) res blackTexture)
               termNewLine
            _       -> do
               termAppendStyled (TextRange fontItalic (PointSize 12) "Unknown command" redTexture)
               termNewLine
         termPrompt
         writeTVar termStr ""
         needRedraw
      termGetContents = do
         cs <- readTVar termContents
         c  <- readTVar termStr
         let c' = [TextRange fontNormal (PointSize 12) c blackTexture]
         return $ case cs of
            []     -> [c']
            (x:xs) -> (x++c'):xs

   atomically (termNewLine >> termPrompt)
   -------------------------------------------------

         
   -------------------------------------------------
   -- Char map management
   -------------------------------------------------
   let
      charMapFr = charMapEn . f
         where f = \case
                  Q         -> A
                  A         -> Q
                  M         -> SemiColon
                  SemiColon -> M
                  Z         -> W
                  W         -> Z
                  k         -> k

      charMapEn = \case
         Key.Space -> " "
         Key0      -> "0"
         Key1      -> "1"
         Key2      -> "2"
         Key3      -> "3"
         Key4      -> "4"
         Key5      -> "5"
         Key6      -> "6"
         Key7      -> "7"
         Key8      -> "8"
         Key9      -> "9"
         Minus     -> "-"
         Equal     -> "="
         Tab       -> "    "
         SemiColon -> ";"
         Slash     -> "/"
         BackSlash -> "\\"
         Comma     -> ","
         Dot       -> "."
         k         -> fmap toLower (show k)
   -------------------------------------------------

   writeStrLn term "Loading input devices..."
   inputs <- loadInputDevices dm
   forM_ inputs \inp -> onEvent (inputDeviceBundles inp) \(InputEventBundle events) -> do
      atomically <| forM_ (fmap inputEventType events) \case
         -- mouse move: using qemu -show-cursor
         InputRelativeEvent RelativeX v -> updateMouseRel v 0
         InputRelativeEvent RelativeY v -> updateMouseRel 0 v
         -- mouse move: using qemu -usbdevice tablet
         InputAbsoluteEvent AbsoluteX v -> updateMouseAbsX v
         InputAbsoluteEvent AbsoluteY v -> updateMouseAbsY v
         InputKeyEvent KeyPress k       -> do
            p <- currentPage
            case k of
               Esc -> case p of
                  PageNone -> mustQuit
                  _        -> changePage PageNone
               F1  -> changePage PageInfo
               F2  -> changePage PageGraphics
               F3  -> changePage PageTerminal
               F4  -> changePage PageArt
               F5  -> changePage PageCanvas
               MouseLeft   -> termWriteSTM "Left click!"
               MouseRight  -> termWriteSTM "Right click!"
               MouseMiddle -> termWriteSTM "Middle click!"
               x   -> case p of
                  PageTerminal -> do
                     case x of
                        BackSpace -> termBackErase
                        Enter     -> termEnter
                        _         -> termAppend (charMapFr x)
                  _        -> return ()
         _                              -> return ()

   cards <- loadGraphicCards dm

   forM_ cards \card -> do

      state <- assertE "Get entities"
                  <| getHandleEntitiesMap (graphicCardHandle card)

      encoders <- assertE "Read encoders"
                  <| getHandleEncoders (graphicCardHandle card)
      let encoderMap = Map.fromList (fmap encoderID encoders `zip` encoders)

      -- get connectors
      conns <- if Map.null (entitiesConnectorsMap state)
         then sysError "No graphics connector found" 
         else return (Map.elems (entitiesConnectorsMap state))

      let
         isValid x  = case connectorState x of
            Connected d -> not (null <| displayModes d)
            _           -> False
         validConns = filter isValid conns

         -- select first connector
         conn = head validConns

         Connected connDev = connectorState conn

         -- select highest mode
         mode   = head (displayModes connDev)
         width, height :: Double
         width  = fromIntegral <| modeHorizontalDisplay mode
         height = fromIntegral <| modeVerticalDisplay mode

      let defaultCtrl = do
            encId  <- connectorEncoderID conn
            enc    <- Map.lookup encId encoderMap
            ctrlId <- encoderControllerID enc
            Map.lookup ctrlId (entitiesControllersMap state)

          Just ctrl = case defaultCtrl of
            -- we already have a connected controller, use it
            Just c  -> Just c
            -- we need to select a controller and an encoder
            Nothing -> do
               encId  <- headMaybe (connectorPossibleEncoderIDs conn)
               enc    <- Map.lookup encId encoderMap
               ctrlId <- headMaybe (encoderPossibleControllers enc)
               Map.lookup ctrlId (entitiesControllersMap state)
            

      let
         bgColor  = 0x316594
         ptrColor = PixelRGBA8 0 0 0 255
         --whiteClr = PixelRGBA8 255 255 255 255
         trans    = PixelRGBA8 0 0 0 0
         ptrLen   = 8 :: Int
         ptrLenP1 = fromIntegral ptrLen + 1
         ptrWidth = 2*ptrLen+1
         ptrWidth'= fromIntegral ptrWidth
         ptr      = renderDrawing ptrWidth ptrWidth trans <| do
                        withTexture (uniformTexture ptrColor) <| do
                           stroke 1 (JoinMiter 0) (CapStraight 0, CapStraight 0)
                               <| line (V2 ptrLenP1 0) (V2 ptrLenP1 ptrWidth')
                           stroke 1 (JoinMiter 0) (CapStraight 0, CapStraight 0)
                               <| line (V2 0 ptrLenP1) (V2 ptrWidth' ptrLenP1)
         topBarDiagram = rasterizeDiagram (mkWidth (realToFrac width)) 
                                 (topBarDiag (realToFrac width) 
                                             (realToFrac height))
         makeInfoPage i = rasterizeDiagram (mkWidth (realToFrac width)) (infoPageDiag i)
         infoPage = makeInfoPage <$> info

      canvasPage <- do
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
            viewport = Viewport
               { viewportCenter      = V3 0 0 100
               , viewportNormal      = V3 0 0 (-1)
               , viewportResolutionX = 200
               , viewportResolutionY = 200
               , viewportPixelSize   = 1.0
               }


         renderScene sceneRenderer rectWorld viewport Nothing
            ||> promoteImage -- convert RGB8 into RGBA8
            |> liftIO

      initRenderingEngine card ctrl mode conn 3 [WaitDrawn,WaitPending] <| \_ gfb -> do
         let
            centerPos x = ( (floor width  - imageWidth x ) `div` 2
                          , (floor height - imageHeight x) `div` 2
                          )

            fullImg x   = ( 0
                          , 0
                          , imageWidth x
                          , imageHeight x
                          )

         do
            -- wait if redrawing is not needed
            atomically $ do
               redrawNeeded >>= \case
                  False -> retry
                  True  -> redrawing

            (mx,my) <- getMousePosIO
            liftIO <| fillFrame gfb bgColor
            currentPageIO >>= \case
               PageNone -> liftIO <| blendImage gfb logo BlendAlpha (centerPos logo) (fullImg logo)

               PageInfo -> case infoPage of
                  VRight d -> liftIO <| blendImage gfb d BlendAlpha (centerPos d) (fullImg d)
                  VLeft _  -> return ()

               PageGraphics -> runE_ do
                  diag <- graphicsPage card
                  let d = rasterizeDiagram (mkWidth (realToFrac width)) diag
                  liftIO <| blendImage gfb d BlendAlpha (centerPos d) (fullImg d)

               PageArt -> do
                  let d = makeArt 1520476193207 60 60 12
                  liftIO <| blendImage gfb d BlendAlpha (centerPos d) (fullImg d)

               PageTerminal -> do
                  termLines <- atomically termGetContents
                  let abcd = renderDrawing 1000 700 (PixelRGBA8 255 255 255 255)
                              . withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $ do
                                  forM_ (reverse termLines `zip` [0..]) $ \(rs,i) ->
                                    printTextRanges (V2 20 (20*i + 20)) rs

                  liftIO <| blendImage gfb abcd BlendAlpha (10,50) (fullImg abcd)

               PageCanvas -> do
                  liftIO <| blendImage gfb canvasPage BlendAlpha (10,50) (fullImg canvasPage)


            liftIO <| blendImage gfb topBarDiagram BlendAlpha (0,0) (fullImg topBarDiagram)
            liftIO <| blendImage gfb ptr BlendAlpha (floor mx-ptrLen,floor my-ptrLen) (fullImg ptr)



   writeStrLn term "Done."

   -- wait for a key in the standard input (in the console) or ESC (in the graphic interface)
   sysFork "Terminal wait for key" do
      waitForKey term
      atomically mustQuit
   
   atomically <| do
      q <- quitRequired
      unless q retry

   writeStrLn term "Log:"
   sysLogPrint

   -- shutdown the computer
   powerOff
