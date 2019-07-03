{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Haskus.System
import Haskus.System.Linux.Graphics.Property
import Haskus.System.Linux.Graphics.Object
import Haskus.System.Linux.Graphics.AtomicConfig

main :: IO ()
main = runSys' do
   sys   <- defaultSystemInit
   term  <- defaultTerminal
   cards <- loadGraphicCards (systemDeviceManager sys)
   
   forM_ cards \card -> do
      let
         showProps o = do
            -- get properties of object o
            mprops <- graphicsConfig (graphicCardHandle card) do
                        runE (getObjectProperties o)
            -- show them
            forM_ mprops \props -> do
               writeStrLn term ("* " ++ showObjectQualifiedID o)
               forM_ props \prop ->
                  writeStrLn term ("    " ++ showProperty prop)

      entities <- getEntities card
                     |> assertLogShowErrorE "Get entities"

      mapM_ showProps (entitiesConnectors entities)
      mapM_ showProps (entitiesControllers entities)
      mapM_ showProps (entitiesPlanes entities)
      mapM_ showProps (entitiesFrames entities)

   powerOff
