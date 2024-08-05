module Haskus.Apps.System.Build.CmdLine
   ( BuildOptions (..)
   , TestOptions (..)
   , MakeDiskOptions (..)
   , MakeDeviceOptions (..)
   , buildOptions
   , testOptions
   , makeDiskOptions
   , makeDeviceOptions
   )
where

import Options.Applicative

data TestOptions = TestOptions
   { testOptInit :: String
   }

testOptions :: Parser TestOptions
testOptions =
   TestOptions
      <$> strOption
         (  long "init"
         <> metavar "INIT-PROGRAM"
         <> value ""
         <> help "Init program to use"
         )

data BuildOptions = BuildOptions
   { buildOptInit :: String
   }

buildOptions :: Parser BuildOptions
buildOptions =
   BuildOptions
      <$> strOption
         (  long "init"
         <> metavar "INIT-PROGRAM"
         <> value ""
         <> help "Init program to use"
         )

data MakeDiskOptions = MakeDiskOptions
   { diskOptPath :: String
   }

makeDiskOptions :: Parser MakeDiskOptions
makeDiskOptions =
   MakeDiskOptions
      <$> strOption
         (  long "output"
         <> short 'o'
         <> metavar "OUTPUT-DIRECTORY"
         <> help "Output disk directory"
         )

data MakeDeviceOptions = MakeDeviceOptions
   { deviceOptPath :: String
   }

makeDeviceOptions :: Parser MakeDeviceOptions
makeDeviceOptions =
   MakeDeviceOptions
      <$> strOption
         (  long "device"
         <> short 'd'
         <> metavar "DEVICE"
         <> help "Device path"
         )
