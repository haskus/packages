{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Haskus.Apps.System.Build.Cabal
   ( cabalGetBinPath
   , cabalGetGHCVersion
   , cabalBuild
   )
where

import System.Process
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as L

import Haskus.Apps.System.Build.Utils
import Haskus.Utils.Flow

-- | Get GHC version (using stack exec)
cabalGetGHCVersion :: IO String
cabalGetGHCVersion =
   -- FIXME
   last . words <$> readProcess "cabal" ["exec", "--", "ghc", "--version"] ""

cabalGetBinPath :: Text -> IO FilePath
cabalGetBinPath x = do
  p <- readProcess "cabal" ["list-bin", Text.unpack x] ""

  when ("Error" `L.isPrefixOf` p) do
    error $ "Invalid Cabal executable name: " ++ p

  case lines p of
    [] -> error "cabal list-bin returned nothing"
    -- for some reason cabal adds some \n...
    (l:_) -> pure l


cabalBuild :: Text -> IO ()
cabalBuild x = do
   showStep "Building with Cabal..."
   shellWaitErr ("cabal build " <> Text.unpack x)
      <| failWith "Error during `stack build`"

