{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskus.Web.Files
   ( serveFiles
   , scriptFiles
   , cssFiles
   )
where

import Haskus.Web.Response
import Haskus.Web.Server
import Haskus.Utils.Embed.ByteString

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text
import Data.ByteString
import Control.Monad

-- | Serve static files
serveFiles :: ServerPartT IO Response
serveFiles = msum
   [ dir "script" $ path $ \p ->
      case Map.lookup p scriptFiles of
         Nothing -> mzero
         Just c  -> sendJS c
   , dir "style" $ path $ \p ->
      case Map.lookup p cssFiles of
         Nothing -> mzero
         Just c  -> sendJS c
   ]

-- | Map (FileName,Contents)
scriptFiles :: Map Text ByteString
scriptFiles = Map.fromList
   [("jquery.min.js"         ,$(embedBSFilePrefix "haskus-web" "src/scripts/jquery-3.2.1.min.js"))
   ,("jquery-ui.min.js"      ,$(embedBSFilePrefix "haskus-web" "src/scripts/jquery-ui.min.js"))
   ,("jquery-ui.js"          ,$(embedBSFilePrefix "haskus-web" "src/scripts/jquery-ui.js"))
   ,("jquery-ui-touch.min.js",$(embedBSFilePrefix "haskus-web" "src/scripts/jquery.ui.touch-punch.min.js"))
   ,("jquery-ui-block.js"    ,$(embedBSFilePrefix "haskus-web" "src/scripts/jquery.blockUI.js"))
   ,("jquery-imgload.min.js" ,$(embedBSFilePrefix "haskus-web" "src/scripts/imagesloaded.pkgd.min.js"))
   ,("jquery-imgload.js"     ,$(embedBSFilePrefix "haskus-web" "src/scripts/imagesloaded.pkgd.js"))
   ]

-- | Map (FileName,Contents)
cssFiles :: Map Text ByteString
cssFiles = Map.fromList
   [("jquery-ui.css"     ,$(embedBSFilePrefix "haskus-web" "src/css/jquery-ui.theme.css"))
   ]
