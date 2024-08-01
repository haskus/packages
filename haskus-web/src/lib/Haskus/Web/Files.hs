{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskus.Web.Files
   ( scriptFiles
   , cssFiles
   )
where

import Haskus.Utils.Embed.ByteString

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text
import Data.ByteString

-- | Map (FileName,Contents)
scriptFiles :: Map Text ByteString
scriptFiles = Map.fromList
   [("jquery.min.js"         ,$(embedBSFile "src/scripts/jquery-3.2.1.min.js"))
   ,("jquery-ui.min.js"      ,$(embedBSFile "src/scripts/jquery-ui.min.js"))
   ,("jquery-ui.js"          ,$(embedBSFile "src/scripts/jquery-ui.js"))
   ,("jquery-ui-touch.min.js",$(embedBSFile "src/scripts/jquery.ui.touch-punch.min.js"))
   ,("jquery-ui-block.js"    ,$(embedBSFile "src/scripts/jquery.blockUI.js"))
   ,("jquery-imgload.min.js" ,$(embedBSFile "src/scripts/imagesloaded.pkgd.min.js"))
   ,("jquery-imgload.js"     ,$(embedBSFile "src/scripts/imagesloaded.pkgd.js"))
   ,("d3.js"                 ,$(embedBSFile "src/scripts/d3.v5.js"))
   ,("d3.min.js"             ,$(embedBSFile "src/scripts/d3.v5.min.js"))
   ]

-- | Map (FileName,Contents)
cssFiles :: Map Text ByteString
cssFiles = Map.fromList
   [("jquery-ui.css"     ,$(embedBSFile "src/css/jquery-ui.theme.css"))
   ]
