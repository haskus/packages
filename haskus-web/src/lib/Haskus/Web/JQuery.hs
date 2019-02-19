{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskus.Web.JQuery
   ( jqueryHtmlHeader
   , jqueryFiles
   )
where

import Haskus.Web.Response
import Haskus.Web.Server
import Haskus.Web.Html
import Haskus.Utils.Embed.ByteString

import Data.Text
import Control.Monad

-- | Add jquery headers
jqueryHtmlHeader :: Bool -> Html ()
jqueryHtmlHeader minimized = do
   let
      addScript :: Text -> Text -> Html ()
      addScript mini normal =
         script_ [ src_ (if minimized then mini else normal) ] (mempty :: Html ())

   addScript
      "/script/jquery/jquery-3.2.1.min.js"
      "/script/jquery/jquery-3.2.1.min.js"   -- TODO: add normal jquery

   -- JQuery UI: some widgets
   addScript
      "/script/jquery/jquery-ui.min.js"
      "/script/jquery/jquery-ui.js"

   -- allow jquery UI to support Touch Screens
   addScript
      "/script/jquery/jquery.ui.touch-punch.min.js"
      "/script/jquery/jquery.ui.touch-punch.min.js" --TODO: add normal script

   -- ability to block the whole screen with a JS popup
   addScript
      "/script/jquery/jquery.blockUI.js" --TODO: minimized script
      "/script/jquery/jquery.blockUI.js"

   -- ability to detect that images are loaded
   -- From: https://github.com/desandro/imagesloaded
   addScript
      "/script/jquery/imagesloaded.pkgd.min.js"
      "/script/jquery/imagesloaded.pkgd.js"

   link_ [ rel_  "stylesheet"
         , type_ "text/css"
         , href_ "/style/jquery/jquery-ui.theme.css"
         ]

-- | Serve JQuery files
jqueryFiles :: ServerPartT IO Response
jqueryFiles = msum
   [ dir "script" $ dir "jquery" $ msum
      [ dir "jquery-3.2.1.min.js"          $ sendJS $ $(embedBSFile "src/scripts/jquery-3.2.1.min.js")
      , dir "jquery-ui.min.js"             $ sendJS $ $(embedBSFile "src/scripts/jquery-ui.min.js")
      , dir "jquery-ui.js"                 $ sendJS $ $(embedBSFile "src/scripts/jquery-ui.js")
      , dir "jquery.ui.touch-punch.min.js" $ sendJS $ $(embedBSFile "src/scripts/jquery.ui.touch-punch.min.js")
      , dir "jquery.blockUI.js"            $ sendJS $ $(embedBSFile "src/scripts/jquery.blockUI.js")
      , dir "imagesloaded.pkgd.min.js"     $ sendJS $ $(embedBSFile "src/scripts/imagesloaded.pkgd.min.js")
      , dir "imagesloaded.pkgd.js"         $ sendJS $ $(embedBSFile "src/scripts/imagesloaded.pkgd.js")
      ]
   , dir "style" $ dir "jquery" $ msum
      [ dir "jquery-ui.theme.css"            $ sendJS $ $(embedBSFile "src/css/jquery-ui.theme.css")
      ]
   ]
