{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskus.Web.JQuery
   ( jqueryHtmlHeader
   )
where

import Haskus.Web.Html
import Data.Text

-- | Add jquery headers
jqueryHtmlHeader :: Bool -> Html ()
jqueryHtmlHeader minimized = do
   let
      addScript :: Text -> Text -> Html ()
      addScript mini normal =
         script_ [ src_ (if minimized then mini else normal) ] (mempty :: Html ())

   addScript
      "/script/jquery.min.js"
      "/script/jquery.min.js"   -- TODO: add normal jquery

   -- JQuery UI: some widgets
   addScript
      "/script/jquery-ui.min.js"
      "/script/jquery-ui.js"

   -- allow jquery UI to support Touch Screens
   addScript
      "/script/jquery-ui-touch.min.js"
      "/script/jquery-ui-touch.min.js" --TODO: add normal script

   -- ability to block the whole screen with a JS popup
   addScript
      "/script/jquery-ui-block.js" --TODO: minimized script
      "/script/jquery-ui-block.js"

   -- ability to detect that images are loaded
   -- From: https://github.com/desandro/imagesloaded
   addScript
      "/script/jquery-imgload.min.js"
      "/script/jquery-imgload.js"

   link_ [ rel_  "stylesheet"
         , type_ "text/css"
         , href_ "/style/jquery-ui.css"
         ]
