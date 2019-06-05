{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Haskus.Web.Page
   ( HtmlPageOpts (..)
   , OpenGraph (..)
   , htmlPage
   )
where

import Lucid
import Data.Text
import Haskus.Utils.Flow
import Haskus.Web.Html
import Haskus.Web.JQuery

-- | HTML page options
data HtmlPageOpts = HtmlPageOpts
   { pageTitle      :: Html ()         -- ^ Title of the page
   , pageIsWebApp   :: Bool            -- ^ Is the page a web-app
   , pageOpenGraph  :: Maybe OpenGraph -- ^ Open Graph config
   , pageThemeColor :: Maybe Text      -- ^ Color of the address bar (on mobiles)
   , pageCss        :: [Text]          -- ^ Style files
   , pageJquery     :: Bool            -- ^ Enable JQuery
   , pageScripts    :: [Text]          -- ^ Script files
   , pageManifest   :: Maybe Text      -- ^ Manifest (json file)
   , pageIcon       :: Maybe Text      -- ^ Icon
   , pageCustomHead :: Html ()         -- ^ Additional custom head
   }

-- | Open graph options
data OpenGraph = OpenGraph
   { ogTitle    :: Text   -- ^ Title
   , ogType     :: Text   -- ^ Type
   , ogImages   :: [Text] -- ^ Images
   , ogUrl      :: Text   -- ^ URL
   , ogSiteName :: Text   -- ^ Site name
   }


htmlPage :: HtmlPageOpts -> Html () -> Html ()
htmlPage HtmlPageOpts{..} body = do
   doctype_

   let
      -- support open-graph prefix
      htmlProps = case pageOpenGraph of
         Nothing -> []
         Just _  -> [prefix_ "og: http://ogp.me/ns#" ] 

   html_ htmlProps <| do
      head_ <| do
         title_ pageTitle
         meta_ [ httpEquiv_ "Content-Type"
               , content_   "text/html;charset=utf-8"
               ]
         when pageIsWebApp <| do
            meta_ [ name_ "mobile-web-app-capable"
                  , content_ "yes"
                  ]
            meta_ [ name_ "apple-mobile-web-app-capable"
                  , content_ "yes"
                  ]

         -- Open-Graph protocol (see ogp.me)
         -- Allows image, title and text when sharing the page (on Facebook, etc.)
         forM_ pageOpenGraph \OpenGraph{..} -> do
            meta_ [ property_ "og:title"
                  , content_ ogTitle
                  ]
            meta_ [ property_ "og:type"
                  , content_ ogType
                  ]
            forM_ ogImages \i -> do
               meta_ [ property_ "og:image"
                     , content_ i
                     ]

            meta_ [ property_ "og:url"
                  , content_ ogUrl
                  ]
            meta_ [ property_ "og:site_name"
                  , content_ ogSiteName
                  ]

         -- viewport for mobile devices
         -- See also: @viewport CSS property
         -- (not currently implemented)
         meta_ [ name_ "viewport"
               , content_ "width=device-width, initial-scale=1.0, shrink-to-fit=no"
               ]

         -- mobile address bar color
         forM_ pageThemeColor \col ->
            meta_ [ name_ "theme-color"
                  , content_ col
                  ]

         forM_ pageCss \css ->
            link_ [ rel_  "stylesheet"
                  , type_ "text/css"
                  , href_ css
                  ]

         forM_ pageManifest \man ->
            link_ [ rel_ "manifest"
                  , href_ man
                  ]

         forM_ pageIcon \i ->
            link_ [ rel_ "icon"
                  , href_ i
                  ]

         when pageJquery <|
            jqueryHtmlHeader True -- minimized scripts

         forM_ pageScripts \sc ->
            script_ [ src_ sc] ("" :: String)

         pageCustomHead

      body_ body
