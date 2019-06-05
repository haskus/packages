{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Haskus.Web.Html
   ( module Lucid
   , module Lucid.Base
   , renderText'
   , minLength_
   , emptyAttribute
   , nbsp_
   , lightTripleDashVert
   , property_
   , prefix_
   )
where

import Lucid
import Lucid.Base
import Data.Text
import Data.Text.Lazy (toStrict)

renderText' :: Html a -> Text
renderText' = toStrict . renderText

minLength_ :: Text -> Attribute
minLength_ = makeAttribute "minlength"

property_ :: Text -> Attribute
property_ = makeAttribute "property"

prefix_ :: Text -> Attribute
prefix_ = makeAttribute "prefix"

emptyAttribute :: Attribute
emptyAttribute = makeAttribute mempty mempty

nbsp_ :: Html ()
nbsp_ = toHtmlRaw ("&nbsp;" :: Text)

lightTripleDashVert :: Html ()
lightTripleDashVert = toHtmlRaw ("&#9478;" :: Text)
