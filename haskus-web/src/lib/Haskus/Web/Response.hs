module Haskus.Web.Response
   ( Response (..)
   , sendJson
   , sendJS
   , sendPNG
   , sendJPG
   , sendBinary
   )
where

import qualified Data.ByteString as BS

data Response = Response
   { responseMime     :: String
   , responseContents :: BS.ByteString
   }

-- | Send JSON text
sendJson :: BS.ByteString -> Response
sendJson = Response "application/json"

-- | Send JS script
sendJS :: BS.ByteString -> Response
sendJS = Response "text/javascript"

-- | Send binary data
sendBinary :: BS.ByteString -> Response
sendBinary = Response "application/octet-stream"

-- | Send a PNG image
sendPNG :: BS.ByteString -> Response
sendPNG = Response "image/png"

-- | Send a JPG image
sendJPG :: BS.ByteString -> Response
sendJPG = Response "image/jpg"
