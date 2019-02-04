module Haskus.Web.Response
   ( sendJson
   , sendJS
   , sendPNG
   , sendJPG
   , sendData
   , sendBinary
   , sendLazyJson
   , sendLazyData
   )
where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Char8 as BS8

import Haskus.Web.Server

-- | Send JSON text
sendJson :: BS.ByteString -> ServerPartT IO Response
sendJson = sendData "application/json"

-- | Send JS script
sendJS :: BS.ByteString -> ServerPartT IO Response
sendJS = sendData "text/javascript"

-- | Send binary data
sendBinary :: BS.ByteString -> ServerPartT IO Response
sendBinary = sendData "application/octet-stream"

-- | Send a PNG image
sendPNG :: BS.ByteString -> ServerPartT IO Response
sendPNG = sendData "image/png"

-- | Send a JPG image
sendJPG :: BS.ByteString -> ServerPartT IO Response
sendJPG = sendData "image/jpg"

-- | Send data with the given MIME content type
sendData :: String -> BS.ByteString -> ServerPartT IO Response
sendData mime dat = ok (toResponseBS (BS8.pack mime) (LBS.fromStrict dat))

-- | Send JSON text
sendLazyJson :: LBS.ByteString -> ServerPartT IO Response
sendLazyJson = sendLazyData "application/json"

-- | Send data with the given MIME content type
sendLazyData :: String -> LBS.ByteString -> ServerPartT IO Response
sendLazyData mime dat = ok (toResponseBS (BS8.pack mime) dat)

