module Haskus.Web.Server
   ( module Happstack.Server
   , okResponse
   , okResponsePage
   )
where

import Happstack.Server
import Haskus.Web.Page
import Haskus.Web.Html

-- | Return an HTML document
okResponse :: Html () -> ServerPartT IO Response
okResponse bdy = ok (toResponse (renderBS bdy))

-- | Return an HTML document with page headers
okResponsePage :: HtmlPageOpts -> Html () -> ServerPartT IO Response
okResponsePage opts bdy = okResponse (htmlPage opts bdy)
