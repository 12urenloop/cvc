{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CountVonCount.Web.Helpers where

import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

stylesheet :: H.AttributeValue -> H.Html
stylesheet link = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href link

javascript :: H.AttributeValue -> H.Html
javascript link = H.script ! A.type_ "text/JavaScript" ! A.src link $ ""

link_to :: H.AttributeValue -> H.Html -> H.Html
link_to url name = H.a ! A.href url $ name

