{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CountVonCount.Web.Helpers where

import Prelude hiding (div)

import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

stylesheet :: H.AttributeValue -> H.Html
stylesheet link = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href link

javascript :: H.AttributeValue -> H.Html
javascript link = H.script ! A.type_ "text/JavaScript" ! A.src link $ ""

linkTo :: H.AttributeValue -> H.Html -> H.Html
linkTo url name = H.a ! A.href url $ name

div :: H.AttributeValue -> H.Html -> H.Html
div i = H.div ! A.id i

div_ :: H.AttributeValue -> H.Html
div_ i = div i $ ""

divC :: H.AttributeValue -> H.Html -> H.Html
divC cl = H.div ! A.class_ cl

postForm uri = H.form ! A.action (H.toValue uri) ! A.method "post"
