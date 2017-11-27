{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web.Views.Util
    ( stylesheet
    , javascript
    , linkTo
    , buttonTo
    , block
    , postForm
    ) where

import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

stylesheet :: H.AttributeValue -> H.Html
stylesheet link = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href link

javascript :: H.AttributeValue -> H.Html
javascript link = H.script ! A.type_ "text/JavaScript" ! A.src link $ ""

linkTo :: H.AttributeValue -> H.Html -> H.Html
linkTo url = H.a ! A.href url

buttonTo :: H.AttributeValue -> H.AttributeValue -> H.Html
buttonTo uri label = H.form ! A.action uri ! A.method "GET" $
    H.input ! A.type_ "submit" ! A.value label

block :: H.AttributeValue -> H.Html -> H.Html
block i = H.div ! A.id i

postForm :: String -> H.Html -> H.Html
postForm uri = H.form ! A.action (H.toValue uri) ! A.method "post"
