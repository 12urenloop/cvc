{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web.Views
    ( index
    ) where

import Text.Blaze (Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

template :: Html -> Html
template body = H.docTypeHtml $ do
    H.head $ do
        H.title "count-von-count" 
    H.body $ do
        body 

index :: Html
index = template "Hello world"
