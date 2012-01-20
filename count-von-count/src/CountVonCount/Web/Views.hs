{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CountVonCount.Web.Views
    ( index
    , teams
    ) where

import Control.Monad (forM_)

import Text.Blaze (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import CountVonCount.Persistence

template :: Html -> Html -> Html
template title content = H.docTypeHtml $ do
    H.head $ do
        H.title title
        H.link ! A.rel "stylesheet" ! A.type_ "text/css"
            ! A.href "/css/screen.css"
    H.body $ do
        H.div ! A.id "navigation" $ do
            H.a ! A.href "/monitor" $ "Monitor"
            H.a ! A.href "/teams"   $ "Teams"
            H.a ! A.href "/bonus"   $ "Bonus"
        H.div ! A.id "content" $
            content

index :: Html
index = template "Home" "Hello world"

teams :: [(Ref Team, Team)] -> Html
teams ts = template "Teams" $
    forM_ ts $ \(ref, team) -> H.div ! A.class_ "team" $ do
        "("
        H.toHtml $ show ref
        ") "
        H.toHtml $ teamName team
        " laps: "
        H.toHtml $ teamLaps team
