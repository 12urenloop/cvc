{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CountVonCount.Web.Views
    ( index
    , management
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
        H.div ! A.id "header" $
            H.div ! A.id "navigation" $ do
                H.a ! A.href "/monitor"    $ "Monitor"
                H.a ! A.href "/management" $ "Management"
                H.a ! A.href "/bonus"      $ "Bonus"

        H.div ! A.id "content" $
            content

        H.div ! A.id "footer" $ ""

index :: Html
index = template "Home" "Hello world"

management :: [(Ref Team, Team)] -> [(Ref Baton, Baton)] -> Html
management teams batons = template "Teams" $ do
    H.div ! A.id "secondary" $ do
        H.h1 "Free batons"
        forM_ batons $ \(_, baton) -> H.div ! A.class_ "baton" $ do
            "Baton "
            H.toHtml $ batonNr baton
            " ("
            H.toHtml $ batonMac baton
            ")"

    H.h1 "Teams"
    forM_ teams $ \(ref, team) -> H.div ! A.class_ "team" $ do
        "("
        H.toHtml $ show ref
        ") "
        H.toHtml $ teamName team
        " laps: "
        H.toHtml $ teamLaps team
