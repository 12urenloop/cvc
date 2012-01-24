{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CountVonCount.Web.Views
    ( index
    , monitor
    , management
    ) where

import Control.Monad (forM_)
import Prelude hiding (div)

import Text.Blaze (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import CountVonCount.Persistence
import CountVonCount.Types
import CountVonCount.Web.Helpers

template :: Html -> Html -> Html
template title content = H.docTypeHtml $ do
    H.head $ do
        H.title title
        javascript "/js/jquery-1.7.1.min.js"
        javascript "/js/jquery.flot.min.js"
        stylesheet "/css/screen.css"

    H.body $ do
        div "header" $
            div "navigation" $ do
                linkTo "/monitor"    "Monitor"
                linkTo "/management" "Management"
                linkTo "/bonus"      "Bonus"

        div "content" $ content

        div_ "footer"

index :: Html
index = template "Home" "Hello world"

monitor :: [Team] -> Html
monitor teams = template "Monitor" $ div "monitor" $ do
    H.h1 "Scores"
    forM_ teams $ \team -> divC "team"
            !  H.dataAttribute "team-id" (H.toValue $ teamId team) $ do
        H.h2 $ H.toHtml $ teamName team
        divC "laps" $ H.toHtml $ teamLaps team
        divC "speed" $ ""
    javascript "/js/monitor.js"

management :: [(Ref Team, Team, Maybe Baton)] -> [Baton] -> Html
management teams batons = template "Teams" $ div "management" $ do
    div "secondary" $ do
        H.h1 "Free batons"
        forM_ batons $ \baton -> divC "baton" $ do
            H.toHtml $ batonName baton
            " ("
            H.toHtml $ batonMac baton
            ")"

    H.h1 "Teams"
    forM_ teams $ \(ref, team, assigned) -> divC "team" $ do
        let assignUri = "/team/" ++ refToString ref ++ "/assign"

        H.toHtml $ teamName team

        postForm assignUri $ do
            H.select ! A.name "baton" $ do
                case assigned of
                    Just baton ->
                        H.option ! A.value (macValue baton)
                                 ! A.selected "selected" $
                            H.toHtml (batonName baton)
                    _          ->
                        H.option ! A.value "" ! A.selected "selected" $ ""

                forM_ batons $ \baton ->
                    H.option ! A.value (macValue baton) $
                        H.toHtml (batonName baton)

            H.input ! A.type_ "submit" ! A.value "Assign"
  where
    macValue = H.toValue . batonMac
