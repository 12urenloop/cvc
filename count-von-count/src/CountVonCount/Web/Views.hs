{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CountVonCount.Web.Views
    ( index
    , monitor
    , management
    ) where

import Control.Monad (forM_)

import Text.Blaze (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import CountVonCount.Persistence
import CountVonCount.Types

template :: Html -> Html -> Html
template title content = H.docTypeHtml $ do
    H.head $ do
        H.title title
        H.script ! A.type_ "text/JavaScript"
            ! A.src "http://code.jquery.com/jquery-1.6.3.min.js" $ ""
        H.script ! A.type_ "text/JavaScript"
            ! A.src "/js/jquery.flot.min.js" $ ""
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

monitor :: [Team] -> Html
monitor teams = template "Monitor" $ H.div ! A.id "monitor" $ do
    H.h1 "Scores"
    forM_ teams $ \team -> H.div ! A.class_ "team"
            !  H.dataAttribute "team-id" (H.toValue $ teamId team) $ do
        H.h2 $ H.toHtml $ teamName team
        H.div ! A.class_ "laps" $ H.toHtml $ teamLaps team
        H.div ! A.class_ "speed" $ ""
    H.script ! A.type_ "text/JavaScript" ! A.src "/js/monitor.js" $ ""

management :: [(Ref Team, Team, Maybe Baton)] -> [Baton] -> Html
management teams batons = template "Teams" $ H.div ! A.id "management" $ do
    H.div ! A.id "secondary" $ do
        H.h1 "Free batons"
        forM_ batons $ \baton -> H.div ! A.class_ "baton" $ do
            H.toHtml $ batonName baton
            " ("
            H.toHtml $ batonMac baton
            ")"

    H.h1 "Teams"
    forM_ teams $ \(ref, team, assigned) -> H.div ! A.class_ "team" $ do
        let assignUri = "/team/" ++ refToString ref ++ "/assign"

        H.toHtml $ teamName team

        H.form ! A.action (H.toValue assignUri) ! A.method "post" $ do
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
