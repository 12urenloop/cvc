{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CountVonCount.Web.Views
    ( index
    , monitor
    , management
    , bonus
    ) where

import Control.Monad (forM_)
import Prelude hiding (div)

import Text.Blaze (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import CountVonCount.Persistence
import CountVonCount.Types
import CountVonCount.Web.Views.Util

template :: Html -> Html -> Html
template title content = H.docTypeHtml $ do
    H.head $ do
        H.title title
        javascript "/js/jquery-1.7.1.min.js"
        javascript "/js/jquery.flot.min.js"
        stylesheet "/css/screen.css"

    H.body $ do
        block "header" $
            block "navigation" $ do
                linkTo "/monitor"    "Monitor"
                linkTo "/management" "Management"

        block "content" content

        block "footer" ""

index :: Html
index = template "Home" "Hello world"

monitor :: [Team] -> Html
monitor teams = template "Monitor" $ block "monitor" $ do
    H.h1 "Scores"
    forM_ teams $ \team -> H.div
            ! A.class_ "team"
            ! H.dataAttribute "team-id" (H.toValue $ teamId team) $ do
        H.h2 $ H.toHtml $ teamName team
        H.div ! A.class_ "laps" $ H.toHtml $ teamLaps team
        H.div ! A.class_ "speed" $ ""
    javascript "/js/monitor.js"

management :: [(Ref Team, Team, Maybe Baton)] -> [Baton] -> Html
management teams batons = template "Teams" $ block "management" $ do
    block "secondary" $ do
        H.h1 "Free batons"
        forM_ batons $ \baton -> H.div ! A.class_ "baton" $ do
            H.toHtml $ batonName baton
            " ("
            H.toHtml $ batonMac baton
            ")"

    H.h1 "Teams"
    forM_ teams $ \(ref, team, assigned) -> H.div ! A.class_ "team" $ do
        let assignUri = "/team/" ++ refToString ref ++ "/assign"
            bonusUri  = "/team/" ++ refToString ref ++ "/bonus"

        H.toHtml $ teamName team
        " "
        H.span ! A.class_ "soft" $ do
            "("
            maybe "no baton" (H.toHtml . batonName) assigned
            ")"

        H.div ! A.class_ "controls" $ do
            postForm bonusUri $
                H.input ! A.type_ "submit" ! A.value "Add bonus"

            postForm assignUri $ do
                H.select ! A.name "baton" $ do
                    H.option ! A.value "" ! A.selected "selected" $
                        "Choose baton..."
                    forM_ batons $ \baton ->
                        H.option ! A.value (macValue baton) $
                            H.toHtml (batonName baton)

                H.input ! A.type_ "submit" ! A.value "Assign"
  where
    macValue = H.toValue . batonMac

bonus :: Ref Team -> Team -> Html
bonus ref team = template "Add bonus" $ block "bonus" $ do
    let bonusUri  = "/team/" ++ refToString ref ++ "/bonus"  -- TODO: cleanup

    H.h1 "Add bonus"
    H.p $ do
        "Specify bonus for "
        H.toHtml $ teamName team
        ":"
    postForm bonusUri $ do
        H.label ! A.for "laps" $ "Laps:"
        H.input ! A.name "laps" ! A.id "laps"
            ! A.type_ "text" ! A.size "5" ! A.value "1"
        H.br
        H.label ! A.for "reason" $ "Because:"
        H.input ! A.name "reason" ! A.id "reason"
            ! A.type_ "text" ! A.size "30" ! A.value ""
        H.br
        H.input ! A.type_ "submit" ! A.value "Add bonus"
