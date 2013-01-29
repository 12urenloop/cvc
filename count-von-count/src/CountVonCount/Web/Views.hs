{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CountVonCount.Web.Views
    ( index
    , monitor
    , management
    , laps
    , boxxies
    , teamNew
    , teamBonus
    , multibonus

      -- * Partials
    , counterState
    , deadBatons
    ) where

import Control.Monad (forM_)
import Data.Time (TimeZone, formatTime, utcToLocalTime)
import Prelude hiding (div)
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

import Text.Blaze.Html (Html, (!))
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Digestive as D
import qualified Text.Digestive.Blaze.Html5 as D

import CountVonCount.Boxxy
import CountVonCount.Counter.Core
import CountVonCount.Persistence
import CountVonCount.Sensor.Filter
import CountVonCount.Types
import CountVonCount.Web.Partial
import CountVonCount.Web.Views.Util

template :: Html -> Html -> Html
template title content = H.docTypeHtml $ do
    H.head $ do
        H.title title
        javascript "/js/jquery-1.7.1.min.js"
        javascript "/js/partial.js"
        stylesheet "/css/screen.css"

    H.body $ do
        block "header" $
            block "navigation" $ do
                linkTo "/monitor"    "Monitor"
                linkTo "/management" "Management"
                linkTo "/laps"       "Laps"
                linkTo "/boxxies"    "Boxxies"

        block "content" content

        block "footer" ""

index :: Html
index = template "Home" "Hello world"

monitor :: Double -> [(Team, Maybe CounterState)] -> [Baton] -> Html
monitor circuitLength teams deadBatons' =
    template "Monitor" $ block "monitor" $ do
        block "secondary" $ block "batons" $ H.toHtml $ deadBatons deadBatons'

        H.h1 "Teams"
        block "teams" $ forM_ teams $
            H.toHtml . uncurry (counterState circuitLength)
        javascript "/js/monitor.js"

management :: [(Team, Maybe Baton)] -> [Baton] -> Html
management teams batons = template "Teams" $ block "management" $ do
    block "secondary" $ do
        H.h1 "Extra"
        buttonTo "/team/new" "Add new team"
        buttonTo "/multibonus" "Multibonus"

        H.h1 "Free batons"
        forM_ batons $ \baton -> H.div ! A.class_ "baton" $
            H.toHtml $ show baton

    H.h1 "Teams"
    forM_ teams $ \(team, assigned) -> H.div ! A.class_ "team" $ do
        let assignUri = "/team/" ++ refToString (teamId team) ++ "/assign"
            bonusUri  = "/team/" ++ refToString (teamId team) ++ "/bonus"
            resetUri  = "/team/" ++ refToString (teamId team) ++ "/reset"

        H.toHtml $ teamName team
        " "
        H.span ! A.class_ "soft" $ do
            "("
            H.toHtml $ teamLaps team
            " laps, "
            maybe "no baton" (H.toHtml . batonName) assigned
            ")"

        H.div ! A.class_ "controls" $ do
            buttonTo (H.toValue bonusUri) "Add bonus"

            postForm assignUri $ do
                H.select ! A.name "baton" $ do
                    H.option ! A.value "" ! A.selected "selected" $
                        "Choose baton..."
                    forM_ batons $ \baton ->
                        H.option ! A.value (macValue baton) $
                            H.toHtml (batonName baton)

                H.input ! A.type_ "submit" ! A.value "Assign"

            postForm resetUri $
                H.input ! A.type_ "submit" ! A.value "Reset counter"
  where
    macValue = H.toValue . batonMac

laps :: [(Team, [Lap])] -> TimeZone -> Html
laps teams tz = template "Laps" $ block "laps" $ do
    H.h1 "Laps"
    forM_ teams $ \(team, laps') -> do
        H.h2 $ H.toHtml $ teamName team
        H.table $ do
            H.tr $ do
                H.th "Timestamp"
                H.th "Count"
                H.th "Reason given"
            forM_ laps' $ \lap -> H.tr $ do
                let lt = utcToLocalTime tz (lapTimestamp lap)
                H.td $ H.toHtml $ formatTime defaultTimeLocale "%H:%M:%S" lt
                H.td $ H.toHtml $ lapCount lap
                H.td $ H.toHtml $ lapReason lap

boxxies :: [(BoxxyConfig, BoxxyState)] -> Html
boxxies boxxies' = template "Boxxies" $ block "boxxies" $ do
    H.h1 "Boxxies"
    H.table $ forM_ boxxies' $ \(b, c) -> H.tr $ do
        H.td $ H.toHtml $ show b
        H.td $ H.toHtml $ show c

teamNew :: D.View Html -> Html
teamNew view = template "Add team" $ do
    H.h1 "Add team"
    D.form view "/team/new" $ do
        D.label     "name" view "Name: " >> H.br
        D.inputText "name" view          >> H.br
        D.errorList "name" view

        D.inputSubmit "Add team"

teamBonus :: Team -> D.View Html -> Html
teamBonus team view = template "Add bonus" $ block "bonus" $ do
    -- TODO: cleanup
    let bonusUri  = "/team/" ++ refToString (teamId team) ++ "/bonus"

    H.h1 "Add bonus"
    H.p $ do
        "Specify bonus for "
        H.toHtml $ teamName team
        ":"
    D.form view (T.pack bonusUri) $ do
        D.childErrorList "" view

        D.label     "laps" view "Laps: "
        D.inputText "laps" view ! A.size "5"
        H.br

        D.label     "reason" view "Because: "
        D.inputText "reason" view ! A.size "30"
        H.br

        D.inputSubmit "Add bonus"

multibonus :: [Team] -> D.View Html -> Html
multibonus teams view = template "Multibonus" $ block "multibonus" $ do
    H.h1 "Multibonus"
    D.form view "/multibonus" $ do
        D.childErrorList "" view

        H.h2 "Specify bonus"

        D.label     "laps" view "Laps: "
        D.inputText "laps" view ! A.size "5"
        H.br

        D.label     "reason" view "Because: "
        D.inputText "reason" view ! A.size "30"
        H.br

        H.h2 "Specify teams"
        forM_ teams $ \team -> do
            let dref = D.makeRef (refToText $ teamId team)
            D.inputCheckbox dref view
            D.label         dref view $ H.toHtml $ teamName team
            H.br

        D.inputSubmit "Add multibonux"

counterState :: Double -> Team -> Maybe CounterState -> Partial
counterState circuitLength team cs = partial selector $ H.div
    ! A.class_ "team"
    ! H.dataAttribute "team-id" (H.toValue $ refToText $ teamId team) $ do
        H.h2 $ H.toHtml $ teamName team
        H.span ! A.class_ "laps" $ H.toHtml $ teamLaps team
        " laps "
        case cs of
            Nothing                         -> "No baton assigned."
            Just NoCounterState             -> "Unitialized."
            Just (CounterState _ e speed _) -> do
                H.span ! A.class_ "speed" $
                    H.toHtml (printf "%.2f" speed :: String)
                " m/s"

                H.div $ do
                    "Last updated "
                    H.span ! A.class_ "last-update" $ "0"
                    "s ago"

                let progress = stationPosition (sensorStation e) / circuitLength
                    style    = printf "width: %.0f%%" (progress * 100) :: String
                H.div ! A.class_ "progress" $
                    H.div ! A.class_ "fill" ! A.style (H.toValue style) $ ""
  where
    selector = T.concat
        ["[data-team-id = \"", refToText (teamId team), "\"]"]

deadBatons :: [Baton] -> Partial
deadBatons [] = partial "#batons" $ H.h1 "Batons OK"
deadBatons bs = partial "#batons" $ do
    H.h1 "Batons down!"
    forM_ bs $ H.div . H.toHtml . show
