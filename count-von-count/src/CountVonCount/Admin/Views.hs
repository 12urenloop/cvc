{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CountVonCount.Admin.Views
    ( root
    , mac
    ) where

import Data.Monoid (mappend)
import Control.Monad (forM_)
import qualified Data.Map as M

import Text.Blaze (Html, toHtml, unsafeByteString, unsafeByteStringValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import CountVonCount.Dispatcher
import CountVonCount.Types
import CountVonCount.Counter

root :: DispatcherState -> Html
root state = do
    H.h1 "Admin interface"
    H.ul $ forM_ (M.toList state) $ \(mac', _) -> H.li $ do
        H.a ! A.href ("/" `mappend` unsafeByteStringValue mac')
            $ unsafeByteString mac'

mac :: Mac -> CounterState -> Html
mac mac' state = do
    H.h1 $ unsafeByteString mac'
    H.p $ H.a ! A.href "/" $ "Back to overview"
    H.form ! A.action reset ! A.method "POST" $ do
        H.input ! A.type_ "submit" ! A.value "Reset"
        H.input ! A.type_ "checkbox" ! A.name "sure" ! A.id "sure"
        H.label ! A.for "sure" $ "I am sure"
    H.table $ do
        H.tr $ do
            H.td "Time"
            H.td "Position"
        forM_ (zip times positions) $ \(time, position) ->
            H.tr ! A.style "text-align: right"
                 $ do
                H.td $ toHtml time
                H.td $ toHtml position
  where
    DataSet times positions = counterDataSet state
    reset = "/" `mappend` unsafeByteStringValue mac' `mappend` "/reset"
