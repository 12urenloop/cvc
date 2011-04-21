{-# LANGUAGE OverloadedStrings #-}
module Views
    ( overview
    ) where

import Control.Monad (forM_)
import qualified Data.Map as M

import Text.Blaze (Html, toHtml, unsafeByteString)
import qualified Text.Blaze.Html5 as H

import Types

overview :: Count -> Html
overview count = H.table $ do
    H.tr $ do
        H.th "mac"
        H.th "score"
    forM_ (M.toList count) $ \(mac, score) -> H.tr $ do
        H.td $ unsafeByteString mac
        H.td $ toHtml score
