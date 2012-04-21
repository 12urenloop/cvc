{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web.Partial
    ( Partial
    , partial
    ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Text.Blaze (ToMarkup (..))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

data Partial = Partial
    { partialSelector :: Text
    , partialHtml     :: Html
    }

instance ToMarkup Partial where
    toMarkup = partialHtml

instance ToJSON Partial where
    toJSON p = object
        [ "selector" .= partialSelector p
        , "html"     .= renderHtml (partialHtml p)
        ]

partial :: Text -> Html -> Partial
partial = Partial
