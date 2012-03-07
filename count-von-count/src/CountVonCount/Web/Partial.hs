{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web.Partial
    ( Partial
    , partial
    ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Text.Blaze (Html, ToHtml (..))
import Text.Blaze.Renderer.Utf8 (renderHtml)

data Partial = Partial
    { partialSelector :: Text
    , partialHtml     :: Html
    }

instance ToHtml Partial where
    toHtml = partialHtml

instance ToJSON Partial where
    toJSON p = object
        [ "selector" .= partialSelector p
        , "html"     .= renderHtml (partialHtml p)
        ]

partial :: Text -> Html -> Partial
partial = Partial
