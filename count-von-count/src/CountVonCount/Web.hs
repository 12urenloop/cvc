{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web
    ( serve
    ) where

import qualified Snap.Blaze as Snap
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap

import qualified CountVonCount.Web.Views as Views

index :: Snap.Snap ()
index = Snap.blaze Views.index

site :: Snap.Snap ()
site = Snap.route
    [ ("", Snap.ifTop index)
    ]

serve :: IO ()
serve = Snap.httpServe Snap.defaultConfig site
