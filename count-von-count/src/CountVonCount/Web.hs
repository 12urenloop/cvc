{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web
    ( serve
    ) where

import Control.Applicative ((<|>))

import qualified Snap.Blaze as Snap
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Snap.Util.FileServe as Snap

import CountVonCount.Persistence
import qualified CountVonCount.Web.Views as Views

index :: Snap.Snap ()
index = Snap.blaze Views.index

teams :: Snap.Snap ()
teams = do
    ts <- runPersistence getAll
    Snap.blaze $ Views.teams ts

site :: Snap.Snap ()
site = Snap.route
    [ ("",       Snap.ifTop index)
    , ("/teams", teams)
    ] <|> Snap.serveDirectory "static"

serve :: IO ()
serve = Snap.httpServe Snap.defaultConfig site
