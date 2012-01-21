{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web
    ( serve
    ) where

import Control.Applicative ((<|>))
import Control.Monad (liftM2)

import qualified Snap.Blaze as Snap
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Snap.Util.FileServe as Snap

import CountVonCount.Persistence
import qualified CountVonCount.Web.Views as Views

index :: Snap.Snap ()
index = Snap.blaze Views.index

management :: Snap.Snap ()
management = do
    (teams, batons) <- runPersistence $ liftM2 (,) getAll getAll
    Snap.blaze $ Views.management teams batons

site :: Snap.Snap ()
site = Snap.route
    [ ("",            Snap.ifTop index)
    , ("/management", management)
    ] <|> Snap.serveDirectory "static"

serve :: IO ()
serve = Snap.httpServe Snap.defaultConfig site
