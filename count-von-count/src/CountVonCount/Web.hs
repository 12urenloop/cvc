{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web
    ( serve
    ) where

import Control.Applicative ((<$>), (<|>))
import Data.Foldable (forM_)
import Data.List (sortBy)
import Data.Maybe (isNothing)
import Data.Ord (comparing)

import qualified Snap.Blaze as Snap
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Snap.Util.FileServe as Snap

import CountVonCount.Persistence
import CountVonCount.Web.Util
import qualified CountVonCount.Web.Views as Views

index :: Snap.Snap ()
index = Snap.blaze Views.index

management :: Snap.Snap ()
management = do
    (teams, batons) <- runPersistence $ do
        teams  <- sortBy (comparing (teamName . snd))  <$> getAll
        batons <- filter (isNothing . batonTeam . snd) <$> getAll
        return (teams, batons)
    Snap.blaze $ Views.management teams batons

assign :: Snap.Snap ()
assign = do
    teamRef  <- refFromParam "id"
    batonRef <- refFromParam "baton"
    runPersistence $ do
        team  <- get teamRef
        baton <- get batonRef

        -- Unassign old baton
        forM_ (teamBaton team) $ \oldBatonRef -> do
            oldBaton <- get oldBatonRef
            put oldBatonRef $ oldBaton {batonTeam = Nothing}
            
        put teamRef  $ team {teamBaton = Just batonRef}
        put batonRef $ baton {batonTeam = Just teamRef}
    Snap.redirect "/management"

site :: Snap.Snap ()
site = Snap.route
    [ ("",                 Snap.ifTop index)
    , ("/management",      management)
    , ("/team/:id/assign", assign)
    ] <|> Snap.serveDirectory "static"

serve :: IO ()
serve = Snap.httpServe Snap.defaultConfig site
