{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web
    ( listen
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Monad (forM, unless)
import Data.Foldable (forM_)
import Data.List (sortBy)
import Data.Maybe (isNothing)
import Data.Ord (comparing)

import qualified Data.ByteString as B
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
    (teams, freeBatons) <- runPersistence $ do
        -- Find all teams, find corresponding batons
        teams      <- sortBy (comparing (teamName . snd))  <$> getAll
        withBatons <- forM teams $ \(ref, team) ->
            case teamBaton team of
                Nothing -> return (ref, team, Nothing)
                Just br -> do
                    b <- get br
                    return (ref, team, Just b)

        -- Find free batons
        freeBatons <- filter (isNothing . batonTeam . snd) <$> getAll

        return (withBatons, freeBatons)

    Snap.blaze $ Views.management teams freeBatons

assign :: Snap.Snap ()
assign = do
    Just hasBaton <- Snap.getParam "baton"
    unless (B.null hasBaton) $ do
        Just teamRef  <- refFromParam "id"
        Just batonRef <- refFromParam "baton"
        runPersistence $ do
            team  <- get teamRef
            baton <- get batonRef

            -- Unassign old baton if necessary
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

listen :: IO ()
listen = Snap.httpServe Snap.defaultConfig site
