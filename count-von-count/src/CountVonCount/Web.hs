{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web
    ( listen
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Arrow ((&&&))
import Control.Monad (unless)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Map as M

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Snap.Blaze as Snap
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Snap.Util.FileServe as Snap

import CountVonCount.Config
import CountVonCount.Types
import CountVonCount.Persistence
import CountVonCount.Web.Util
import qualified CountVonCount.Web.Views as Views

type Web = ReaderT Config Snap.Snap

index :: Web ()
index = Snap.blaze Views.index

management :: Web ()
management = do
    batons <- configBatons <$> ask
    teams  <- sortBy (comparing (teamName . snd)) <$> runPersistence getAll
    let batonMap   = M.fromList $ map (batonMac &&& id) batons
        withBatons = flip map teams $ \(ref, team) ->
            (ref, team, teamBaton team >>= flip M.lookup batonMap . BC.pack)
        freeBatons = map snd $ M.toList $ foldl (flip M.delete) batonMap $
            map BC.pack $ catMaybes $ map (teamBaton . snd) teams

    Snap.blaze $ Views.management withBatons freeBatons

assign :: Web ()
assign = do
    Just mac <- Snap.getParam "baton"
    unless (B.null mac) $ do
        Just teamRef <- refFromParam "id"
        runPersistence $ do
            team  <- get teamRef
            put teamRef  $ team {teamBaton = Just (BC.unpack mac)}

    Snap.redirect "/management"

site :: Web ()
site = Snap.route
    [ ("",                 Snap.ifTop index)
    , ("/management",      management)
    , ("/team/:id/assign", assign)
    ] <|> Snap.serveDirectory "static"

listen :: Config -> IO ()
listen = Snap.httpServe Snap.defaultConfig . runReaderT site
