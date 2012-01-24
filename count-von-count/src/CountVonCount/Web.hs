{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web
    ( listen
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Arrow ((&&&))
import Control.Monad (unless)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.List (sort, sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Map as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import qualified Snap.Blaze as Snap
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Snap.Util.FileServe as Snap

import CountVonCount.Config
import CountVonCount.Types
import CountVonCount.Persistence
import CountVonCount.Web.Util
import qualified CountVonCount.Web.Views as Views
import Network.WebSockets.PubSub

data WebEnv = WebEnv
    { webConfig :: Config
    , webPubSub :: PubSub
    }

type Web = ReaderT WebEnv Snap.Snap

index :: Web ()
index = Snap.blaze Views.index

config :: Web ()
config = do
    conf <- webConfig <$> ask
    Snap.modifyResponse $ Snap.setContentType "application/json"
    Snap.writeLBS $ A.encode conf

monitor :: Web ()
monitor = do
    teams <- sort . map snd <$> runPersistence getAll
    Snap.blaze $ Views.monitor teams

monitorSubscribe :: Web ()
monitorSubscribe = do
    pubSub <- webPubSub <$> ask
    Snap.liftSnap $ WS.runWebSocketsSnap $ wsApp pubSub
  where
    wsApp :: PubSub -> WS.Request -> WS.WebSockets WS.Hybi00 ()
    wsApp pubSub req = do
        WS.acceptRequest req
        subscribe pubSub

management :: Web ()
management = do
    batons <- configBatons . webConfig <$> ask
    teams  <- sortBy (comparing snd) <$> runPersistence getAll
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
    [ ("",                   Snap.ifTop index)
    , ("/config.json",       config)
    , ("/monitor",           monitor)
    , ("/monitor/subscribe", monitorSubscribe)
    , ("/management",        management)
    , ("/team/:id/assign",   assign)
    ] <|> Snap.serveDirectory "static"

listen :: Config -> PubSub -> IO ()
listen conf pubSub = Snap.httpServe Snap.defaultConfig $ runReaderT site env
  where
    env = WebEnv
        { webConfig = conf
        , webPubSub = pubSub
        }
