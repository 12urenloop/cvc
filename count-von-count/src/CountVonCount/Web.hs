{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web
    ( listen
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Arrow ((&&&))
import Control.Monad (forM, unless)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (liftIO)
import Data.List (find, sort, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import qualified Data.Map as M

import Data.Time (getCurrentTime)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import qualified Network.WebSockets.Util.PubSub as WS
import qualified Snap.Blaze as Snap
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Snap.Util.FileServe as Snap

import CountVonCount.Config
import CountVonCount.Counter
import CountVonCount.Log (Log)
import CountVonCount.Persistence
import CountVonCount.Types
import CountVonCount.Web.Util
import qualified CountVonCount.Log as Log
import qualified CountVonCount.Web.Views as Views

data WebEnv = WebEnv
    { webConfig  :: Config
    , webLog     :: Log
    , webPubSub  :: WS.PubSub WS.Hybi00
    , webCounter :: Counter
    }

type Web = ReaderT WebEnv Snap.Snap

index :: Web ()
index = Snap.redirect "/monitor"

config :: Web ()
config = do
    conf <- webConfig <$> ask
    Snap.modifyResponse $ Snap.setContentType "application/json"
    Snap.writeLBS $ A.encode conf

monitor :: Web ()
monitor = do
    teams <- sort . map snd <$> runPersistence getAll
    Snap.blaze $ Views.monitor teams

feed :: Web ()
feed = do
    pubSub <- webPubSub <$> ask
    Snap.liftSnap $ WS.runWebSocketsSnap $ wsApp pubSub
  where
    wsApp :: WS.PubSub WS.Hybi00 -> WS.Request -> WS.WebSockets WS.Hybi00 ()
    wsApp pubSub req = do
        WS.acceptRequest req
        WS.subscribe pubSub

management :: Web ()
management = do
    batons <- configBatons . webConfig <$> ask
    teams  <- sortBy (comparing snd) <$> runPersistence getAll
    let batonMap   = M.fromList $ map (batonMac &&& id) batons
        withBatons = flip map teams $ \(ref, team) ->
            (ref, team, teamBaton team >>= flip M.lookup batonMap)
        freeBatons = map snd $ M.toList $ foldl (flip M.delete) batonMap $
            mapMaybe (teamBaton . snd) teams

    Snap.blaze $ Views.management withBatons freeBatons

laps :: Web ()
laps = do
    laps' <- runPersistence $ do
        laps' <- getLaps 0 20
        forM laps' $ \lap -> do
            team <- get $ lapTeam lap
            return (lap, team)

    Snap.blaze $ Views.laps laps'

assign :: Web ()
assign = do
    Just mac <- Snap.getParam "baton"
    unless (B.null mac) $ do
        Just teamRef <- refFromParam "id"
        logger       <- webLog <$> ask
        runPersistence $ do
            team  <- get teamRef
            liftIO $ Log.string logger $
                "assigning " ++ show mac ++ " to " ++ show team
            put teamRef team {teamBaton = Just (T.decodeUtf8 mac)}

    Snap.redirect "/management"

bonus :: Web ()
bonus = do
    Just teamRef <- refFromParam "id"
    mlaps        <- readParam "laps"
    mreason      <- readParam "reason"
    case (mlaps, mreason) of
        -- Success
        (Just laps', Just reason) -> do
            timestamp <- liftIO getCurrentTime
            runPersistence $ addLaps teamRef timestamp reason laps'
            Snap.redirect "/management"
        -- Render form
        _                -> do
            team <- runPersistence $ get teamRef
            Snap.blaze $ Views.bonus teamRef team

reset :: Web ()
reset = do
    Just teamRef <- refFromParam "id"
    counter    <- webCounter <$> ask
    batons     <- configBatons . webConfig <$> ask
    runPersistence $ do
        team   <- get teamRef
        case teamBaton team of
            Just mac -> do
                let Just baton = find (\x -> batonMac x == mac) batons
                liftIO $ resetCounterFor baton counter

            Nothing  -> return ()
    -- TODO render view

site :: Web ()
site = Snap.route
    [ ("",                 Snap.ifTop index)
    , ("/config.json",     config)
    , ("/monitor",         monitor)
    , ("/feed",            feed)
    , ("/management",      management)
    , ("/laps",            laps)
    , ("/team/:id/assign", assign)
    , ("/team/:id/bonus",  bonus)
    , ("/team/:id/reset",  reset)
    ] <|> Snap.serveDirectory "static"

listen :: Config -> Log -> WS.PubSub WS.Hybi00 -> Counter -> IO ()
listen conf logger pubSub counter =
    Snap.httpServe Snap.defaultConfig $ runReaderT site env
  where
    env = WebEnv
        { webConfig  = conf
        , webLog     = logger
        , webPubSub  = pubSub
        , webCounter = counter
        }
