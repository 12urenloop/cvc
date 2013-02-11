{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web
    ( listen
    ) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (forM, forM_, unless)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Data.Time (getCurrentTimeZone)
import Data.Traversable (traverse)
import Text.Blaze.Html (Html)
import Text.Digestive ((.:))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import qualified Network.WebSockets.Util.PubSub as WS
import qualified Snap.Blaze as Snap
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Snap.Util.FileServe as Snap
import qualified Text.Digestive as DF
import qualified Text.Digestive.Snap as DFS

import CountVonCount.Config
import CountVonCount.Counter
import CountVonCount.Log (Log)
import CountVonCount.Management
import CountVonCount.Persistence
import CountVonCount.Web.Util
import CountVonCount.Boxxy
import qualified CountVonCount.Log as Log
import qualified CountVonCount.Web.Views as Views

data WebEnv = WebEnv
    { webConfig   :: Config
    , webLog      :: Log
    , webDatabase :: Database
    , webPubSub   :: WS.PubSub WS.Hybi00
    , webCounter  :: Counter
    , webBoxxies  :: Boxxies
    }

type Web = ReaderT WebEnv Snap.Snap

index :: Web ()
index = Snap.redirect "/monitor"

monitor :: Web ()
monitor = do
    db         <- webDatabase <$> ask
    counter    <- webCounter <$> ask
    (teams, _) <- liftIO $ assignment db
    states     <- forM teams $ \(team, mbaton) -> liftIO $ case mbaton of
        Nothing -> return (team, Nothing)
        Just b  ->
            counterStateFor (batonId b) counter >>= \s -> return (team, Just s)

    lifespan <- configBatonWatchdogLifespan . webConfig <$> ask
    dead     <- liftIO $ findDeadBatons lifespan counter
    dead'    <- liftIO $ mapM (getBaton db) dead
    clen     <- configCircuitLength . webConfig <$> ask
    Snap.blaze $ Views.monitor clen states dead'

monitorFeed :: Web ()
monitorFeed = do
    pubSub <- webPubSub <$> ask
    Snap.liftSnap $ WS.runWebSocketsSnap $ wsApp pubSub
  where
    wsApp :: WS.PubSub WS.Hybi00 -> WS.Request -> WS.WebSockets WS.Hybi00 ()
    wsApp pubSub req = do
        WS.acceptRequest req
        WS.subscribe pubSub

management :: Web ()
management = do
    db                       <- webDatabase <$> ask
    (withBatons, freeBatons) <- liftIO $ assignment db
    Snap.blaze $ Views.management withBatons freeBatons

laps :: Web ()
laps = do
    db    <- webDatabase <$> ask
    tz    <- liftIO getCurrentTimeZone
    teams <- liftIO (getAllTeams db)
    laps' <- forM teams $ \team -> do
        l <- liftIO $ getLatestLaps db (Just $ teamId team) 5
        return (team, l)
    Snap.blaze $ Views.laps laps' tz

boxxies :: Web ()
boxxies = do
    list <- liftIO . boxxiesToList . webBoxxies =<< ask
    Snap.blaze $ Views.boxxies list

teamForm :: DF.Form Html Web Text
teamForm = "name" .: notNull (DF.text Nothing)
  where
    notNull = DF.check "Can't be empty" $ not . T.null

teamNew :: Web ()
teamNew = do
    db             <- webDatabase <$> ask
    (view, result) <- DFS.runForm "team" teamForm
    case result of
        Just name -> do
            _ <- liftIO $ addTeam db name
            Snap.redirect "/management"
        _         ->
            Snap.blaze $ Views.teamNew view

teamAssign :: Web ()
teamAssign = do
    Just mac <- fmap T.decodeUtf8 <$> Snap.getParam "baton"
    db       <- webDatabase <$> ask
    counter  <- webCounter <$> ask

    unless (T.null mac) $ do
        Just baton   <- liftIO $ getBatonByMac db mac
        Just teamRef <- refFromParam "id"
        liftIO $ assignBaton db counter (batonId baton) teamRef

    Snap.redirect "/management"

data BonusForm = BonusForm Int Text
    deriving (Show)

bonusForm :: Monad m => DF.Form Html m BonusForm
bonusForm = BonusForm
    <$> "laps"   .: DF.stringRead "Can't read number of laps" Nothing
    <*> "reason" .: DF.text Nothing

teamBonus :: Web ()
teamBonus = do
    Just teamRef   <- refFromParam "id"
    db             <- webDatabase <$> ask
    team           <- liftIO $ getTeam db teamRef
    (view, result) <- DFS.runForm "bonus" bonusForm
    case result of
        Just (BonusForm laps' reason) -> do
            logger    <- webLog <$> ask
            boxxies'  <- webBoxxies <$> ask
            liftIO $ addBonus db logger boxxies' teamRef reason laps'
            Snap.redirect "/management"
        _ -> Snap.blaze $ Views.teamBonus team view

teamReset :: Web ()
teamReset = do
    Just teamRef <- refFromParam "id"
    counter      <- webCounter <$> ask
    db           <- webDatabase <$> ask
    logger       <- webLog <$> ask
    team         <- liftIO $ getTeam db teamRef
    case teamBaton team of
        Just bid -> do
            liftIO $ resetCounterFor bid counter
            liftIO $ Log.string logger "CountVonCount.Web.teamReset" $
                "Resetting counter for " ++ show team
        Nothing  -> return ()
    Snap.redirect "/management"

data MultibonusForm = MultibonusForm Int Text [Ref Team] deriving (Show)

multibonusForm :: Monad m => [Team] -> DF.Form Html m MultibonusForm
multibonusForm teams = MultibonusForm
    <$> "laps"   .: DF.stringRead "Can't read number of laps" Nothing
    <*> "reason" .: DF.text Nothing
    <*> fmap checked (traverse checkbox teams)
  where
    checked       = map (teamId . fst) . filter snd
    checkbox team = ((,) team)
        <$> DF.makeRef (refToText $ teamId team) .: DF.bool (Just False)

multibonus :: Web ()
multibonus = do
    db          <- webDatabase <$> ask
    allTeams    <- liftIO $ getAllTeams db
    (view, res) <- DFS.runForm "multibonus" $ multibonusForm allTeams
    case res of
        Just (MultibonusForm laps' reason teams) -> do
            logger    <- webLog <$> ask
            boxxies'  <- webBoxxies <$> ask
            forM_ teams $ \team ->
                liftIO $ addBonus db logger boxxies' team reason laps'
            Snap.redirect "/management"

        _ -> Snap.blaze $ Views.multibonus allTeams view

site :: Web ()
site = Snap.route
    [ ("",                     Snap.ifTop index)
    , ("/monitor",             monitor)
    , ("/monitor/feed",        monitorFeed)
    , ("/management",          management)
    , ("/laps",                laps)
    , ("/boxxies",             boxxies)
    , ("/team/new",            teamNew)
    , ("/team/:id/assign",     teamAssign)
    , ("/team/:id/bonus",      teamBonus)
    , ("/team/:id/reset",      teamReset)
    , ("/multibonus",          multibonus)
    ] <|> Snap.serveDirectory "static"

listen :: Config -> Log -> Database -> WS.PubSub WS.Hybi00 -> Counter -> Boxxies
       -> IO ()
listen conf logger db pubSub counter boxxies' =
    Snap.httpServe snapConfig $ runReaderT site env
  where
    env = WebEnv
        { webConfig   = conf
        , webLog      = logger
        , webDatabase = db
        , webPubSub   = pubSub
        , webCounter  = counter
        , webBoxxies  = boxxies'
        }

    snapConfig = Snap.setPort (configWebPort conf) Snap.defaultConfig
