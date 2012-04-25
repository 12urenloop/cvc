{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web
    ( listen
    ) where

import Control.Applicative (pure, (<$>), (<*>), (<|>))
import Control.Monad (forM, unless)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (liftIO)
import Data.Char (isDigit, isLower)
import Data.List (sort)

import Data.Text (Text)
import Data.Time (getCurrentTime, getCurrentTimeZone)
import Text.Blaze.Html (Html)
import Text.Digestive (Form, check, checkM, stringRead, text, (.:))
import Text.Digestive.Snap (runForm)
import qualified Data.Text as T
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
import CountVonCount.Management
import CountVonCount.Persistence
import CountVonCount.Web.Util
import CountVonCount.Boxxy
import qualified CountVonCount.Log as Log
import qualified CountVonCount.Web.Views as Views

data WebEnv = WebEnv
    { webConfig  :: Config
    , webLog     :: Log
    , webPubSub  :: WS.PubSub WS.Hybi00
    , webCounter :: Counter
    , webBoxxies :: Boxxies
    }

type Web = ReaderT WebEnv Snap.Snap

index :: Web ()
index = Snap.redirect "/monitor"

config :: Web ()
config = ask >>= json . webConfig

monitor :: Web ()
monitor = do
    teams    <- sort . map snd <$> runPersistence getAllTeams
    counter  <- webCounter <$> ask
    batons   <- configBatons . webConfig <$> ask
    states   <- forM teams $ \team -> liftIO $
        case teamBaton team >>= flip findBaton batons of
            Nothing -> return (team, Nothing)
            Just b  -> couterStateFor b counter >>= \s -> return (team, Just s)
    lifespan <- configBatonWatchdogLifespan . webConfig <$> ask
    dead     <- liftIO $ findDeadBatons lifespan counter
    clen     <- configCircuitLength . webConfig <$> ask
    Snap.blaze $ Views.monitor clen states dead

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
    batons                   <- configBatons . webConfig <$> ask
    (withBatons, freeBatons) <- liftIO $ assignment batons
    Snap.blaze $ Views.management withBatons freeBatons

laps :: Web ()
laps = do
    tz    <- liftIO getCurrentTimeZone
    laps' <- runPersistence $ do
        teams <- getAllTeams
        forM teams $ \(r, t) -> do
            l <- getLatestLaps r 5
            return (t, l)
    Snap.blaze $ Views.laps laps' tz

boxxies :: Web ()
boxxies = do
    list <- liftIO . boxxiesToList . webBoxxies =<< ask
    Snap.blaze $ Views.boxxies list

teamForm :: Form Html Web Team
teamForm = Team
    <$> "id"    .: (uniqueId . validId . notNull $ text Nothing)
    <*> "name"  .: notNull (text Nothing)
    <*> "laps"  .: pure 0
    <*> "baton" .: pure Nothing
  where
    notNull = check "Can't be empty" $ not . T.null
    validId = check "Should only contain lowercase letters, numbers or -" $
        T.all $ \c -> isLower c || isDigit c || c == '-'
    uniqueId = checkM "Should be unique" $ \id' -> do
        teams <- map snd <$> runPersistence getAllTeams
        return $ not $ any ((== id') . teamId) teams

teamNew :: Web ()
teamNew = do
    (view, result) <- runForm "team" teamForm
    case result of
        Just team -> do
            _ <- liftIO $ runPersistence $ addTeam team
            Snap.redirect "/management"
        _         ->
            Snap.blaze $ Views.teamNew view

teamAssign :: Web ()
teamAssign = do
    Just mac <- fmap T.decodeUtf8 <$> Snap.getParam "baton"
    counter  <- webCounter <$> ask
    batons   <- configBatons . webConfig <$> ask

    unless (T.null mac) $ do
        let Just baton = findBaton mac batons
        Just teamRef <- refFromParam "id"
        liftIO $ assignBaton counter batons baton teamRef

    Snap.redirect "/management"

data BonusForm = BonusForm Int Text
    deriving (Show)

bonusForm :: Monad m => Form Html m BonusForm
bonusForm = BonusForm
    <$> "laps"   .: stringRead "Can't read number of laps" Nothing
    <*> "reason" .: text Nothing

teamBonus :: Web ()
teamBonus = do
    Just teamRef   <- refFromParam "id"
    team           <- runPersistence $ getTeam teamRef
    (view, result) <- runForm "bonus" bonusForm
    case result of
        Just (BonusForm laps' reason) -> do
            boxxies'  <- webBoxxies <$> ask
            logger    <- webLog <$> ask
            timestamp <- liftIO getCurrentTime
            team'     <- runPersistence $ addLaps teamRef timestamp reason laps'
            liftIO $ withBoxxies logger boxxies' $ \b ->
                putLaps b team' timestamp laps' Nothing (Just reason)

            Snap.redirect "/management"
        _ -> Snap.blaze $ Views.teamBonus teamRef team view

teamReset :: Web ()
teamReset = do
    Just teamRef <- refFromParam "id"
    counter      <- webCounter <$> ask
    logger       <- webLog <$> ask
    batons       <- configBatons . webConfig <$> ask
    runPersistence $ do
        team <- getTeam teamRef
        case teamBaton team of
            Just mac -> do
                let Just baton = findBaton mac batons
                liftIO $ resetCounterFor baton counter
                liftIO $ Log.string logger $
                    "Resetting counter for " ++ show team
            Nothing  -> return ()
    Snap.redirect "/management"

site :: Web ()
site = Snap.route
    [ ("",                     Snap.ifTop index)
    , ("/config.json",         config)
    , ("/monitor",             monitor)
    , ("/monitor/feed",        monitorFeed)
    , ("/management",          management)
    , ("/laps",                laps)
    , ("/boxxies",             boxxies)
    , ("/team/new",            teamNew)
    , ("/team/:id/assign",     teamAssign)
    , ("/team/:id/bonus",      teamBonus)
    , ("/team/:id/reset",      teamReset)
    ] <|> Snap.serveDirectory "static"

listen :: Config -> Log -> WS.PubSub WS.Hybi00 -> Counter -> Boxxies -> IO ()
listen conf logger pubSub counter boxxies' =
    Snap.httpServe snapConfig $ runReaderT site env
  where
    env = WebEnv
        { webConfig  = conf
        , webLog     = logger
        , webPubSub  = pubSub
        , webCounter = counter
        , webBoxxies = boxxies'
        }

    snapConfig = Snap.setPort (configWebPort conf) Snap.defaultConfig
