--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (forM)
import qualified Data.Aeson                     as A
import           Data.Foldable                  (forM_)
import qualified Network.WebSockets             as WS
import qualified Network.WebSockets.Util.PubSub as WS


--------------------------------------------------------------------------------
import           CountVonCount.Boxxy
import           CountVonCount.Config
import           CountVonCount.Counter
import           CountVonCount.Counter.Core
import           CountVonCount.EventBase
import qualified CountVonCount.Log              as Log
import qualified CountVonCount.Persistence      as P
import           CountVonCount.Sensor
import qualified CountVonCount.Sensor           as Sensor
import           CountVonCount.Sensor.Filter
import qualified CountVonCount.Web              as Web
import qualified CountVonCount.Web.Views        as Views


--------------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "Count Von Count starting in 1..2..3..."
    config    <- readConfigFile "count-von-count.yaml"
    logger    <- Log.open (configLog config) True
    eventBase <- newEventBase logger
    database  <- P.newDatabase "count-von-count.db"
    replayLog <- Log.open (configReplayLog config) False
    Log.string logger "CountVonCount.Main.main" "count-von-count started"

    -- Create the pubsub system
    pubSub <- WS.newPubSub

    -- Publish counter events to browser
    subscribe eventBase "WS counter handler" $
        \(team, cstate, _ :: CounterEvent) ->
            WS.publish pubSub $ WS.textData $ A.encode $
            Views.counterState (configCircuitLength config) team (Just cstate)

    -- Publish baton watchdog events to browser
    subscribe eventBase "baton handler" $ \deadBatons -> do
        deadBatons' <- mapM (P.getBaton database) deadBatons
        WS.publish pubSub $ WS.textData $ A.encode $
            Views.deadBatons deadBatons'

    -- Initialize boxxy
    boxxies <- newBoxxies logger eventBase (configBoxxies config) $ \b -> do
        teams <- P.getAllTeams database
        laps  <- P.getLatestLaps database Nothing 10
        laps' <- forM laps $ \lap -> do
            team <- P.getTeam database $ P.lapTeam lap
            return (lap, team)
        putState b teams laps'

    -- Connecting the sensor to the counter
    subscribe eventBase "sensor handler" $ \event -> do
        let filterSensorEvent' = filterSensorEvent database logger
                (configRssiThreshold config)
        Log.raw replayLog $ toReplay event
        filtered <- filterSensorEvent' event
        forM_ filtered $ publish eventBase

    -- Start the sensor
    _ <- forkIO $ Sensor.listen logger eventBase (configSensorPort config)

    -- Start the counter
    counter <- newCounter
    subscribeCounter counter (configCircuitLength config)
        (configMaxSpeed config) logger eventBase database

    -- Start the baton watchdog
    _ <- forkIO $ watchdog counter eventBase
        (configBatonWatchdogInterval config)
        (configBatonWatchdogLifespan config)

    Web.listen config logger database pubSub counter boxxies

    Log.string logger "CountVonCount.Main.main" "Closing..."
    P.closeDatabase database
    Log.close replayLog
    Log.close logger
