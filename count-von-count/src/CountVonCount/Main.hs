--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent             (forkIO)
import qualified Data.Aeson                     as A
import qualified Network.WebSockets             as WS
import qualified Network.WebSockets.Util.PubSub as WS


--------------------------------------------------------------------------------
import           CountVonCount.Boxxy
import           CountVonCount.Config
import qualified CountVonCount.Counter          as Counter
import           CountVonCount.EventBase
import qualified CountVonCount.Log              as Log
import qualified CountVonCount.Persistence      as P
import qualified CountVonCount.Sensor           as Sensor
import qualified CountVonCount.Sensor.Filter    as Filter
import qualified CountVonCount.Sensor.Replay    as Replay
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
    Log.string logger "CountVonCount.Main.main" "count-von-count started"

    -- Create the pubsub system
    pubSub <- WS.newPubSub

    -- Start the counter
    counter <- Counter.newCounter
    Counter.subscribe counter (configCircuitLength config)
        (configMaxSpeed config) logger eventBase database

    -- Publish counter events to browser
    -- TODO: Now that we're using EventBase, we should be able to push this into
    -- the Web module.
    subscribe eventBase "WS counter handler" $ \ce -> case ce of
        Counter.PositionEvent team cstate ->
            WS.publish pubSub $ WS.textData $ A.encode $
            Views.counterState (configCircuitLength config) team (Just cstate)
        _ -> return ()

    -- Publish baton watchdog events to browser
    subscribe eventBase "baton handler" $ \deadBatons -> do
        deadBatons' <- mapM (P.getBaton database) deadBatons
        WS.publish pubSub $ WS.textData $ A.encode $
            Views.deadBatons deadBatons'

    -- Initialize boxxy
    boxxies <- newBoxxies config logger database counter eventBase

    -- Connecting raw sensor events to filtered sensor events
    Filter.subscribe eventBase database logger (configRssiThreshold config)

    -- Connecting the sensor to the replay log
    Replay.subscribe eventBase (configReplayLog config)

    -- Start the sensor
    _ <- forkIO $ Sensor.listen logger eventBase (configSensorPort config)

    -- Start the baton watchdog
    _ <- forkIO $ Counter.watchdog counter eventBase
        (configBatonWatchdogInterval config)
        (configBatonWatchdogLifespan config)

    Web.listen config logger eventBase database pubSub counter boxxies

    Log.string logger "CountVonCount.Main.main" "Closing..."
    P.closeDatabase database
    Log.close logger
