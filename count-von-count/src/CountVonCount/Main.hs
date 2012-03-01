module Main
    ( main
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, writeChan)
import Data.Foldable (forM_)

import qualified Data.Aeson as A
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Util.PubSub as WS

import CountVonCount.Boxxy
import CountVonCount.Config
import CountVonCount.Counter
import CountVonCount.Counter.Core
import CountVonCount.Feed
import CountVonCount.Log (Log)
import CountVonCount.Persistence (Team (..), getAll, runPersistence)
import CountVonCount.Sensor
import CountVonCount.Sensor.Filter
import CountVonCount.Util
import qualified CountVonCount.Log as Log
import qualified CountVonCount.Sensor as Sensor
import qualified CountVonCount.Web as Web

main :: IO ()
main = do
    putStrLn "Count Von Count starting in 1..2..3..."
    config    <- readConfigFile "count-von-count.yaml"
    logger    <- Log.setModule "Main" <$> Log.open (configLog config)
    replayLog <- Log.open $ configReplayLog config
    Log.string logger "count-von-count started"

    -- Create the pubsub system
    pubSub <- WS.newPubSub

    -- Initialize boxxy
    isolate logger "Initialize boxxy" $ do
        teams <- map snd <$> runPersistence getAll
        forM_ (configBoxxies config) $ \boxxy -> putConfig boxxy
            (configCircuitLength config) (configStations config) teams

    -- Connecting the sensor to the counter
    sensorChan <- newChan
    let filterSensorEvent' = filterSensorEvent
            (configRssiThreshold config) (configStations config)
            (configBatons config)
        sensorHandler event = do
            Log.raw replayLog $ toReplay event
            forM_ (filterSensorEvent' event) $ writeChan sensorChan

    -- Start the sensor
    _ <- forkIO $ Sensor.listen (Log.setModule "Sensor" logger)
        (configSensorPort config) sensorHandler

    -- Start the counter
    counter <- newCounter
    _       <- forkIO $ runCounter counter (configCircuitLength config)
        (configMaxSpeed config) (Log.setModule "Counter" logger)
        (counterHandler logger (configBoxxies config) pubSub) sensorChan

    Web.listen config (Log.setModule "Web" logger) pubSub counter

    putStrLn "Closing..."
    Log.close replayLog
    Log.close logger

counterHandler :: WS.TextProtocol p
               => Log -> [BoxxyConfig] -> WS.PubSub p -> Team -> CounterEvent
               -> IO ()
counterHandler logger boxxies pubSub team event = do
    -- TODO: remove this
    print (team, event)

    -- Send to websockets pubsub
    publish $ CounterEvent team event

    -- Send to boxxies
    forM_ boxxies $ \boxxy -> isolate logger ("Calling boxxy: " ++ show boxxy) $
        case event of
            Lap _ _                     -> putLaps boxxy team Nothing Nothing
            Progression _ station speed -> putPosition boxxy team station speed
  where
    publish = WS.publish pubSub . WS.textData . A.encode
