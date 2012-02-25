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

import CountVonCount.Config
import CountVonCount.Counter
import CountVonCount.Feed
import CountVonCount.Sensor
import CountVonCount.Sensor.Filter
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
    let publish = WS.publish pubSub . WS.textData . A.encode

    -- Connecting the counter to whatever (TODO)
    let counterHandler team event = do
            print (team, event)
            publish $ CounterEvent team event

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
        counterHandler sensorChan

    Web.listen config (Log.setModule "Web" logger) pubSub counter

    putStrLn "Closing..."
    Log.close replayLog
    Log.close logger
