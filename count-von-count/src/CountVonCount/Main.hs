module Main
    ( main
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, writeChan)

import CountVonCount.Config
import CountVonCount.Counter
import CountVonCount.Feed
import CountVonCount.Monitor
import CountVonCount.Types
import Network.WebSockets.PubSub
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
    pubSub <- newPubSub

    -- Connecting the counter to whatever (TODO)
    let counterHandler team event = do
            print (team, event)
            publish pubSub $ CounterEvent team event

    -- Connecting the sensor to the counter
    sensorChan <- newChan
    let sensorHandler event = do
            Log.raw replayLog $ toReplay event
            writeChan sensorChan event

    -- Initialize the monitoring state
    monitor <- newMonitor config

    _ <- forkIO $ Sensor.listen (configSensorPort config)
        (configStations config) (configBatons config) sensorHandler

    _      <- forkIO $ counter (configCircuitLength config)
        (Log.setModule "Counter" logger) counterHandler sensorChan
    -- _   <- forkIO $ runMonitor monitor
    Web.listen config (Log.setModule "Web" logger) pubSub

    putStrLn "Closing..."
    Log.close replayLog
    Log.close logger
