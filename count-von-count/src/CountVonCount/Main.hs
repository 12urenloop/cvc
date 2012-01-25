module Main
    ( main
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, writeChan)

import CountVonCount.Config
import CountVonCount.Counter
import CountVonCount.Feed
import CountVonCount.Log
import CountVonCount.Monitor
import CountVonCount.Types
import Network.WebSockets.PubSub
import qualified CountVonCount.Sensor as Sensor
import qualified CountVonCount.Web as Web

main :: IO ()
main = do
    putStrLn "Count Von Count starting in 1..2..3..."
    config    <- readConfigFile "count-von-count.yaml"
    mainLog   <- openLog $ configLog config
    replayLog <- openLog $ configReplayLog config
    logStr mainLog "Main" "count-von-count started"

    -- Create the pubsub system
    pubSub <- newPubSub

    -- Connecting the counter to whatever (TODO)
    let counterHandler team event = do
            print (team, event)
            publish pubSub $ CounterEvent team event

    -- Connecting the sensor to the counter
    sensorChan <- newChan
    let sensorHandler event = do
            logRaw replayLog $ toReplay event
            writeChan sensorChan event

    -- Initialize the monitoring state
    monitor <- newMonitor config

    _ <- forkIO $ Sensor.listen (configSensorPort config)
        (configStations config) (configBatons config) sensorHandler

    _      <- forkIO $ counter config mainLog counterHandler sensorChan
    -- _   <- forkIO $ runMonitor monitor
    Web.listen config mainLog pubSub

    putStrLn "Closing..."
    closeLog replayLog
    closeLog mainLog
