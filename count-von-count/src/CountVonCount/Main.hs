module Main
    ( main
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan)

import CountVonCount.Config
import CountVonCount.Counter
import CountVonCount.Feed
import CountVonCount.Monitor
import Network.WebSockets.PubSub
import qualified CountVonCount.Sensor as Sensor
import qualified CountVonCount.Web as Web

main :: IO ()
main = do
    putStrLn "Count Von Count starting in 1..2..3..."
    config <- readConfigFile "count-von-count.yaml"

    -- Create the pubsub system
    pubSub <- newPubSub

    -- Connecting the counter to whatever (TODO)
    let counterHandler team event = do
            print (team, event)
            publish pubSub $ CounterEvent team event

    -- Connecting the sensor to the counter
    sensorChan <- newChan

    -- Initialize the monitoring state
    monitor <- newMonitor config

    _      <- forkIO $ Sensor.listen config sensorChan
    _      <- forkIO $ counter config counterHandler sensorChan
    -- _   <- forkIO $ runMonitor monitor
    Web.listen config pubSub
