module Main
    ( main
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan)

import CountVonCount.Config
import CountVonCount.Counter
import CountVonCount.Monitor
import CountVonCount.Persistence ()
import Network.WebSockets.PubSub
import qualified CountVonCount.Sensor as Sensor
import qualified CountVonCount.Web as Web

main :: IO ()
main = do
    putStrLn "Hello world"

    -- Create the pubsub system
    pubSub <- newPubSub

    -- Connecting the counter to whatever (TODO)
    let counterHandler team event = do
            print (team, event)
            publish pubSub $ CounterEvent team event

    -- Connecting the sensor to the counter
    sensorChan <- newChan

    config <- readConfigFile "count-von-count.yaml"
    _      <- forkIO $ Sensor.listen config sensorChan
    _      <- forkIO $ counter config counterHandler sensorChan
    Web.listen config pubSub
