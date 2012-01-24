module Main
    ( main
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, writeChan)

import CountVonCount.Config
import CountVonCount.Counter
import CountVonCount.Stream
import CountVonCount.Persistence ()
import Network.WebSockets.PubSub
import qualified CountVonCount.Sensor as Sensor
import qualified CountVonCount.Web as Web

main :: IO ()
main = do
    putStrLn "Hello world"

    -- Connecting the sensor to the counter
    sensorOut <- newChan
    let sensorHandler time smac bmac = writeChan sensorOut (time, smac, bmac)

    -- Create the pubsub system
    pubSub <- newPubSub

    -- Connecting the counter to whatever (TODO)
    let counterHandler team event = do
            print (team, event)
            publish pubSub $ CounterEvent team event

    config <- readConfigFile "count-von-count.yaml"
    _      <- forkIO $ Sensor.listen (configSensorPort config) sensorHandler
    _      <- forkIO $ counter config counterHandler sensorOut
    Web.listen config pubSub
