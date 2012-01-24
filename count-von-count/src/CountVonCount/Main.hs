module Main
    ( main
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, writeChan)

import CountVonCount.Config
import CountVonCount.Counter
import CountVonCount.Monitor
import CountVonCount.Stream
import Network.WebSockets.PubSub
import qualified CountVonCount.Sensor as Sensor
import qualified CountVonCount.Web as Web

main :: IO ()
main = do
    putStrLn "Count Von Count starting in 1..2..3..."
    config <- readConfigFile "count-von-count.yaml"

    -- Connecting the sensor to the counter
    sensorOut <- newChan
    let sensorHandler time smac bmac = writeChan sensorOut (time, smac, bmac)

    -- Create the pubsub system
    pubSub <- newPubSub

    -- Connecting the counter to whatever (TODO)
    let counterHandler team event = do
            print (team, event)
            publish pubSub $ CounterEvent team event

    _ <- forkIO $ Sensor.listen (configSensorPort config) sensorHandler
    _ <- forkIO $ counter config counterHandler sensorOut
    _ <- forkIO $ monitor config
    Web.listen config pubSub
