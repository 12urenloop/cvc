module Main
    ( main
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, writeChan)

import CountVonCount.Counter
import CountVonCount.Persistence ()
import CountVonCount.Config
import qualified CountVonCount.Sensor as Sensor
import qualified CountVonCount.Web as Web

main :: IO ()
main = do
    putStrLn "Hello world"

    -- Connecting the sensor to the counter
    sensorOut <- newChan
    let sensorHandler time smac bmac = writeChan sensorOut (time, smac, bmac)

    -- Connecting the counter to whatever (TODO)
    let counterHandler bmac event = putStrLn $
            "Baton " ++ show bmac ++ ": " ++ show event

    config <- readConfigFile "count-von-count.yaml"
    _      <- forkIO $ Sensor.listen (configSensorPort config) sensorHandler
    _      <- forkIO $ counter config counterHandler sensorOut
    Web.listen config
