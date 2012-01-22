module Main
    ( main
    ) where

import Control.Concurrent (forkIO)

import CountVonCount.Counter ()
import CountVonCount.Persistence ()
import CountVonCount.Config
import qualified CountVonCount.Sensor as Sensor
import qualified CountVonCount.Web as Web

main :: IO ()
main = do
    putStrLn "Hello world"
    config <- readConfigFile "count-von-count.yaml"
    _      <- forkIO $ Sensor.listen handler
    Web.listen config
  where
    handler time station mac =
        putStrLn $ "Got event: " ++ show (time, station, mac)
