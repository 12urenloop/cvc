module Main
    ( main
    ) where

import Control.Concurrent (forkIO)

import CountVonCount.Counter ()
import CountVonCount.Persistence ()
import qualified CountVonCount.Sensor as Sensor
import qualified CountVonCount.Web as Web

main :: IO ()
main = do
    putStrLn "Hello world"
    _ <- forkIO $ Sensor.listen handler
    Web.listen
  where
    handler time station mac =
        putStrLn $ "Got event: " ++ show (time, station, mac)
