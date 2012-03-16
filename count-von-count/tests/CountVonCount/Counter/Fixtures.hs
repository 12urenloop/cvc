{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Counter.Fixtures
    ( time
    , circuitLength
    , maxSpeed
    , fixtures
    ) where

import Data.Time (Day (..), UTCTime (..))

import CountVonCount.Counter.Fixtures.Internal
import CountVonCount.Types

time :: UTCTime
time = UTCTime (ModifiedJulianDay 0) 0

circuitLength :: Double
circuitLength = 400

maxSpeed :: Double
maxSpeed = 12

station0 :: Station
station0 = Station "station-0" "0" 10

station1 :: Station
station1 = Station "station-1" "1" 100

station2 :: Station
station2 = Station "station-2" "2" 180

station3 :: Station
station3 = Station "station-3" "3" 320

fixtures :: [(String, CounterFixtureM ())]
fixtures =
    [ ("fixture 01", fixture01)
    , ("fixture 02", fixture02)
    , ("fixture 03", fixture03)
    , ("fixture 04", fixture04)
    , ("fixture 05", fixture05)
    , ("fixture 06", fixture06)
    ]

-- Normal lap
fixture01 :: CounterFixtureM ()
fixture01 = do
    noLap  0 station0
    noLap 10 station1
    noLap 20 station2
    noLap 30 station3 -- This gives an "Impossibru!", why?
    lap   40 station0

-- Multiple detections
fixture02 :: CounterFixtureM ()
fixture02 = do
    noLap  0 station0
    noLap 10 station1
    noLap 11 station1
    noLap 12 station1
    noLap 15 station0

-- Running backwards
fixture03 :: CounterFixtureM ()
fixture03 = do
    noLap  0 station3
    noLap 10 station2
    lap   20 station1
    noLap 30 station0
    noLap 40 station3

-- Only passing 2 points
fixture04 :: CounterFixtureM ()
fixture04 = do
    noLap  0 station3
    lap   10 station0
    noLap 20 station3
    noLap 30 station0

-- Only passing 2 points
fixture05 :: CounterFixtureM ()
fixture05 = do
    noLap  0 station1
    noLap 20 station3
    lap   40 station1
    noLap 60 station3
    lap   80 station1

-- Taking a nap
fixture06 :: CounterFixtureM ()
fixture06 = do
    noLap   0 station0
    noLap  10 station1
    noLap  20 station2
    noLap  30 station3
    lap    40 station0
    noLap  50 station1
    noLap 200 station2
    noLap 210 station3
    lap   220 station0
