--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Counter.Fixtures
    ( time
    , circuitLength
    , maxSpeed
    , fixtures
    ) where


--------------------------------------------------------------------------------
import           Data.Time                               (Day (..),
                                                          UTCTime (..))


--------------------------------------------------------------------------------
import           CountVonCount.Counter.Fixtures.Internal
import           CountVonCount.Persistence.Tests.Util


--------------------------------------------------------------------------------
time :: UTCTime
time = UTCTime (ModifiedJulianDay 0) 0


--------------------------------------------------------------------------------
circuitLength :: Double
circuitLength = 400


--------------------------------------------------------------------------------
maxSpeed :: Double
maxSpeed = 12


--------------------------------------------------------------------------------
fixtures :: [(String, CounterFixtureM ())]
fixtures =
    [ ("fixture 01", fixture01)
    , ("fixture 02", fixture02)
    , ("fixture 03", fixture03)
    , ("fixture 04", fixture04)
    , ("fixture 05", fixture05)
    , ("fixture 06", fixture06)
    , ("fixture 07", fixture07)
    ]


--------------------------------------------------------------------------------
-- Normal lap
fixture01 :: CounterFixtureM ()
fixture01 = do
    noLap  0 (testStations !! 0)
    noLap 20 (testStations !! 1)
    noLap 40 (testStations !! 2)
    noLap 60 (testStations !! 3)
    lap   80 (testStations !! 0)


--------------------------------------------------------------------------------
-- Multiple detections
fixture02 :: CounterFixtureM ()
fixture02 = do
    noLap  0 (testStations !! 0)
    noLap 10 (testStations !! 1)
    noLap 11 (testStations !! 1)
    noLap 12 (testStations !! 1)
    noLap 15 (testStations !! 0)


--------------------------------------------------------------------------------
-- Running backwards
fixture03 :: CounterFixtureM ()
fixture03 = do
    noLap  0 (testStations !! 3)
    noLap 10 (testStations !! 2)
    lap   20 (testStations !! 1)
    noLap 30 (testStations !! 0)
    noLap 40 (testStations !! 3)


--------------------------------------------------------------------------------
-- Only passing 2 points
fixture04 :: CounterFixtureM ()
fixture04 = do
    noLap  0 (testStations !! 3)
    lap   10 (testStations !! 0)
    noLap 20 (testStations !! 3)
    noLap 30 (testStations !! 0)


--------------------------------------------------------------------------------
-- Only passing 2 points
fixture05 :: CounterFixtureM ()
fixture05 = do
    noLap  0 (testStations !! 1)
    noLap 20 (testStations !! 3)
    lap   40 (testStations !! 1)
    noLap 60 (testStations !! 3)
    lap   80 (testStations !! 1)


--------------------------------------------------------------------------------
-- Taking a nap
fixture06 :: CounterFixtureM ()
fixture06 = do
    noLap   0 (testStations !! 0)
    noLap  15 (testStations !! 1)
    noLap  30 (testStations !! 2)
    noLap  45 (testStations !! 3)
    lap    60 (testStations !! 0)
    noLap  75 (testStations !! 1)
    noLap 300 (testStations !! 2)
    noLap 315 (testStations !! 3)
    lap   330 (testStations !! 0)


--------------------------------------------------------------------------------
-- Riding a fast unicorn
fixture07 :: CounterFixtureM ()
fixture07 = do
    noLap   0 (testStations !! 0)
    noLap   2 (testStations !! 1)
    noLap   4 (testStations !! 2)
    noLap   6 (testStations !! 3)
    noLap   8 (testStations !! 0)
    noLap  10 (testStations !! 1)
    noLap  12 (testStations !! 2)
    noLap  14 (testStations !! 3)
    noLap  16 (testStations !! 0)
    noLap  18 (testStations !! 0)
    noLap  20 (testStations !! 0)
