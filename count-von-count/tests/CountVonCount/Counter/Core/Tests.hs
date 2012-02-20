{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Counter.Core.Tests
    ( tests
    ) where

import Data.List (mapAccumL)
import Data.Time (Day (..), UTCTime (..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import Data.ByteString.Char8 ()

import CountVonCount.Counter.Core
import CountVonCount.Sensor.Filter
import CountVonCount.Types

tests :: Test
tests = testGroup "CountVonCount.Counter.Core.Tests"
    [ testCase "test01" test01
    , testCase "test02" test02
    ]

station0 :: Station
station0 = Station "station-0" "0" 10

station1 :: Station
station1 = Station "station-1" "1" 100

station2 :: Station
station2 = Station "station-2" "2" 180

station3 :: Station
station3 = Station "station-3" "3" 320

fromSeconds :: Int -> UTCTime
fromSeconds = UTCTime (ModifiedJulianDay 0) . fromIntegral

sensorEvent :: Int -> Station -> SensorEvent
sensorEvent time station = SensorEvent (fromSeconds time) station $
    error "Baton is irrelevant"

simulate :: [SensorEvent] -> [CounterEvent]
simulate = concat . snd . mapAccumL step emptyCounterState
  where
    stepCounter' = stepCounterState 400
    step acc x   = let (ys, acc') = stepCounter' x acc in (acc', ys)

numLaps :: [CounterEvent] -> Int
numLaps = length . filter isLap

test01 :: Assertion
test01 = 1 @=? numLaps (simulate events)
  where
    events =
        [ sensorEvent  0 station0
        , sensorEvent 10 station1
        , sensorEvent 20 station2
        , sensorEvent 30 station3
        , sensorEvent 40 station0
        ]

test02 :: Assertion
test02 = 0 @=? numLaps (simulate events)
  where
    events =
        [ sensorEvent  0 station0
        , sensorEvent 10 station1
        , sensorEvent 11 station1
        , sensorEvent 12 station1
        , sensorEvent 15 station0
        ]
