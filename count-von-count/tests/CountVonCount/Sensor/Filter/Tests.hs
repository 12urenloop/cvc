{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Sensor.Filter.Tests
    ( tests
    ) where

import Data.Maybe (isNothing)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assert, (@=?))

import CountVonCount.Sensor
import CountVonCount.Sensor.Filter
import CountVonCount.Types

stations :: [Station]
stations =
    [ Station "Pallet Town"   "00:00:00:00:00:00" 0
    , Station "Viridian City" "00:00:00:00:00:01" 100
    , Station "Celadon City"  "00:00:00:00:00:02" 200
    , Station "Fuchsia City"  "00:00:00:00:00:03" 300
    ]

batons :: [Baton]
batons =
    [ Baton "00:00:00:00:01:00" 1
    , Baton "00:00:00:00:01:01" 2
    , Baton "00:00:00:00:01:02" 3
    ]

rssiTreshold :: Double
rssiTreshold = -20

tests :: Test
tests = testGroup "CountVonCount.Sensor.Tests"
    [ testCase "filterSensorEvent 1" $
        assert $ isNothing $ filterSensorEvent'
            (RawSensorEvent undefined undefined undefined (-40))
    , testCase "filterSensorEvent 2" $
        assert $ isNothing $ filterSensorEvent' (RawSensorEvent
            undefined "10:00:00:00:00:00" "00:00:00:00:01:00" (-40))
    , testCase "filterSensorEvent 3" $
        assert $ isNothing $ filterSensorEvent' (RawSensorEvent
            undefined "00:00:00:00:00:00" "13:00:00:00:01:00" (-40))
    , testCase "filterSensorEvent 4" $
        let Just (SensorEvent _ s b) = filterSensorEvent' (RawSensorEvent
                undefined "00:00:00:00:00:00" "00:00:00:00:01:00" (-10))
        in (s, b) @=? (stations !! 0, batons !! 0)
    , testCase "filterSensorEvent 5" $
        let Just (SensorEvent _ s b) = filterSensorEvent' (RawSensorEvent
                undefined "00:00:00:00:00:03" "00:00:00:00:01:02" (-10))
        in (s, b) @=? (stations !! 3, batons !! 2)
    ]
  where
    filterSensorEvent' = filterSensorEvent rssiTreshold stations batons
