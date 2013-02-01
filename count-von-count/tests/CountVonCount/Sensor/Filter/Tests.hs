--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Sensor.Filter.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe                           (isNothing)
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.HUnit                           (assert, (@=?))


--------------------------------------------------------------------------------
import           CountVonCount.Persistence.Tests.Util
import           CountVonCount.Sensor
import           CountVonCount.Sensor.Filter


--------------------------------------------------------------------------------
rssiTreshold :: Double
rssiTreshold = -20


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "CountVonCount.Sensor.Tests"
    [ testCase "filterSensorEvent 1" $ testDatabase $ \db -> do
        x <- filterSensorEvent db rssiTreshold
            (RawSensorEvent undefined undefined undefined (-40))
        assert $ isNothing x

    , testCase "filterSensorEvent 2" $ testDatabase $ \db -> do
        x <- filterSensorEvent db rssiTreshold (RawSensorEvent
            undefined "10:00:00:00:00:00" "00:00:00:00:01:00" (-40))
        assert $ isNothing x

    , testCase "filterSensorEvent 3" $ testDatabase $ \db -> do
        x <- filterSensorEvent db rssiTreshold (RawSensorEvent
            undefined "00:00:00:00:00:00" "13:00:00:00:01:00" (-40))
        assert $ isNothing x

    , testCase "filterSensorEvent 4" $ testDatabase $ \db -> do
        Just (SensorEvent _ s b) <- filterSensorEvent db rssiTreshold
            (RawSensorEvent
                undefined "00:00:00:00:00:00" "00:00:00:00:01:00" (-10))
        (s, b) @=? (stations !! 0, batons !! 0)

    , testCase "filterSensorEvent 5" $ testDatabase $ \db -> do
        Just (SensorEvent _ s b) <- filterSensorEvent db rssiTreshold
            (RawSensorEvent
                undefined "00:00:00:00:00:03" "00:00:00:00:01:02" (-10))
        (s, b) @=? (stations !! 3, batons !! 2)
    ]
