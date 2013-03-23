--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Sensor.Filter.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@=?))


--------------------------------------------------------------------------------
import           CountVonCount.Persistence
import           CountVonCount.Sensor
import           CountVonCount.Sensor.Filter
import           CountVonCount.TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "CountVonCount.Sensor.Tests"
    [ testCase "filterSensorEvent 1" $ do
        x <- fse $ RawSensorEvent undefined undefined undefined (-40)
        Nothing @=? x

    , testCase "filterSensorEvent 2" $ do
        x <- fse $ RawSensorEvent
                undefined "00:00:00:00:00:03" "00:00:00:00:01:01" (-10)
        Nothing @=? x

    , testCase "filterSensorEvent 3" $ do
        Just (SensorEvent _ s b _ _) <- fse $ RawSensorEvent
            undefined "00:00:00:00:00:00" "00:00:00:00:01:00" (-10)
        (s, b) @=? (testStations !! 0, testBatons !! 0)

    , testCase "filterSensorEvent 4" $ do
        Just (SensorEvent _ s b _ _) <- fse $ RawSensorEvent
                undefined "00:00:00:00:00:03" "00:00:00:00:01:00" (-10)
        (s, b) @=? (testStations !! 3, testBatons !! 0)
    ]
  where
    rssiTreshold = -20
    fse re       = testDatabase $ \db -> testLog $ \l -> do
        teamRef <- addTeam db "wina"
        setTeamBaton db teamRef (Just $ batonId $ testBatons !! 0)
        filterSensorEvent db l rssiTreshold re
