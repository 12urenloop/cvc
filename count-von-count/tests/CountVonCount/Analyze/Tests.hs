{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Analyze.Tests
    ( tests
    ) where

import Data.List (mapAccumL)
import Data.Time (Day (..), UTCTime (..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assert)

import Data.ByteString.Char8 ()

import CountVonCount.Analyze

tests :: Test
tests = testGroup "CountVonCount.Analyze.Tests"
    [ testCase "test01" test01
    , testCase "test02" test02
    ]

station0 :: Station
station0 = Station "station0" 10

station1 :: Station
station1 = Station "station1" 100

station2 :: Station
station2 = Station "station2" 180

station3 :: Station
station3 = Station "station3" 320

fromSeconds :: Int -> UTCTime
fromSeconds = UTCTime (ModifiedJulianDay 0) . fromIntegral

sensorEvent :: Int -> Station -> SensorEvent
sensorEvent = SensorEvent . fromSeconds

simulate :: [SensorEvent] -> [AnalyzerEvent]
simulate = concat . snd . mapAccumL step emptyAnalyzerState
  where
    step acc x = let (ys, acc') = stepAnalyzer x acc in (acc', ys)

test01 :: Assertion
test01 = assert $ case simulate events of
    [     Progression _ _ _
        , Progression _ _ _
        , Progression _ _ _
        , Lap         _
        ] -> True
    _     -> False
  where
    events =
        [ sensorEvent  0 station0
        , sensorEvent 10 station1
        , sensorEvent 20 station2
        , sensorEvent 30 station3
        , sensorEvent 40 station0
        ]

test02 :: Assertion
test02 = assert $ case simulate events of
    [Progression _ _ _] -> True
    _                   -> False
  where
    events =
        [ sensorEvent  0 station0
        , sensorEvent 10 station1
        , sensorEvent 11 station1
        , sensorEvent 12 station1
        , sensorEvent 15 station0
        ]
