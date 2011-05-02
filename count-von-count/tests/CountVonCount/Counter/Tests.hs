{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Counter.Tests
    ( tests
    ) where

import Control.Monad.State (runState)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import Debug.Trace (traceShow)

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, State)

import CountVonCount.Types
import CountVonCount.Counter
import CountVonCount.Configuration

-- | Number of laps, suspicious laps and refused laps
--
data Laps = Laps Int Int Int
          deriving (Show, Eq)

instance Monoid Laps where
    mempty = Laps 0 0 0
    Laps x1 y1 z1 `mappend` Laps x2 y2 z2 = Laps (x1 + x2) (y1 + y2) (z1 + z2)

-- | Test a counter
--
testCounter :: String           -- ^ Test name
            -> Laps             -- ^ Expected laps
            -> [Measurement]    -- ^ Measurements
            -> Test             -- ^ Resulting test
testCounter name expected measurements = testCase name $ do
    Just config <- loadConfigurationFromFile "config.yaml"
    let env = CounterEnvironment config "00:00:00:00:00:00"
        (reports, _) = foldl step ([], emptyCounterState) measurements
        step (rs, s) m =
            let (r, s') = runState (runReaderT (counter m) env) s
            in (r : rs, s')
        laps = mconcat $ map toLaps $ catMaybes reports
        
    expected @=? laps
  where
    toLaps report = traceShow (reportScore report) $ case reportScore report of
        Good      -> Laps 1 0 0
        Warning _ -> Laps 0 1 0
        Refused _ -> Laps 0 0 1

-- | Actual tests
--
tests :: [Test]
tests =
    [ testCounter "simple example" (Laps 2 0 0)
        [ (  0,   0)
        , ( 20, 100)
        , ( 30, 200)
        , ( 40, 300)
        , ( 50,   0)
        , ( 60, 100)
        , ( 70, 200)
        , ( 80, 300)
        , ( 90,   0)
        ]

    , testCounter "suspicous lap" (Laps 0 1 1)
        [ ( 0,   0)
        , (10, 300)
        , (20, 200)
        , (10, 100)
        ]

    , testCounter "little results" (Laps 1 0 0)
        [ ( 0, 1)
        , (20, 3)
        , (40, 1)
        ]
    ]
