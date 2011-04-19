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
        (reports, _) = foldr step ([], emptyCounterState) measurements
        step m (rs, s) =
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
    [ testCounter "simple example" (Laps 3 0 0)
        [ ( 10, 1)
        , ( 20, 2)
        , ( 30, 3)
        , ( 40, 1)
        , ( 50, 2)
        , ( 60, 3)
        , ( 70, 1)
        , ( 80, 2)
        , ( 90, 3)
        , (100, 1)
        ]

    , testCounter "suspicous lap" (Laps 0 1 1)
        [ ( 0, 1)
        , (10, 3)
        , (20, 2)
        , (10, 1)
        ]

    , testCounter "little results" (Laps 1 0 0)
        [ ( 0, 1)
        , (20, 3)
        , (40, 1)
        ]
    ]
