{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Analyzer.Tests
    ( tests
    ) where

import Data.Monoid (Monoid, mempty, mappend)
import Control.Monad.Writer (Writer, tell, execWriter)
import Control.Monad.State (StateT, evalStateT, get, put, modify)
import Control.Arrow (second)
import Debug.Trace (traceShow)
import Test.QuickCheck (Arbitrary, arbitrary, shrink)

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, State)

import CountVonCount.DataSet
import CountVonCount.Types
import CountVonCount.Counter
import CountVonCount.Analyzer

-- | Number of laps and number of suspicious laps
--
data Laps = Laps Int Int
          deriving (Show, Eq)

instance Monoid Laps where
    mempty = Laps 0 0
    Laps x1 y1 `mappend` Laps x2 y2 = Laps (x1 + x2) (y1 + y2)

-- | Monad for counter testing
--
type CounterTestM a = StateT (DataSet, Timestamp) (Writer Laps) a

-- | Utility function for use in testing code: see the racer at a given position
--
see :: Position         -- ^ Position
    -> CounterTestM ()  -- ^ No result
see position = do
    (dataSet, time) <- get
    let dataSet' = addMeasurement (time, position) dataSet
        score    = analyze dataSet'
    traceShow score $ case score of
        Good      -> tell (Laps 1 0) >> put (mempty, time)
        Warning _ -> tell (Laps 0 1) >> put (mempty, time)
        Refused _ -> put (dataSet', time)

-- | Utility function for use in testing code: increment time (optionally
-- negative)
--
wait :: Timediff         -- ^ Time to wait
     -> CounterTestM ()  -- ^ No result
wait increment = modify $ second (+ increment)

-- | Test a counter
--
testCounter :: String           -- ^ Test name
            -> Laps             -- ^ Expected laps
            -> CounterTestM ()  -- ^ Testing code
            -> Test             -- ^ Resulting test
testCounter name laps counterTestM = testCase name $
    laps @=? execWriter (evalStateT counterTestM (mempty, 0))

-- | Actual tests
--
tests :: [Test]
tests =
    [ testCounter "simple example" (Laps 3 0) $ do
        wait 10 >> see 1
        wait 10 >> see 2
        wait 10 >> see 3
        wait 10 >> see 1
        wait 10 >> see 2
        wait 10 >> see 3
        wait 10 >> see 1
        wait 10 >> see 2
        wait 10 >> see 3
        wait 10 >> see 1

    , testCounter "suspicous lap" (Laps 0 1) $ do
        see 1
        wait 5
        see 2
        wait (-1)
        see 3
        wait 1
        see 1

    , testCounter "skips" (Laps 1 0) $ do
        see 1
        wait 10
        see 3
        wait 10
        see 1
    ]
