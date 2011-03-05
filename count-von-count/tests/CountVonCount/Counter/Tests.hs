{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Counter.Tests
    ( tests
    ) where

import Data.Monoid (Monoid, mempty, mappend)
import Control.Monad.Writer (Writer, tell, execWriter)
import Control.Monad.State (StateT, runState, evalStateT, get, put, modify)
import Control.Arrow (second)

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, State)

import CountVonCount.Types
import CountVonCount.Counter

-- | Number of laps, suspicious laps and refused laps
--
data Laps = Laps Int Int Int
          deriving (Show, Eq)

instance Monoid Laps where
    mempty = Laps 0 0 0
    Laps x1 y1 z1 `mappend` Laps x2 y2 z2 = Laps (x1 + x2) (y1 + y2) (z1 + z2)

-- | Monad for counter testing
--
type CounterTestM a = StateT (CounterState, Timestamp) (Writer Laps) a

-- | Utility function for use in testing code: see the racer at a given position
--
see :: Position         -- ^ Position
    -> CounterTestM ()  -- ^ No result
see position = do
    -- Run the counter
    (state, time) <- get
    let measurement = (time, position)
        (score, state') = runState (counter measurement) state

    -- Analyze counter results
    case score of
        Just Good        -> tell (Laps 1 0 0)
        Just (Warning _) -> tell (Laps 0 1 0)
        Just (Refused _) -> tell (Laps 0 0 1)
        Nothing          -> return ()

    -- Save the counter state
    put (state', time)

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
    laps @=? execWriter (evalStateT counterTestM (emptyCounterState, 0))

-- | Actual tests
--
tests :: [Test]
tests =
    [ testCounter "simple example" (Laps 3 0 0) $ do
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

    , testCounter "suspicous lap" (Laps 0 1 0) $ do
        see 1
        wait 5
        see 3
        wait 1
        see 2
        wait 1
        see 1

    , testCounter "little results" (Laps 0 1 0) $ do
        see 1
        wait 10
        see 3
        wait 10
        see 1
    ]
