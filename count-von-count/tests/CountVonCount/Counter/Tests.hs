{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Counter.Tests
    ( tests
    ) where

import Data.Monoid (Monoid, mempty, mappend)
import Control.Monad.Writer (Writer, tell, execWriter)
import Control.Monad.State (StateT, evalStateT, get, put, modify)
import Control.Arrow (second)

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, State)

import CountVonCount.Types
import CountVonCount.Counter

-- | Number of laps and number of suspicious laps
--
data Laps = Laps Int Int
          deriving (Show, Eq)

instance Monoid Laps where
    mempty = Laps 0 0
    Laps x1 y1 `mappend` Laps x2 y2 = Laps (x1 + x2) (y1 + y2)

-- | Monad for counter testing
--
type CounterTestM a = StateT (Counter, Int) (Writer Laps) a

-- | Utility function for use in testing code: see the racer at a given position
--
see :: Int              -- ^ Position
    -> CounterTestM ()  -- ^ No result
see position = do
    (counter, time) <- get
    let (lap, counter') = tick (position, time) counter
    put (counter', time)
    case lap of
        Nothing                -> return ()
        Just Lap               -> tell $ Laps 1 0
        Just (SuspiciousLap _) -> tell $ Laps 0 1

-- | Utility function for use in testing code: increment time (optionally
-- negative)
--
wait :: Int              -- ^ Time to wait
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
    ]
