{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Counter.Tests
    ( tests
    ) where

import Data.Monoid (Monoid, mempty, mappend)
import Control.Monad.Writer (Writer, tell, execWriter)
import Control.Monad.State (StateT, evalStateT, get, put)

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
type CounterTestM a = StateT Counter (Writer Laps) a

-- | Utility function for use in testing code
--
see :: Int              -- ^ Position
    -> Int              -- ^ Time
    -> CounterTestM ()  -- ^ No result
see position time = do
    counter <- get
    let (lap, counter') = tick (position, time) counter
    put counter'
    case lap of
        Nothing                -> return ()
        Just Lap               -> tell $ Laps 1 0
        Just (SuspiciousLap _) -> tell $ Laps 0 1

-- | Test a counter
--
testCounter :: String           -- ^ Test name
            -> Laps             -- ^ Expected laps
            -> CounterTestM ()  -- ^ Testing code
            -> Test             -- ^ Resulting test
testCounter name laps counterTestM = testCase name $
    laps @=? execWriter (evalStateT counterTestM mempty)

-- | Actual tests
--
tests :: [Test]
tests =
    [ testCounter "simple example" (Laps 3 0) $ do
        see 1  20
        see 2  40
        see 3  60
        see 1  80
        see 2 100
        see 3 120
        see 1 150
        see 2 180
        see 3 210
        see 1 240
    ]
