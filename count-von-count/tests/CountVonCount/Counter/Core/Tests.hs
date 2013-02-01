--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Counter.Core.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                          (Test, testGroup)
import           Test.Framework.Providers.HUnit          (testCase)
import           Test.HUnit                              (assert)


--------------------------------------------------------------------------------
import           CountVonCount.Counter.Core
import           CountVonCount.Counter.Fixtures
import           CountVonCount.Counter.Fixtures.Internal
import           CountVonCount.Persistence


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "CountVonCount.Counter.Core.Tests"
    [ testCase ("counter core: " ++ name) (assert $ testCounterFixtureM fixture)
    | (name, fixture) <- fixtures
    ]


--------------------------------------------------------------------------------
testCounterFixtureM :: CounterFixtureM () -> Bool
testCounterFixtureM cf = test emptyCounterState $
    runCounterFixtureM cf time baton
  where
    baton = Baton 0 "Baton is irrelevant" 0
    step  = stepCounterState circuitLength maxSpeed

    test _     []                                = True
    test state (CounterFixture se expected : xs) =
        let (es, _, state') = runCounterM (step se) state
        in any isLap es == expected && test state' xs
