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
    runCounterFixtureM cf time team baton
  where
    baton = Baton 0 "Baton is irrelevant" 0
    team  = Team 0 "Team is irrelevant" 0 (Just $ batonId baton)
    step  = stepCounterState circuitLength maxSpeed

    test _     []                                = True
    test state (CounterFixture se expected : xs) =
        let (es, _, state') = runCounterM (step se) state
        in any isLapEvent es == expected && test state' xs
