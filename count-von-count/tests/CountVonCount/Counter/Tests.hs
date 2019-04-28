--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Counter.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent                      (threadDelay)
import           Control.Monad                           (forM, forM_)
import           Data.List                               (sortBy)
import           Data.Ord                                (comparing)
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Test.Framework                          (Test, testGroup)
import           Test.Framework.Providers.HUnit          (testCase)
import           Test.HUnit                              (Assertion,
                                                          assertEqual)
import           Text.Printf                             (printf)


--------------------------------------------------------------------------------
import qualified CountVonCount.Counter                   as Counter
import           CountVonCount.Counter.Fixtures
import           CountVonCount.Counter.Fixtures.Internal
import           CountVonCount.EventBase
import           CountVonCount.Persistence
import           CountVonCount.Sensor.Filter
import           CountVonCount.TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "CountVonCount.Counter.Tests"
    [ testCase "counter test" counterTest
    ]


--------------------------------------------------------------------------------
counterTest :: Assertion
counterTest = testLog $ \logger -> do
    -- Initialize stuffs
    db        <- newDatabase "test.db"

    -- Add stations
    forM_ testStations (\s -> addStation db (stationName s)
                                            (stationMac s)
                                            (stationPosition s))

    counter   <- Counter.newCounter logger db circuitLength maxSpeed
    eventBase <- newEventBase logger
    Counter.subscribe counter eventBase

    -- Add teams, calculate expected output
    ts <- forM (zip teamsAndBatons fixtures) $ \((name, baton), f) -> do
        tr     <- addTeam db name
        br     <- addBaton db (batonMac baton) (batonName baton)
        team   <- getTeam db tr
        baton' <- getBaton db br
        setTeamBaton db tr $ Just br
        let fs = runCounterFixtureM (snd f) time team baton'
        return (tr, sensorEvents fs, numLaps fs)

    -- Feed input to chan
    let events = sortBy (comparing sensorTime) [e | (_, es, _) <- ts, e <- es]
    forM_ events $ publish eventBase

    -- Some time for the DB
    threadDelay $ 1 * 1000 * 1000

    -- Check the laps for each team
    forM_ ts $ \(ref, _, laps) -> do
        team <- getTeam db ref
        assertEqual
          (printf "Unexpected lap count for %s" (show team))
          laps $
          teamLaps team

    -- Cleanup
    deleteAll db


--------------------------------------------------------------------------------
teamsAndBatons :: [(Text, Baton)]
teamsAndBatons =
    [ (name, Baton (fromIntegral i) mac (T.pack $ "Baton " ++ show i))
    | i <- [1 :: Int .. 99]
    , let name = "Team " `T.append` T.pack (show i)
    , let mac  = "01:02:03:00:00" `T.append` T.pack (printf "%2d" i)
    ]
