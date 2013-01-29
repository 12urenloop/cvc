--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Counter.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent                      (forkIO, killThread,
                                                          threadDelay)
import           Control.Concurrent.Chan                 (newChan, writeChan)
import           Control.Monad                           (forM, forM_)
import           Data.List                               (sortBy)
import           Data.Ord                                (comparing)
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Test.Framework                          (Test, testGroup)
import           Test.Framework.Providers.HUnit          (testCase)
import           Test.HUnit                              (Assertion, (@=?))
import           Text.Printf                             (printf)


--------------------------------------------------------------------------------
import           CountVonCount.Counter
import           CountVonCount.Counter.Fixtures
import           CountVonCount.Counter.Fixtures.Internal
import qualified CountVonCount.Log                       as Log
import           CountVonCount.Persistence
import           CountVonCount.Sensor.Filter
import           CountVonCount.Types


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "CountVonCount.Counter.Tests"
    [ testCase "counter test" counterTest
    ]


--------------------------------------------------------------------------------
counterTest :: Assertion
counterTest = do
    -- Initialize stuffs
    logger   <- Log.open "/dev/null" False
    db       <- newDatabase "test.db"
    counter  <- newCounter
    chan     <- newChan
    threadId <- forkIO $ runCounter counter circuitLength maxSpeed
        logger db handler' chan

    -- Add teams, calculate expected output
    ts <- forM (zip teamsAndBatons fixtures) $ \((name, baton), f) -> do
        r <- addTeam db name
        setTeamBaton db r $ Just baton
        let fs = runCounterFixtureM (snd f) time baton
        return (r, sensorEvents fs, numLaps fs)

    -- Feed input to chan
    let events = sortBy (comparing sensorTime) [e | (_, es, _) <- ts, e <- es]
    forM_ events $ writeChan chan

    -- 3 seconds, we have plenty of time
    threadDelay $ 3 * 1000 * 1000

    -- Check the laps for each team
    forM_ ts $ \(ref, _, laps) -> do
        team <- getTeam db ref
        laps @=? teamLaps team

    -- Check the output
    deleteAll db
    killThread threadId
  where
    handler' = handler "Crashing handler" $
        error "Errors in handlers should not break this test"


--------------------------------------------------------------------------------
teamsAndBatons :: [(Text, Baton)]
teamsAndBatons =
    [ (name, Baton mac i)
    | i <- [1 :: Int .. 99]
    , let name = "Team " `T.append` T.pack (show i)
    , let mac  = "01:02:03:00:00" `T.append` T.pack (printf "%2d" i)
    ]
