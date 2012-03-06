{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Counter.Tests
    ( tests
    ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Chan (newChan, writeChan)
import Control.Monad (forM, forM_)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assert)
import Text.Printf (printf)
import qualified Data.Text as T

import CountVonCount.Counter
import CountVonCount.Counter.Fixtures
import CountVonCount.Counter.Fixtures.Internal
import CountVonCount.Persistence
import CountVonCount.Sensor.Filter
import CountVonCount.Types
import qualified CountVonCount.Log as Log

tests :: Test
tests = testGroup "CountVonCount.Counter.Tests"
    [ testCase "counter test" $ assert counterTest
    ]

counterTest :: IO Bool
counterTest = do
    -- Initialize stuffs
    logger   <- Log.open "/dev/null"
    counter  <- newCounter
    chan     <- newChan
    threadId <- forkIO $ runCounter counter circuitLength maxSpeed
        logger handler chan

    -- Add teams, calculate expected output
    ts <- runPersistence $
        forM (zip [1 ..] (zip teams fixtures)) $ \(i, (t, f)) -> do
            r <- add t
            let baton = Baton (fromJust (teamBaton t)) i
                fs    = runCounterFixtureM (snd f) time baton
            return (r, sensorEvents fs, numLaps fs)

    -- Feed input to chan
    let events = sortBy (comparing sensorTime) [e | (_, es, _) <- ts, e <- es]
    forM_ events $ writeChan chan

    threadDelay 500000

    -- Check the laps for each team
    results <- runPersistence $ forM ts $ \(ref, _, laps) -> do
        team <- get ref
        return $ teamLaps team == laps

    -- Check the output
    runPersistence clean
    killThread threadId
    return $ and results
  where
    handler _ _ = error "Errors in handlers should break this test"

    clean = do
        deleteAll (undefined :: Team)
        deleteAll (undefined :: Lap)

teams :: [Team]
teams =
    [ Team i name 0 (Just mac)
    | i <- [0 :: Int .. 99]
    , let name = "Team " `T.append` T.pack (show i)
    , let mac = "01:02:03:00:00" `T.append` T.pack (printf "%2d" i)
    ]
