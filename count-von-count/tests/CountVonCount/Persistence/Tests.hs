{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Persistence.Tests
    ( tests
    ) where

import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)

import Data.Time (diffUTCTime, getCurrentTime)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assert)

import CountVonCount.Persistence

wina :: Team
wina = Team "homos" "wina" 4 (Just "00:40:10:07:00:09")

tests :: Test
tests = testGroup "CountVonCount.Persistence.Tests"
    [ testCase "store/get team" $ testPersistence $ do
        r  <- add wina
        x' <- get r
        return $ wina == x'

    , testCase "getTeamByMac" $ testPersistence $ do
        r  <- add wina
        x' <- getTeamByMac $ fromJust $ teamBaton wina
        return $ Just (r, wina) == x'

    , testCase "addLaps/getLaps" $ testPersistence $ do
        r    <- add wina
        time <- liftIO getCurrentTime
        let reason = "Because they're gay" 
            laps   = 10

        addLaps r time reason laps
        (lap : _) <- getLaps 0 10

        return $ lapTeam lap == r &&
            -- Might a marginal difference in the times due to conversion,
            -- should never be more than one second
            abs (lapTimestamp lap `diffUTCTime` time) < 1 &&
            lapReason lap == reason &&
            lapCount lap == laps
    ]

testPersistence :: Persistence Bool -> Assertion
testPersistence x = assert x'
  where
    x' :: IO Bool
    x' = runPersistence $ do
        r <- x
        deleteAll (undefined :: Team)
        deleteAll (undefined :: Lap)
        return r
