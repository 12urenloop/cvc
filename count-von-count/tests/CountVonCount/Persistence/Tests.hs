{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Persistence.Tests
    ( tests
    ) where

import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)
import Data.Time (diffUTCTime, getCurrentTime)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import CountVonCount.Persistence
import CountVonCount.Persistence.Tests.Util

wina :: Team
wina = Team "homos" "wina" 4 (Just "00:40:10:07:00:09")

tests :: Test
tests = testGroup "CountVonCount.Persistence.Tests"
    [ testCase "store/get team" $ testPersistence $ do
        r  <- addTeam wina
        x' <- getTeam r
        return $ wina == x'

    , testCase "getTeamByMac" $ testPersistence $ do
        r  <- addTeam wina
        x' <- getTeamByMac $ fromJust $ teamBaton wina
        return $ Just (r, wina) == x'

    , testCase "addLaps/latestLap" $ testPersistence $ do
        r    <- addTeam wina
        time <- liftIO getCurrentTime
        let reason = "Because they're gay"
            laps   = 10

        _   <- addLaps r wina time reason laps
        lap <- latestLap r

        return $
            -- Might a marginal difference in the times due to conversion,
            -- should never be more than one second
            abs (lapTimestamp lap `diffUTCTime` time) < 1 &&
            lapReason lap == reason &&
            lapCount lap == laps
    ]
