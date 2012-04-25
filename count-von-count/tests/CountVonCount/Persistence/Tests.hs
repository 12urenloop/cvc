{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Persistence.Tests
    ( tests
    ) where

import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)

import Data.Text (Text)
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

    , testCase "addLaps/getLatestLaps" $ testPersistence $ do
        r    <- addTeam wina
        time <- liftIO getCurrentTime
        let reason1 = "Because they're gay"
            laps1   = 10
            reason2 = "Bousson = FAG"
            laps2   = 2

        _            <- addLaps r time reason1 laps1
        _            <- addLaps r time reason2 laps2
        [lap2, lap1] <- getLatestLaps r 2

        return $
            -- Might a marginal difference in the times due to conversion,
            -- should never be more than one second
            ((time, reason1, laps1) `eqLap` lap1) &&
            ((time, reason2, laps2) `eqLap` lap2)
    ]

eqLap :: (UTCTime, Text, Int) -> Lap -> Bool
eqLap (time, reason, laps) lap =
    abs (lapTimestamp lap `diffUTCTime` time) < 1 &&
    lapReason lap == reason &&
    lapCount lap == laps
