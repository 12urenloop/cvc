--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Persistence.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans            (liftIO)
import           Data.Text                      (Text)
import           Data.Time                      (UTCTime, diffUTCTime,
                                                 getCurrentTime)
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assert, (@=?))


--------------------------------------------------------------------------------
import           CountVonCount.Persistence
import           CountVonCount.TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "CountVonCount.Persistence.Tests"
    [ testCase "store/get team" $ testDatabase $ \db -> do
        r  <- addTeam db "wina"
        x' <- getTeam db r
        "wina" @=? teamName x'

    , testCase "getTeamByMac" $ testDatabase $ \db -> do
        r       <- addTeam db "wina"
        batons' <- getAllBatons db
        setTeamBaton db r (Just $ batonId $ head batons')
        x' <- getTeamByMac db (batonMac $ head batons')
        Just "wina" @=? fmap teamName x'

    , testCase "addLaps/getLatestLaps" $ testDatabase $ \db -> do
        r    <- addTeam db "wina"
        time <- liftIO getCurrentTime
        let reason1 = Just "Because they're gay"
            laps1   = 10
            reason2 = Nothing
            laps2   = 2
        _    <- addLaps db r time reason1 laps1
        _    <- addLaps db r time reason2 laps2
        _    <- addLaps db r time reason2 laps2
        _    <- addLaps db r time reason1 laps1
        _    <- addLaps db r time reason1 laps1
        _    <- addLaps db r time reason2 laps2
        _    <- addLaps db r time reason2 laps2
        laps <- getLatestLaps db (Just r) 5

        -- Might a marginal difference in the times due to conversion,
        -- should never be more than one second
        assert ((time, reason1, laps1) `eqLap` (laps !! 2))
        assert ((time, reason2, laps2) `eqLap` (laps !! 1))
        assert ((time, reason2, laps2) `eqLap` (laps !! 4))
    ]


--------------------------------------------------------------------------------
eqLap :: (UTCTime, Maybe Text, Int) -> Lap -> Bool
eqLap (time, reason, laps) lap =
    abs (lapTimestamp lap `diffUTCTime` time) < 1 &&
    lapReason lap == reason &&
    lapCount lap == laps
