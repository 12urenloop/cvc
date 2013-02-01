--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Management.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans                  (liftIO)
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.HUnit                           ((@=?))


--------------------------------------------------------------------------------
import           CountVonCount.Counter
import           CountVonCount.Management
import           CountVonCount.Persistence
import           CountVonCount.Persistence.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "CountVonCount.Management.Tests"
    [ testCase "assignBaton/assignment" $ testDatabase $ \db -> do
        counter <- liftIO newCounter
        refs    <- mapM (addTeam db) ["wina", "vtk", "vek"]
        batons' <- getAllBatons db

        assignBaton db counter (batonId $ batons' !! 1) (refs !! 2)
        assignBaton db counter (batonId $ batons' !! 2) (refs !! 1)
        (withBatons, freeBatons) <- assignment db

        [batons !! 0]        @=? freeBatons
        [Just (batons' !! 1)] @=? [b | (t, b) <- withBatons, teamName t == "vek"]
        [Just (batons' !! 2)] @=? [b | (t, b) <- withBatons, teamName t == "vtk"]
    ]
