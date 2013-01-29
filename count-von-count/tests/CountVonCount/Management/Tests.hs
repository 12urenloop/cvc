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
import           CountVonCount.Types


--------------------------------------------------------------------------------
batons :: [Baton]
batons =
    [ Baton "11:11:11:11:11:11" 1
    , Baton "22:22:22:22:22:22" 2
    , Baton "33:33:33:33:33:33" 3
    ]


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "CountVonCount.Management.Tests"
    [ testCase "findBaton" $
        Just (batons !! 0) @=? findBaton "11:11:11:11:11:11" batons

    , testCase "assignBaton/assignment" $ testDatabase $ \db -> do
        counter <- liftIO newCounter
        refs    <- mapM (addTeam db) ["wina", "vtk", "vek"]

        assignBaton db counter batons (batons !! 1) (refs !! 2)
        assignBaton db counter batons (batons !! 2) (refs !! 1)

        (withBatons, freeBatons) <- assignment db batons

        [batons !! 0]        @=? freeBatons
        [Just (batons !! 1)] @=? [b | (t, b) <- withBatons, teamName t == "vek"]
        [Just (batons !! 2)] @=? [b | (t, b) <- withBatons, teamName t == "vtk"]
    ]
