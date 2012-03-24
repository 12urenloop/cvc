{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Management.Tests
    ( tests
    ) where

import Control.Monad.Trans (liftIO)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

import CountVonCount.Counter
import CountVonCount.Management
import CountVonCount.Persistence
import CountVonCount.Persistence.Tests.Util
import CountVonCount.Types

batons :: [Baton]
batons =
    [ Baton "11:11:11:11:11:11" 1
    , Baton "22:22:22:22:22:22" 1
    , Baton "33:33:33:33:33:33" 1
    ]

teams :: [Team]
teams =
    [ Team "t1" "wina" 0 Nothing
    , Team "t2" "vtk"  0 Nothing
    , Team "t3" "vek"  0 Nothing
    ]

tests :: Test
tests = testGroup "CountVonCount.Management.Tests"
    [ testCase "findBaton" $
        Just (batons !! 0) @=? findBaton "11:11:11:11:11:11" batons

    , testCase "assignBaton/assignment" $ testPersistence $ do
        counter <- liftIO newCounter
        refs    <- mapM add teams

        liftIO $ assignBaton counter batons (batons !! 1) (refs !! 2)
        liftIO $ assignBaton counter batons (batons !! 2) (refs !! 1)

        (withBatons, freeBatons) <- liftIO $ assignment batons

        return $ freeBatons == [batons !! 0] &&
            (refs !! 2, teams !! 2, Just (batons !! 1)) `elem` withBatons &&
            (refs !! 1, teams !! 1, Just (batons !! 2)) `elem` withBatons
    ]
