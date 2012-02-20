{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Persistence.Tests
    ( tests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assert)

import CountVonCount.Persistence

tests :: Test
tests = testGroup "CountVonCount.Persistence.Tests"
    [ testCase "store/get team" $ testPersistence $ do
        let x = Team 1 "wina" 4 Nothing
        r  <- add x
        x' <- get r
        return $ x == x'

    , testCase "getTeamByMac" $ testPersistence $ do
        let mac = "00:40:10:07:00:09"
            x   = Team 1 "wina" 4 (Just mac)
        r  <- add x
        x' <- getTeamByMac mac
        return $ Just (r, x) == x'
        
    ]

testPersistence :: Persistence Bool -> Assertion
testPersistence x =
    let x' = runPersistence x :: IO Bool
    in assert x'
