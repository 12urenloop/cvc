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
    [ testCase "test01" test01
    ]

testPersistence :: Persistence Bool -> Assertion
testPersistence x =
    let x' = runPersistence x :: IO Bool
    in assert x'

test01 :: Assertion
test01 = testPersistence $ do
    let x = Team "wina" 4 Nothing
    r  <- add x
    x' <- get r
    return $ x == x'
