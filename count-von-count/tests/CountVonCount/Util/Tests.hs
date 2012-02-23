{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Util.Tests
    ( tests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assert)

import CountVonCount.Util
import qualified CountVonCount.Log as Log

tests :: Test
tests = testGroup "CountVonCount.Util.Tests"
    [ testCase "isolate test" $ assert $ do
        logger <- Log.open "/dev/null"
        isolate logger "isolate test" $ fail "Sup guys"
        return True
    ]
