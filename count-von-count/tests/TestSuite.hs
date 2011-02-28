module TestSuite where

import Test.Framework (defaultMain, testGroup)

import qualified CountVonCount.Counter.Tests

main :: IO ()
main = defaultMain
    [ testGroup "CountVonCount.Counter.Tests" CountVonCount.Counter.Tests.tests
    ]
