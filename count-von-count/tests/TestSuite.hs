module TestSuite where

import Test.Framework (defaultMain, testGroup)

import qualified CountVonCount.Analyzer.Tests

main :: IO ()
main = defaultMain
    [ testGroup "CountVonCount.Analyzer.Tests"
        CountVonCount.Analyzer.Tests.tests
    ]
