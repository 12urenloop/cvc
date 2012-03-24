module CountVonCount.Persistence.Tests.Util
    ( testPersistence
    ) where

import Test.HUnit (Assertion, assert)

import CountVonCount.Persistence

testPersistence :: Persistence Bool -> Assertion
testPersistence x = assert x'
  where
    x' :: IO Bool
    x' = runPersistence $ do
        r <- x
        deleteAll (undefined :: Team)
        deleteAll (undefined :: Lap)
        return r
