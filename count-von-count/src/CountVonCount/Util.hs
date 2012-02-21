{-# LANGUAGE ScopedTypeVariables #-}
module CountVonCount.Util
    ( isolate
    ) where

import Control.Exception (SomeException, catch)
import Prelude hiding (catch)

import CountVonCount.Log

-- | Isolate any exception in the given worker code and log it
isolate :: Log -> String -> IO () -> IO ()
isolate logger name worker = catch worker $ \(ex :: SomeException) ->
    string logger $ "[isolate " ++ name ++ "]: caught exception: " ++ show ex
