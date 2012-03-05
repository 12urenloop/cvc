{-# LANGUAGE ScopedTypeVariables #-}
module CountVonCount.Util
    ( isolate
    ) where

import qualified Control.Exception as E

import CountVonCount.Log

-- | Isolate any exception in the given worker code and log it
isolate :: Log -> String -> IO () -> IO ()
isolate logger name worker = E.catches worker
    [ E.Handler $ \async -> case async of
        E.UserInterrupt -> E.throw E.UserInterrupt
        _               -> isolate' async
    , E.Handler $ \(ex :: E.SomeException) -> isolate' ex
    ]
  where
    isolate' ex = string logger $ "[isolate " ++ name ++ "]: caught " ++ show ex
