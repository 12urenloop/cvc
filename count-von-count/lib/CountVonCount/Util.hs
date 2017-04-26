--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module CountVonCount.Util
    ( isolate
    , isolate_
    ) where


--------------------------------------------------------------------------------
import qualified Control.Exception as E


--------------------------------------------------------------------------------
import CountVonCount.Log


--------------------------------------------------------------------------------
-- | Isolate any exception in the given worker code and log it
isolate :: Log -> String -> IO () -> IO (Maybe E.SomeException)
isolate logger name worker = E.catches (worker >> return Nothing)
    [ E.Handler $ \async -> case async of
        E.UserInterrupt -> E.throw E.UserInterrupt
        _               -> isolate' (E.SomeException async)
    , E.Handler $ \(ex :: E.SomeException) -> isolate' ex
    ]
  where
    isolate' ex = do
        string logger "CountVonCount.Util.isolate" $
            name ++ " threw: " ++ show ex
        return (Just ex)


--------------------------------------------------------------------------------
isolate_ :: Log -> String -> IO () -> IO ()
isolate_ logger name worker = do
    _ <- isolate logger name worker
    return ()
