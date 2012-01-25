module CountVonCount.Log
    ( Log
    , openLog
    , closeLog
    , logStr
    , logRaw
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import System.Locale (defaultTimeLocale)
import qualified System.IO as IO

import qualified Data.Time as Time

data LogStep
    = LogString String
    | LogStop
    deriving (Show)

-- | Handle to a log
data Log = Log
    { logChan :: Chan LogStep
    , logStop :: MVar ()
    }

-- | Open a new log file. New content is appended.
openLog :: FilePath -> IO Log
openLog filePath = do
    logger <- Log <$> newChan <*> newEmptyMVar
    handle <- IO.openFile filePath IO.AppendMode
    _      <- forkIO $ writer logger handle
    return logger

-- | Worker thread to write to the log
writer :: Log -> IO.Handle -> IO ()
writer logger handle = do
    IO.hSetBuffering handle IO.LineBuffering
    loop
  where
    loop = do
        step <- readChan $ logChan logger
        case step of
            LogString str -> IO.hPutStrLn handle str >> loop
            LogStop       -> IO.hClose handle >> putMVar (logStop logger) ()

-- | Close a log. This method blocks until all queued messages have been
-- written.
closeLog :: Log -> IO ()
closeLog logger = do
    writeChan (logChan logger) LogStop
    takeMVar (logStop logger)

-- | Write a message to the log. This function will prepend the current time.
logStr :: Log      -- ^ Log handle to write to
       -> String   -- ^ Name of the component (usually canonical module name)
       -> String   -- ^ Actual message
       -> IO ()    -- ^ Returns immediately
logStr logger component str = do
    tz  <- Time.getCurrentTimeZone
    utc <- Time.getCurrentTime
    let time = Time.utcToLocalTime tz utc
        fmt  = Time.formatTime defaultTimeLocale "[%H:%M:%S]" time
    logRaw logger $ fmt ++ " " ++ component ++ ": " ++ str

-- | Write a raw line to the log.
logRaw :: Log -> String -> IO ()
logRaw logger = writeChan (logChan logger) . LogString
