module CountVonCount.Log
    ( Log
    , open
    , close
    , setModule
    , string
    , raw
    ) where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (when)
import System.Locale (defaultTimeLocale)
import qualified System.IO as IO

import qualified Data.Time as Time

data LogStep
    = LogString String
    | LogStop
    deriving (Show)

-- | Handle to a log
data Log = Log
    { logChan   :: Chan LogStep
    , logStop   :: MVar ()
    , logModule :: Maybe String
    }

-- | Open a new log file. New content is appended and optionally also
-- printed to stdout
open :: FilePath -> Bool -> IO Log
open filePath logStdout = do
    logger <- Log <$> newChan <*> newEmptyMVar <*> pure Nothing
    handle <- IO.openFile filePath IO.AppendMode
    _      <- forkIO $ writer logger handle logStdout
    return logger

-- | Worker thread to write to the log
writer :: Log -> IO.Handle -> Bool -> IO ()
writer logger handle logStdout = do
    IO.hSetBuffering handle IO.LineBuffering
    loop
  where
    loop = do
        step <- readChan $ logChan logger
        case step of
            LogString str -> do
                when logStdout $ IO.putStrLn str
                IO.hPutStrLn handle str
                loop
            LogStop       -> IO.hClose handle >> putMVar (logStop logger) ()

-- | Close a log. This method blocks until all queued messages have been
-- written.
close :: Log -> IO ()
close logger = do
    writeChan (logChan logger) LogStop
    takeMVar (logStop logger)

-- | Create a logger for a specific module
setModule :: String -> Log -> Log
setModule m l = l {logModule = Just m}

-- | Write a message to the log. This function will prepend the current time.
string :: Log      -- ^ Log handle to write to
       -> String   -- ^ Actual message
       -> IO ()    -- ^ Returns immediately
string logger str = do
    tz  <- Time.getCurrentTimeZone
    utc <- Time.getCurrentTime
    let time = Time.utcToLocalTime tz utc
        fmt  = Time.formatTime defaultTimeLocale "[%H:%M:%S]" time
    raw logger $ fmt ++ " " ++ moduleInfo ++ str
  where
    moduleInfo = maybe "" (++ ": ") $ logModule logger

-- | Write a raw line to the log.
raw :: Log -> String -> IO ()
raw logger = writeChan (logChan logger) . LogString
