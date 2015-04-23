--------------------------------------------------------------------------------
module CountVonCount.Log
    ( Log
    , open
    , close
    , string
    , raw
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative     ((<$>), (<*>))
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad           (when)
import qualified Data.Time               as Time
import qualified System.IO               as IO


--------------------------------------------------------------------------------
data LogStep
    = LogString String
    | LogStop
    deriving (Show)


--------------------------------------------------------------------------------
-- | Handle to a log
data Log = Log
    { logChan :: Chan LogStep
    , logStop :: MVar ()
    }


--------------------------------------------------------------------------------
-- | Open a new log file. New content is appended and optionally also
-- printed to stdout
open :: FilePath -> Bool -> IO Log
open filePath logStdout = do
    logger <- Log <$> newChan <*> newEmptyMVar
    handle <- IO.openFile filePath IO.AppendMode
    _      <- forkIO $ writer logger handle logStdout
    return logger


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
-- | Close a log. This method blocks until all queued messages have been
-- written.
close :: Log -> IO ()
close logger = do
    writeChan (logChan logger) LogStop
    takeMVar (logStop logger)


--------------------------------------------------------------------------------
-- | Write a message to the log. This function will prepend the current time.
string :: Log      -- ^ Log handle to write to
       -> String   -- ^ Fully qualified source function name
       -> String   -- ^ Actual message
       -> IO ()    -- ^ Returns immediately
string logger fname str = do
    tz  <- Time.getCurrentTimeZone
    utc <- Time.getCurrentTime
    let time = Time.utcToLocalTime tz utc
        fmt  = Time.formatTime Time.defaultTimeLocale "[%H:%M:%S]" time
    raw logger $ fmt ++ " " ++ fname ++ ": " ++ str


--------------------------------------------------------------------------------
-- | Write a raw line to the log.
raw :: Log -> String -> IO ()
raw logger = writeChan (logChan logger) . LogString
