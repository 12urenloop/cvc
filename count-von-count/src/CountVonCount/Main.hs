module Main
    ( main
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Data.Time (getCurrentTime, formatTime)
import System.Locale (defaultTimeLocale)

import CountVonCount.FiniteChan
import CountVonCount.Dispatcher
import CountVonCount.CsvLog
import CountVonCount.Receiver
import CountVonCount.Rest
import CountVonCount.Configuration
import CountVonCount.Types

countVonCount :: Configuration
              -> Logger
              -> IO ()
countVonCount configuration logger = do
    -- Create channels
    inChan <- newFiniteChan "IN" logger
    outChan <- newFiniteChan "OUT" logger
    csvLogChan <- dupFiniteChan "PERSISTENCE" inChan

    -- Out thread
    _ <- forkIO $ runRest configuration logger outChan

    -- Watcher thread
    _ <- forkIO $ do
        runDispatcher configuration logger inChan outChan
        endFiniteChan outChan

    -- Persistence thread
    _ <- forkIO $ runCsvLog configuration csvLogChan

    -- In thread
    _ <- forkIO $ socketReceiver configuration logger inChan

    -- Exit cleanly
    waitFiniteChan inChan
    waitFiniteChan outChan

main :: IO ()
main = do
    -- First things first: create a logger
    logChan <- newFiniteChan "LOG" putStrLn
    let logger = logger' logChan

    -- Log thread
    _ <- forkIO $ runFiniteChan logChan () $ \str () ->
        putStrLn str

    -- Load the configuration
    logger "CountVonCount.Main.main: Loading configuration..."

    -- Fetch yaml config
    mconf <- loadConfigurationFromFile "config.yaml"

    case mconf of
        Just conf -> do
            countVonCount conf logger
            logger "CountVonCount.Main.main: Bye!"
            endFiniteChan logChan
            waitFiniteChan logChan
        Nothing   -> logger $
            "CountVonCount.Main.main: Could not load config file, " ++
            "bailing out"
  where
    logger' logChan string = do
        time <- formatTime defaultTimeLocale "%H:%M:%S" <$> getCurrentTime
        writeFiniteChan logChan $ "[" ++ time ++ "] " ++ string
