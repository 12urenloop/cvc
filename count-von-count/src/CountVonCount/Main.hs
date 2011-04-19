module Main
    ( main
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Time (getCurrentTime, formatTime)
import System.Locale (defaultTimeLocale)
import System.Environment (getArgs)

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
    _ <- forkIO $ runDispatcher configuration logger inChan outChan

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

    -- Get Configuration file name and load the configuration
    configFile <- fromMaybe "config.yaml" . listToMaybe <$> getArgs
    logger $ "CountVonCount.Main.main: Loading configuration: " ++ configFile
    mconf <- loadConfigurationFromFile configFile

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
