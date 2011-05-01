module Main
    ( main
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, dupChan, writeChan)
import Control.Monad (when)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Time (getCurrentTime, formatTime)
import System.Locale (defaultTimeLocale)
import System.Environment (getArgs)

import CountVonCount.Chan
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
    inChan <- newChan
    outChan <- newChan
    csvLogChan <- dupChan inChan

    -- Out thread
    _ <- forkIO $ runRest configuration logger outChan

    -- Watcher thread
    _ <- forkIO $ runDispatcher configuration logger inChan outChan

    -- Persistence thread
    _ <- forkIO $ runCsvLog configuration csvLogChan

    -- In thread
    socketReceiver configuration logger inChan

main :: IO ()
main = do
    -- First things first: create a logger
    logChan <- newChan
    let initialLogger = logger' logChan Debug

    -- Log thread
    _ <- forkIO $ readChanLoop logChan putStrLn

    -- Get Configuration file name and load the configuration
    configFile <- fromMaybe "config.yaml" . listToMaybe <$> getArgs
    initialLogger Info $ "CountVonCount.Main.main: Loading: " ++ configFile
    mconf <- loadConfigurationFromFile configFile

    case mconf of
        Just conf -> do
            let logger = logger' logChan (configurationVerbosity conf)
            countVonCount conf logger
            logger Info "CountVonCount.Main.main: Bye!"
        Nothing   -> initialLogger Error $
            "CountVonCount.Main.main: Could not load config file, " ++
            "bailing out"
  where
    -- Logger thread
    logger' chan treshold verbosity string = when (verbosity >= treshold) $ do
        time <- formatTime defaultTimeLocale "%H:%M:%S" <$> getCurrentTime
        writeChan chan $  "[" ++ time ++ "] ["
                             ++ show verbosity ++ "] " ++ string
