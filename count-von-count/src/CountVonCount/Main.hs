module Main
    ( main
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, writeChan)
import Control.Concurrent.MVar (newMVar)
import Control.Monad (when)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Time (getCurrentTime, formatTime)
import System.Locale (defaultTimeLocale)
import System.Environment (getArgs)

import CountVonCount.Chan
import CountVonCount.Dispatcher
import CountVonCount.ReplayLog
import CountVonCount.Receiver
import CountVonCount.Rest
import CountVonCount.Configuration
import CountVonCount.Types
import CountVonCount.Admin

countVonCount :: Configuration
              -> Logger
              -> IO ()
countVonCount configuration logger = do
    -- Create channels
    inChan <- newChan
    outChan <- newChan
    replayLogChan <- newChan

    -- Out thread
    _ <- forkIO $ runRest configuration logger outChan

    -- Watcher thread
    dispatcherState <- newMVar emptyDispatcherState
    _ <- forkIO $
        runDispatcher configuration logger dispatcherState inChan outChan

    -- Admin interface
    _ <- forkIO $ runAdmin configuration dispatcherState

    -- Persistence thread
    _ <- forkIO $ runReplayLog configuration replayLogChan

    -- In thread
    socketReceiver configuration logger inChan replayLogChan

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
    conf <- loadConfigurationFromFile configFile

    let logger = logger' logChan (configurationVerbosity conf)
    countVonCount conf logger
    logger Info "CountVonCount.Main.main: Bye!"
  where
    -- Logger thread
    logger' chan treshold verbosity string = when (verbosity >= treshold) $ do
        time <- formatTime defaultTimeLocale "%H:%M:%S" <$> getCurrentTime
        writeChan chan $  "[" ++ time ++ "] ["
                             ++ show verbosity ++ "] " ++ string
