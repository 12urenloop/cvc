module Main
    ( main
    ) where

import Control.Concurrent (forkIO)

import CountVonCount.FiniteChan
import CountVonCount.Dispatcher
import CountVonCount.CsvLog
import CountVonCount.Receiver.Stdin
import CountVonCount.Receiver.Socket
import CountVonCount.Rest
import CountVonCount.Configuration

main :: IO ()
main = do
    -- Create logger
    logChan <- newFiniteChan "LOG" putStrLn
    let logger = writeFiniteChan logChan

    inChan <- newFiniteChan "IN" logger
    outChan <- newFiniteChan "OUT" logger
    csvLogChan <- dupFiniteChan "PERSISTENCE" inChan

    -- Log thread
    _ <- forkIO $ runFiniteChan logChan () $ \str () ->
        putStrLn str

    -- Ugly, ugly, ugly
    Just configuration <- loadConfigurationFromFile "config.yaml"

    print $ configurationMacSet configuration

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

    waitFiniteChan inChan
    waitFiniteChan outChan
    endFiniteChan logChan
    waitFiniteChan logChan
