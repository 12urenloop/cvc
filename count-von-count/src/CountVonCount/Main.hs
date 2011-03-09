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
    inChan <- newFiniteChan "IN"
    outChan <- newFiniteChan "OUT"
    csvLogChan <- dupFiniteChan "PERSISTENCE" inChan

    -- Ugly, ugly, ugly
    Just configuration <- loadConfigurationFromFile "config.yaml"

    -- Out thread
    _ <- forkIO $ runRest configuration outChan

    -- Watcher thread
    _ <- forkIO $ do
        runDispatcher configuration inChan outChan
        endFiniteChan outChan

    -- Persistence thread
    _ <- forkIO $ runCsvLog configuration csvLogChan

    -- In thread
    _ <- forkIO $ stdinReceiver inChan

    waitFiniteChan inChan
    waitFiniteChan outChan
