module Main
    ( main
    ) where

import Control.Concurrent (forkIO)

import CountVonCount.FiniteChan
import CountVonCount.Dispatcher
import CountVonCount.Persistence
import CountVonCount.Receiver.Stdin

main :: IO ()
main = do
    inChan <- newFiniteChan "IN"
    outChan <- newFiniteChan "OUT"
    persistenceChan <- dupFiniteChan "PERSISTENCE" inChan

    -- Out thread
    _ <- forkIO $ do
        runFiniteChan outChan () $ \lap () -> putStrLn $ show lap

    -- Watcher thread
    _ <- forkIO $ do
        runDispatcher inChan outChan
        endFiniteChan outChan

    -- Persistence thread
    _ <- forkIO $ runPersistence persistenceChan

    -- In thread
    _ <- forkIO $ stdinReceiver inChan

    waitFiniteChan inChan
    waitFiniteChan outChan
