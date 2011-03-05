module Main
    ( main
    ) where

import Control.Concurrent (forkIO)
import Control.Monad (forM_)
import Control.Applicative ((<$>))

import CountVonCount.FiniteChan
import CountVonCount.Parser
import CountVonCount.Dispatcher
import CountVonCount.Persistence

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
    _ <- forkIO $ do
        lines' <- lines <$> getContents
        forM_ lines' $ \line -> do
            let measurement = parse line
            writeFiniteChan inChan measurement
        endFiniteChan inChan

    waitFiniteChan inChan
    waitFiniteChan outChan
