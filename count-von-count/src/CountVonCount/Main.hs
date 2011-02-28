module Main
    ( main
    ) where

import Control.Concurrent.Chan.Strict (newChan, readChan, writeChan)
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import CountVonCount.Parser
import CountVonCount.Dispatcher

main :: IO ()
main = do
    inChan <- newChan
    outChan <- newChan
    dispatcher <- makeDispatcher inChan outChan

    -- Out thread
    _ <- forkIO $ forever $ do
        lap <- readChan outChan
        putStrLn $ show lap

    -- Watcher thread
    _ <- forkIO $ runDispatcher dispatcher

    -- In thread
    forever $ do
        line <- getLine
        let measurement = parse line
        writeChan inChan measurement
