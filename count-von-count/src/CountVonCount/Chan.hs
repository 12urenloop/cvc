-- | Channel abstractions
--
module CountVonCount.Chan
    ( readChanLoop
    , statefulReadChanLoop
    ) where

import Control.Concurrent.Chan (Chan, readChan)

readChanLoop :: Chan a
             -> (a -> IO ())
             -> IO ()
readChanLoop chan f = statefulReadChanLoop chan () (const . f)

statefulReadChanLoop :: Chan a
                     -> s
                     -> (a -> s -> IO s)
                     -> IO ()
statefulReadChanLoop chan initial f = go initial
  where
    go state = do
        x <- readChan chan
        state' <-  f x state
        go state'
