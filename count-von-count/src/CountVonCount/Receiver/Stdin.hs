-- | Testing purposes receiver that receives the input on stdin in a simple
-- format
--
module CountVonCount.Receiver.Stdin
    ( stdinReceiver
    ) where

import Control.Monad (forM_)
import Control.Applicative ((<$>))

import CountVonCount.Receiver
import CountVonCount.FiniteChan

stdinReceiver :: Receiver
stdinReceiver chan = do
    lines' <- lines <$> getContents
    forM_ lines' $ \line -> do
        let measurement = parse line
        writeFiniteChan chan measurement
    endFiniteChan chan
  where
    parse line =
        let [mac, timestamp, position] = words line
        in (mac, (read timestamp, read position))
