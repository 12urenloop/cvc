-- | Saves received values to the disk somewhere
--
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.ReplayLog
    ( runReplayLog
    ) where

import System.IO (openFile, IOMode (AppendMode), hClose, hFlush)
import Control.Concurrent.Chan (Chan)
import Data.Monoid (mappend)

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC

import CountVonCount.Chan
import CountVonCount.Types
import CountVonCount.Configuration

-- | Run a persistence thread that saves values to a CSV file
--
runReplayLog :: Configuration                 -- ^ Global configuration
             -> Chan (Timestamp, ByteString)  -- ^ Channel to read from
             -> IO ()                         -- ^ Blocks forever
runReplayLog configuration chan = do
    handle <- openFile filePath AppendMode
    readChanLoop chan $ \(time, line) -> do
        SB.hPutStrLn handle $ "REPLAY," `mappend` SBC.pack (show time)
            `mappend` "," `mappend` line
        hFlush handle
    hClose handle
  where
    filePath = configurationReplayLog configuration
