-- | Saves received values to the disk somewhere
--
module CountVonCount.CsvLog
    ( runCsvLog
    ) where

import System.IO (openFile, IOMode (AppendMode), hPutStrLn, hClose)
import Text.Printf (printf)

import CountVonCount.FiniteChan
import CountVonCount.Types
import CountVonCount.Configuration

-- | Run a persistence thread that saves values to a CSV file
--
runCsvLog :: Configuration                  -- ^ Global configuration
          -> FiniteChan (Mac, Measurement)  -- ^ Channel to read from
          -> IO ()                          -- ^ Blocks forever
runCsvLog configuration chan = do
    handle <- openFile filePath AppendMode
    runFiniteChan chan () $ \x () -> do
        let (mac, (time, position)) = x
        hPutStrLn handle $ printf "%s,%f,%f" (show mac) time position
    hClose handle
  where
    filePath = configurationCsvLog configuration
