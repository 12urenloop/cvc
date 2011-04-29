-- | Saves received values to the disk somewhere
--
module CountVonCount.CsvLog
    ( runCsvLog
    ) where

import Control.Monad (when)
import System.IO (openFile, IOMode (AppendMode), hPutStrLn, hClose)
import Text.Printf (printf)

import qualified Data.Set as S

import CountVonCount.FiniteChan
import CountVonCount.Types
import CountVonCount.Configuration

-- | Run a persistence thread that saves values to a CSV file
--
runCsvLog :: Configuration       -- ^ Global configuration
          -> FiniteChan Command  -- ^ Channel to read from
          -> IO ()               -- ^ Blocks forever
runCsvLog configuration chan = do
    handle <- openFile filePath AppendMode
    runFiniteChan chan () $ \x () -> case x of
        Measurement (mac, (time, position)) ->
            when (mac `S.member` configurationMacSet configuration) $
                hPutStrLn handle $ printf "%s,%f,%f" (show mac) time position
        _ -> return ()
    hClose handle
  where
    filePath = configurationCsvLog configuration
