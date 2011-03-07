-- | Saves received values to the disk somewhere
--
module CountVonCount.Persistence
    ( runPersistence
    ) where

import System.IO (openFile, IOMode (AppendMode), hPutStrLn, hClose)
import Text.Printf (printf)

import CountVonCount.FiniteChan
import CountVonCount.Types

-- | Run a persistence thread that saves values to a CSV file
--
runPersistence :: FiniteChan (Team, Measurement)  -- ^ Channel to read from
               -> IO ()                           -- ^ Blocks forever
runPersistence chan = do
    handle <- openFile "measurements.csv" AppendMode 
    runFiniteChan chan () $ \x () -> do
        let (team, (time, position)) = x
        hPutStrLn handle $ printf "%s,%f,%f" team time position
    hClose handle
