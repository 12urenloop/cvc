-- | Saves received values to the disk somewhere
--
module CountVonCount.Persistence
    ( runPersistence
    ) where

import Control.Monad (forever)
import System.IO (openFile, IOMode (AppendMode), hPutStrLn)
import Text.Printf (printf)

import Control.Concurrent.Chan.Strict (Chan, readChan)

import CountVonCount.Types

-- | Run a persistence thread that saves values to a CSV file
--
runPersistence :: Chan (Team, Measurement)  -- ^ Channel to read from
               -> IO ()                     -- ^ Blocks forever
runPersistence chan = do
    handle <- openFile "measurements.csv" AppendMode 
    forever $ do
        (team, (time, position)) <- readChan chan
        hPutStrLn handle $ printf "%s,%f,%f" team time position
