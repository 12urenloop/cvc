{-# LANGUAGE BangPatterns #-}
module CountVonCount.Counter
    ( CounterState (..)
    , CounterEnvironment (..)
    , emptyCounterState
    , CounterM
    , counter
    ) where

import Control.Monad.State (State, get, put)
import Control.Monad.Reader (ReaderT, ask)
import Data.Monoid (mconcat)
import Control.Applicative ((<$>))

import Statistics.LinearRegression (linearRegression)

import CountVonCount.Types
import CountVonCount.DataSet
import CountVonCount.Configuration

data CounterState = CounterState
    { counterDataSet      :: DataSet
    } deriving (Show)

data CounterEnvironment = CounterEnvironment
    { counterConfiguration :: Configuration
    , counterMac           :: Mac
    }

emptyCounterState :: CounterState
emptyCounterState = CounterState emptyDataSet

type CounterM = ReaderT CounterEnvironment (State CounterState)

-- | Run a counter
--
counter :: Measurement -> CounterM (Maybe Report)
counter measurement = do
    -- Read a measurement
    let (timestamp, position) = measurement

    -- Obtain state and create a possible next dataset
    dataSet <- counterDataSet <$> get
    configuration <- counterConfiguration <$> ask
    mac <- counterMac <$> ask
    let dataSet' = addMeasurement measurement dataSet

        -- Not always executed
        (!score, line) = analyze configuration measurement dataSet
                    
        -- First do a quick check using maybeLap, then verify it using isLap
        shouldReport = maybeLap (dataMaxPositon dataSet) position
        shouldClear = shouldReport && validateScore score

    -- Clear the dataset if necessary
    if shouldClear then put $ CounterState cleared
                   else put $ CounterState dataSet'

    -- Return found score
    return $ if not shouldReport
        then Nothing
        else Just Report { reportMac        = mac
                         , reportTimestamp  = timestamp
                         , reportScore      = score
                         , reportDataset    = dataSet
                         , reportRegression = line
                         }
  where
    maybeLap Nothing   _        = False
    maybeLap (Just mp) position = mp > position

    -- A cleared dataset
    cleared = addMeasurement measurement emptyDataSet

analyze :: Configuration -> Measurement -> DataSet -> (Score, Line)
analyze configuration measurement dataSet =
    let (times, positions) = toSamples dataSet
        line = regression times positions
    in (criteria measurement times positions line, line)
  where
    criteria = mconcat $ configurationCriteria configuration
    regression times positions =
        let (a, b) = linearRegression times positions
        in Line a b
