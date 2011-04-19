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
import Data.Monoid (mempty, mconcat)
import Control.Applicative ((<$>))

import Statistics.LinearRegression (linearRegression)

import CountVonCount.Types
import CountVonCount.DataSet
import CountVonCount.Configuration

data CounterState = CounterState
    { counterDataSet      :: DataSet
    , counterLastPosition :: Maybe Position
    } deriving (Show)

data CounterEnvironment = CounterEnvironment
    { counterConfiguration :: Configuration
    , counterMac           :: Mac
    }

emptyCounterState :: CounterState
emptyCounterState = CounterState mempty Nothing

type CounterM = ReaderT CounterEnvironment (State CounterState)

-- | Run a counter
--
counter :: Measurement -> CounterM (Maybe Report)
counter measurement = do
    -- Read a measurement
    let (timestamp, position) = measurement

    -- Obtain state and create a possible next dataset
    dataSet <- counterDataSet <$> get
    lastPosition <- counterLastPosition <$> get
    configuration <- counterConfiguration <$> ask
    mac <- counterMac <$> ask
    let dataSet' = addMeasurement measurement dataSet

        -- Not always executed
        (!score, line) = analyze configuration dataSet
                    
        -- First do a quick check using maybeLap, then verify it using isLap
        shouldReport = maybeLap lastPosition position
        shouldClear = shouldReport && validateScore score

    -- Clear the dataset if necessary
    if shouldClear then put $ CounterState cleared  $ Just position
                   else put $ CounterState dataSet' $ Just position

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
    maybeLap Nothing  _                   = False
    maybeLap (Just lastPosition) position = lastPosition > position

    -- A cleared dataset
    cleared = addMeasurement measurement mempty

analyze :: Configuration -> DataSet -> (Score, Line)
analyze configuration dataSet =
    let (times, positions) = toSamples dataSet
        line = regression times positions
    in (criteria times positions line, line)
  where
    criteria = mconcat $ configurationCriteria configuration
    regression times positions =
        let (a, b) = linearRegression times positions
        in Line a b
