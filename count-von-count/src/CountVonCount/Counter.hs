module CountVonCount.Counter
    ( CounterState
    , emptyCounterState
    , CounterM
    , counter
    , runCounter
    ) where

import Control.Monad.State (State, get, put, runState)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Monoid (mempty, mconcat)
import Control.Applicative ((<$>))

import Statistics.LinearRegression (linearRegression)

import CountVonCount.Types
import CountVonCount.FiniteChan
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
        (score, line) = analyze configuration dataSet
                    
        -- First do a quick check using maybeLap, then verify it using isLap
        haveLap = maybeLap lastPosition position && isLap score

    -- Clear the dataset if necessary
    if haveLap then put $ CounterState cleared  $ Just position
               else put $ CounterState dataSet' $ Just position

    -- Return found score
    if not haveLap
        then return Nothing
        else return $ Just Report { reportMac        = mac
                                  , reportTimestamp  = timestamp
                                  , reportScore      = score
                                  , reportDataset    = dataSet
                                  , reportRegression = line
                                  }
  where
    maybeLap Nothing  _                   = False
    maybeLap (Just lastPosition) position = lastPosition > position

    -- Check if a score is a lap
    isLap (Refused _) = False
    isLap _           = True

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

-- | Run a counter
--
runCounter :: Configuration           -- ^ Configuration
           -> Mac                     -- ^ Identifier
           -> FiniteChan Measurement  -- ^ In Channel
           -> FiniteChan Report       -- ^ Out Channel
           -> IO ()                   -- ^ Blocks forever
runCounter configuration mac inChan outChan = do
    _ <- runFiniteChan inChan emptyCounterState $ \measurement state -> do
        -- Run the pure counter and optionally send the result
        let counter' = runReaderT (counter measurement) env
            (report, state') = runState counter' state
        case report of Nothing -> return ()
                       Just r  -> writeFiniteChan outChan r -- (timestamp, mac, s)

        -- Yield the state for the next iteration
        return state'
    return ()
  where
    env = CounterEnvironment
        { counterConfiguration = configuration
        , counterMac           = mac
        }
