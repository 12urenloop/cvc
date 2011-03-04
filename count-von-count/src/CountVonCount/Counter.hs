module CountVonCount.Counter
    ( runCounter
    ) where

import Control.Monad.State (State, get, put, runState)
import Data.Monoid (mempty)
import Control.Applicative ((<$>))

import CountVonCount.Types
import CountVonCount.FiniteChan
import CountVonCount.DataSet
import CountVonCount.Analyzer

data CounterState = CounterState
    { counterDataSet      :: DataSet
    , counterLastPosition :: Maybe Position
    } deriving (Show)

type CounterM = State CounterState

-- | Run a counter
--
counter :: Measurement -> CounterM (Maybe Score)
counter measurement = do
    -- Read a measurement
    let (_, position) = measurement

    -- Obtain state and create a possible next dataset
    dataSet <- counterDataSet <$> get
    lastPosition <- counterLastPosition <$> get
    let dataSet' = addMeasurement measurement dataSet

    -- Check if we have a possible lap
    score <- if maybeLap lastPosition position
                -- Maybe a lap, check for it
                then checkLap measurement
                -- Certainly no lap, save data and return
                else return Nothing
                    
    -- Clear the dataset if we had a score
    case score of
        Nothing -> put $ CounterState dataSet' $ Just position
        _       -> put $ CounterState cleared  $ Just position

    -- Return found score
    return score
  where
    maybeLap Nothing  _                   = False
    maybeLap (Just lastPosition) position = lastPosition > position

    -- A cleared dataset
    cleared = addMeasurement measurement mempty

checkLap :: Measurement -> CounterM (Maybe Score)
checkLap _ = do
    -- Obtain current dataset
    -- TODO: translate last position and add it?
    dataSet <- counterDataSet <$> get

    -- Run analysis
    let score = analyze dataSet

    -- Examine score
    case score of
        Refused _ -> return Nothing
        x         -> return $ Just x

-- | Run a counter
--
runCounter :: Team                                 -- ^ Identifier
           -> FiniteChan Measurement               -- ^ In Channel
           -> FiniteChan (Timestamp, Team, Score)  -- ^ Out Channel
           -> IO ()                                -- ^ Blocks forever
runCounter team inChan outChan = do
    _ <- runFiniteChan inChan initial $ \measurement state -> do
        -- Run the pure counter and optionally send the result
        let (timestamp, _) = measurement
            (result, state') = runState (counter measurement) state
        case result of Nothing -> return ()
                       Just s  -> writeFiniteChan outChan (timestamp, team, s)

        -- Yield the state for the next iteration
        return state'
    return ()
  where
    initial = CounterState mempty Nothing
