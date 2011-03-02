module CountVonCount.Counter
    ( runCounter
    ) where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Concurrent.Chan.Strict (Chan, readChan, writeChan)
import Data.Monoid (mempty)
import Control.Applicative ((<$>))

import CountVonCount.Types
import CountVonCount.DataSet
import CountVonCount.Analyzer

data CounterEnvironment = CounterEnvironment
    { counterTeam    :: String
    , counterInChan  :: Chan Measurement
    , counterOutChan :: Chan (Team, Score)
    }

data CounterState = CounterState
    { counterDataSet      :: DataSet
    , counterLastPosition :: Maybe Position
    }

type CounterM = ReaderT CounterEnvironment (StateT CounterState IO)

-- | Run a counter
--
counter :: CounterM ()
counter = do
    -- Read a measurement
    inChan <- counterInChan <$> ask
    measurement <- liftIO $ readChan inChan
    let (timestamp, position) = measurement

    -- Obtain state
    CounterState dataSet lastPosition <- get

    -- Add last measurement to dataset
    let dataSet' = addMeasurement measurement dataSet

    -- Analyze the dataset if necessary
    when (trigger lastPosition position) $ do
        -- Get line
        let score = analyze dataSet'

        -- Send out
        team <- counterTeam <$> ask
        outChan <- counterOutChan <$> ask
        liftIO $ writeChan outChan $ (team, score)

    -- Save for next iteration
    put $ CounterState dataSet' $ Just position
  where
    trigger Nothing  _                   = False
    trigger (Just lastPosition) position = lastPosition > position

-- | Run a counter
--
runCounter :: Team                -- ^ Identifier
           -> Chan Measurement    -- ^ In Channel
           -> Chan (Team, Score)  -- ^ Out Channel
           -> IO ()               -- ^ Blocks forever
runCounter team inChan outChan = do
    ((), _) <- runStateT (runReaderT counter env) state
    return ()
  where
    env   = CounterEnvironment team inChan outChan
    state = CounterState mempty Nothing
