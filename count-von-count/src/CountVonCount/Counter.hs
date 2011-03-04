module CountVonCount.Counter
    ( runCounter
    ) where

import Control.Monad (when, forever)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, put, execStateT)
import Data.Monoid (mempty)
import Control.Applicative ((<$>))

import CountVonCount.Types
import CountVonCount.FiniteChan
import CountVonCount.DataSet
import CountVonCount.Analyzer

data CounterEnvironment = CounterEnvironment
    { counterTeam    :: String
    , counterInChan  :: FiniteChan Measurement
    , counterOutChan :: FiniteChan (Team, Score)
    }

data CounterState = CounterState
    { counterDataSet      :: DataSet
    , counterLastPosition :: Maybe Position
    } deriving (Show)

type CounterM = ReaderT CounterEnvironment (StateT CounterState IO)

-- | Run a counter
--
counter :: Measurement -> CounterM ()
counter measurement = do
    -- Read a measurement
    let (timestamp, position) = measurement

    -- Obtain state
    CounterState dataSet lastPosition <- get

    -- Add last measurement to dataset. TODO: translate
    let dataSet' = dataSet
    -- let dataSet' = addMeasurement measurement dataSet
    -- Save for next iteration
    put $ CounterState (addMeasurement measurement dataSet') $ Just position

    liftIO $ putStrLn $ show lastPosition ++ " " ++ show position

    -- Analyze the dataset if necessary
    when (trigger lastPosition position) $ do
    -- when True $ do
        -- Get line
        let score = analyze dataSet'

        -- Send out
        team <- counterTeam <$> ask
        outChan <- counterOutChan <$> ask
        liftIO $ writeFiniteChan outChan $ (team, score)

        liftIO $ writeFile (show timestamp ++ ".plot") $ toGnuPlot dataSet'

        -- Clear dataset
        let cleared = addMeasurement measurement mempty
        case score of
            Refused _ -> return ()
            _         -> put $ CounterState cleared $ Just position
  where
    trigger Nothing  _                   = False
    trigger (Just lastPosition) position = lastPosition > position

-- | Run a counter
--
runCounter :: Team                      -- ^ Identifier
           -> FiniteChan Measurement    -- ^ In Channel
           -> FiniteChan (Team, Score)  -- ^ Out Channel
           -> IO ()                     -- ^ Blocks forever
runCounter team inChan outChan = do
    _ <- runFiniteChan inChan state $ \measurement state' -> do
        execStateT (runReaderT (counter measurement) env) state'
    return ()
  where
    env   = CounterEnvironment team inChan outChan
    state = CounterState mempty Nothing
