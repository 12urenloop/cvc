-- | A dispatcher sends events to the right watchers
--
module CountVonCount.Dispatcher
    ( runDispatcher
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid (mempty)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, execStateT, modify, runState)
import Control.Monad.Trans (liftIO)
import Control.Concurrent.Chan (Chan, writeChan)
import Control.Applicative ((<$>))

import Data.Map (Map)

import CountVonCount.Types
import CountVonCount.Counter
import CountVonCount.Chan
import CountVonCount.Configuration

data DispatcherEnvironment = DispatcherEnvironment
    { dispatcherChan          :: Chan Report
    , dispatcherConfiguration :: Configuration
    , dispatcherLogger        :: Logger
    }

type DispatcherState = Map Mac CounterState

type DispatcherM a = ReaderT DispatcherEnvironment (StateT DispatcherState IO) a

-- | Main dispatcher logic
--
dispatcherMeasurement :: Mac -> Measurement -> DispatcherM ()
dispatcherMeasurement mac measurement = do
    configuration <- dispatcherConfiguration <$> ask
    logger <- dispatcherLogger <$> ask
    outChan <- dispatcherChan <$> ask

    -- Only do something when we allow the mac address
    when (mac `S.member` configurationMacSet configuration) $ do
        -- Get the counter for the mac address
        state <- fromMaybe emptyCounterState . M.lookup mac <$> get

        -- Run the nice, pure code
        let env = CounterEnvironment configuration mac
            counter' = runReaderT (counter measurement) env
            (report, state') = runState counter' state

        -- Write back the state
        modify $ M.insert mac state'

        -- Check and validate the report
        liftIO $ case report of
            Nothing -> return ()
            Just r  -> if (validateReport r)
                then writeChan outChan r
                else logger Info $
                        "CountVonCount.Dispatcher.dispatcherMeasurement: "
                        ++ "invalid: " ++ show (reportScore r)

-- | Reset a mac address
--
dispatcherReset :: Mac -> DispatcherM ()
dispatcherReset mac = do
    configuration <- dispatcherConfiguration <$> ask
    logger <- dispatcherLogger <$> ask

    when (mac `S.member` configurationMacSet configuration) $ do
        modify $ M.insert mac emptyCounterState
        liftIO $ logger Info $ 
            "CountVonCount.Dispatcher.dispatcherReset: " ++
            "succesfully reset " ++ show mac

-- | Exposed run method, uses our monad stack internally
--
runDispatcher :: Configuration  -- ^ Configuration
              -> Logger         -- ^ Logger
              -> Chan Command   -- ^ In channel
              -> Chan Report    -- ^ Out channel
              -> IO ()          -- ^ Blocks forever
runDispatcher configuration logger inChan outChan = do
    statefulReadChanLoop inChan mempty $ \command state -> case command of
        Measurement (t, m) ->
            execStateT (runReaderT (dispatcherMeasurement t m) env) state
        Reset m ->
            execStateT (runReaderT (dispatcherReset m) env) state
  where
    env = DispatcherEnvironment
        { dispatcherChan = outChan
        , dispatcherConfiguration = configuration
        , dispatcherLogger = logger
        }
