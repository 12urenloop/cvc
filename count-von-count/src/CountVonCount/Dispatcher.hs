-- | A dispatcher sends events to the right watchers
--
module CountVonCount.Dispatcher
    ( runDispatcher
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid (mempty)
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, execStateT, modify, runState)
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import Control.Applicative ((<$>))

import Data.Map (Map)

import CountVonCount.Types
import CountVonCount.Counter
import CountVonCount.FiniteChan
import CountVonCount.Configuration

data DispatcherEnvironment = DispatcherEnvironment
    { dispatcherChan          :: FiniteChan Report
    , dispatcherConfiguration :: Configuration
    , dispatcherLogger        :: Logger
    }

type DispatcherState = Map Mac (MVar CounterState)

type DispatcherM a = ReaderT DispatcherEnvironment (StateT DispatcherState IO) a

-- | Get the counter state for a mac
--
macCounter :: Mac -> DispatcherM (MVar CounterState)
macCounter mac = do
    mmvar <- M.lookup mac <$> get
    case mmvar of
        Just m  -> return m
        Nothing -> do
            m <- liftIO $ newMVar emptyCounterState
            modify $ M.insert mac m
            return m

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
        mvar <- macCounter mac

        -- Optionally, this can happen in other thread (using the mvar)
        liftIO $ modifyMVar_ mvar $ \state -> do
            -- Run the nice, pure code
            let env = CounterEnvironment configuration mac
                counter' = runReaderT (counter measurement) env
                (report, state') = runState counter' state

            -- Check and validate the report
            case report of
                Nothing -> return ()
                Just r  -> if (validateReport r)
                    then writeFiniteChan outChan r
                    else logger Info $
                            "CountVonCount.Dispatcher.dispatcherMeasurement: "
                            ++ "invalid: " ++ show (reportScore r)

            return state'

-- | Reset a mac address
--
dispatcherReset :: Mac -> DispatcherM ()
dispatcherReset mac = do
    configuration <- dispatcherConfiguration <$> ask
    logger <- dispatcherLogger <$> ask

    when (mac `S.member` configurationMacSet configuration) $ do
        mvar <- macCounter mac
        liftIO $ do
            modifyMVar_ mvar (return . const emptyCounterState)
            logger Info $  "CountVonCount.Dispatcher.dispatcherReset: "
                        ++ "succesfully reset " ++ show mac

-- | Exposed run method, uses our monad stack internally
--
runDispatcher :: Configuration       -- ^ Configuration
              -> Logger              -- ^ Logger
              -> FiniteChan Command  -- ^ In channel
              -> FiniteChan Report   -- ^ Out channel
              -> IO ()               -- ^ Blocks forever
runDispatcher configuration logger inChan outChan = do
    _ <- runFiniteChan inChan mempty $ \command state -> case command of
            Measurement (t, m) ->
                execStateT (runReaderT (dispatcherMeasurement t m) env) state
            Reset m ->
                execStateT (runReaderT (dispatcherReset m) env) state
    endFiniteChan outChan
  where
    env = DispatcherEnvironment
        { dispatcherChan = outChan
        , dispatcherConfiguration = configuration
        , dispatcherLogger = logger
        }
