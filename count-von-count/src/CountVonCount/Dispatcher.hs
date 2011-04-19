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
dispatcher :: Mac -> Measurement -> DispatcherM ()
dispatcher mac measurement = do
    configuration <- dispatcherConfiguration <$> ask
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
                Just r  -> when (validateReport r) $ writeFiniteChan outChan r

            return state'

-- | Exposed run method, uses our monad stack internally
--
runDispatcher :: Configuration                  -- ^ Configuration
              -> Logger                         -- ^ Logger
              -> FiniteChan (Mac, Measurement)  -- ^ In channel
              -> FiniteChan Report              -- ^ Out channel
              -> IO ()                          -- ^ Blocks forever
runDispatcher configuration logger inChan outChan = do
    _ <- runFiniteChan inChan mempty $ \(t, m) state ->
        execStateT (runReaderT (dispatcher t m) env) state
    endFiniteChan outChan
  where
    env = DispatcherEnvironment
        { dispatcherChan = outChan
        , dispatcherConfiguration = configuration
        , dispatcherLogger = logger
        }
