-- | A dispatcher sends events to the right watchers
--
module CountVonCount.Dispatcher
    ( runDispatcher
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid (mempty)
import Control.Monad (forM_, when)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, execStateT, modify)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO)
import Control.Applicative ((<$>))

import Data.Map (Map)

import CountVonCount.Types
import CountVonCount.Counter
import CountVonCount.FiniteChan
import CountVonCount.Configuration

data DispatcherEnvironment = DispatcherEnvironment
    { dispatcherChan          :: FiniteChan (Timestamp, Mac, Score)
    , dispatcherConfiguration :: Configuration
    }

type DispatcherState = Map Mac (FiniteChan Measurement)

type DispatcherM a = ReaderT DispatcherEnvironment (StateT DispatcherState IO) a

-- | Get the input channel for a mac
--
macChan :: Mac -> DispatcherM (FiniteChan Measurement)
macChan mac = do
    mchan <- M.lookup mac <$> get 
    case mchan of
        -- Channel found
        Just c  -> return c
        -- Not found, add one
        Nothing -> do
            -- Get channels
            macChan' <- liftIO $ newFiniteChan mac
            outChan <- dispatcherChan <$> ask
            configuration <- dispatcherConfiguration <$> ask

            -- Create and fork counter
            _ <- liftIO $ forkIO $ do
                runCounter configuration mac macChan' outChan

            -- Add to state and return
            modify $ M.insert mac macChan'
            return macChan'

-- | Main dispatcher logic
--
dispatcher :: Mac -> Measurement -> DispatcherM ()
dispatcher mac measurement = do
    -- Set of allowed mac addresses
    macSet <- configurationMacSet . dispatcherConfiguration <$> ask

    -- Only do something when we allow the mac address
    when (mac `S.member` macSet) $ do
        -- Get the channel for the mac
        macChan' <- macChan mac

        -- Write the measurement to the mac channel
        liftIO $ writeFiniteChan macChan' measurement

-- | Exposed run method, uses our monad stack internally
--
runDispatcher :: Configuration                       -- ^ Configuration
              -> FiniteChan (Mac, Measurement)       -- ^ In channel
              -> FiniteChan (Timestamp, Mac, Score)  -- ^ Out channel
              -> IO ()                               -- ^ Blocks forever
runDispatcher configuration inChan outChan = do
    finalState <- runFiniteChan inChan mempty $ \(t, m) state ->
        execStateT (runReaderT (dispatcher t m) env) state
    forM_ (M.toList finalState) $ \(_, chan) -> do
        endFiniteChan chan
        waitFiniteChan chan
  where
    env = DispatcherEnvironment outChan configuration
