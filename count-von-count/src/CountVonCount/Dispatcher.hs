-- | A dispatcher sends events to the right watchers
--
module CountVonCount.Dispatcher
    ( runDispatcher
    ) where

import qualified Data.Map as M
import Data.Monoid (mempty)
import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, execStateT, modify)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO)
import Control.Applicative ((<$>))

import Data.Map (Map)

import CountVonCount.Types
import CountVonCount.Counter
import CountVonCount.FiniteChan

type DispatcherEnvironment = FiniteChan (Timestamp, Team, Score)
type DispatcherState = Map Team (FiniteChan Measurement)

type DispatcherM a = ReaderT DispatcherEnvironment (StateT DispatcherState IO) a

-- | Get the input channel for a team
--
teamChan :: Team -> DispatcherM (FiniteChan Measurement)
teamChan team = do
    mchan <- M.lookup team <$> get 
    case mchan of
        -- Channel found
        Just c  -> return c
        -- Not found, add one
        Nothing -> do
            -- Get channels
            teamChan' <- liftIO $ newFiniteChan team
            outChan <- ask

            -- Create and fork counter
            _ <- liftIO $ forkIO $ do
                runCounter team teamChan' outChan

            -- Add to state and return
            modify $ M.insert team teamChan'
            return teamChan'

-- | Main dispatcher logic
--
dispatcher :: Team -> Measurement -> DispatcherM ()
dispatcher team measurement = do
    -- Get the channel for the team
    teamChan' <- teamChan team

    -- Write the measurement to the team channel
    liftIO $ writeFiniteChan teamChan' measurement

-- | Exposed run method, uses our monad stack internally
--
runDispatcher :: FiniteChan (Team, Measurement)       -- ^ In channel
              -> FiniteChan (Timestamp, Team, Score)  -- ^ Out channel
              -> IO ()                                -- ^ Blocks forever
runDispatcher inChan outChan = do
    finalState <- runFiniteChan inChan mempty $ \(t, m) state ->
        execStateT (runReaderT (dispatcher t m) outChan) state
    forM_ (M.toList finalState) $ \(_, chan) -> do
        endFiniteChan chan
        waitFiniteChan chan
