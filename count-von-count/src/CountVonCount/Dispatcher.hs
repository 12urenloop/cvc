-- | A dispatcher sends events to the right watchers
--
module CountVonCount.Dispatcher
    ( Dispatcher
    , makeDispatcher
    , runDispatcher
    ) where

import qualified Data.Map as M
import Data.Monoid (mempty)
import Control.Monad (forever, forM_)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, execStateT, modify)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO)
import Control.Applicative ((<$>))

import Data.Map (Map)

import CountVonCount.Types
import CountVonCount.Counter
import CountVonCount.FiniteChan

data Dispatcher = Dispatcher
    { dispatcherInChan  :: FiniteChan (Team, Measurement)
    , dispatcherOutChan :: FiniteChan (Team, Score)
    }

type DispatcherState = Map Team (FiniteChan Measurement)

type DispatcherM a = ReaderT Dispatcher (StateT DispatcherState IO) a

makeDispatcher :: FiniteChan (Team, Measurement)  -- ^ In channel
               -> FiniteChan (Team, Score)        -- ^ Out channel
               -> IO Dispatcher                   -- ^ New dispatcher
makeDispatcher inChan outChan = return $ Dispatcher inChan outChan

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
            outChan <- dispatcherOutChan <$> ask

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
runDispatcher :: Dispatcher  -- ^ Dispatcher
              -> IO ()       -- ^ Blocks forever
runDispatcher d = do
    finalState <- runFiniteChan (dispatcherInChan d) mempty $ \(t, m) state ->
        execStateT (runReaderT (dispatcher t m) d) state
    forM_ (M.toList finalState) $ \(_, chan) -> do
        putStrLn "blah"
        endFiniteChan chan
        waitFiniteChan chan
