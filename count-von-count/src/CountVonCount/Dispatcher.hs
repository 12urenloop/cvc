-- | A dispatcher sends events to the right watchers
--
module CountVonCount.Dispatcher
    ( Dispatcher
    , makeDispatcher
    , runDispatcher
    ) where

import qualified Data.Map as M
import Data.Monoid (mempty)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, evalStateT, modify)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Strict (Chan, writeChan, readChan, newChan)
import Control.Applicative ((<$>))

import Data.Map (Map)

import CountVonCount.Types
import CountVonCount.Counter

data Dispatcher = Dispatcher
    { dispatcherInChan  :: Chan (Team, Measurement)
    , dispatcherOutChan :: Chan (Team, Score)
    }

type DispatcherState = Map Team (Chan Measurement)

type DispatcherM a = ReaderT Dispatcher (StateT DispatcherState IO) a

makeDispatcher :: Chan (Team, Measurement)  -- ^ In channel
               -> Chan (Team, Score)        -- ^ Out channel
               -> IO Dispatcher             -- ^ New dispatcher
makeDispatcher inChan outChan = return $ Dispatcher inChan outChan

-- | Get the input channel for a team
--
teamChan :: Team -> DispatcherM (Chan Measurement)
teamChan team = do
    mchan <- M.lookup team <$> get 
    case mchan of
        -- Channel found
        Just c  -> return c
        -- Not found, add one
        Nothing -> do
            -- Get channels
            teamChan' <- liftIO newChan
            outChan <- dispatcherOutChan <$> ask

            -- Create and fork counter
            _ <- liftIO $ forkIO $ runCounter team teamChan' outChan

            -- Add to state and return
            modify $ M.insert team teamChan'
            return teamChan'

-- | Main dispatcher logic
--
dispatcher :: DispatcherM ()
dispatcher = forever $ do
    -- Get the in channel and ask an element
    inChan <- dispatcherInChan <$> ask
    (team, measurement) <- liftIO $ readChan inChan

    -- Get the channel for the team
    teamChan' <- teamChan team

    -- Write the measurement to the team channel
    liftIO $ writeChan teamChan' measurement

-- | Exposed run method, uses our monad stack internally
--
runDispatcher :: Dispatcher  -- ^ Dispatcher
              -> IO ()       -- ^ Blocks forever
runDispatcher d = evalStateT (runReaderT dispatcher d) mempty
