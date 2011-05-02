-- | A dispatcher sends events to the right watchers
--
module CountVonCount.Dispatcher
    ( DispatcherState
    , emptyDispatcherState
    , resetCounter
    , runDispatcher
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runState)
import Control.Concurrent.Chan (Chan, writeChan)
import Control.Concurrent.MVar (MVar, takeMVar, putMVar)

import Data.Map (Map)

import CountVonCount.Types
import CountVonCount.Counter
import CountVonCount.Chan
import CountVonCount.Configuration

type DispatcherState = Map Mac CounterState

emptyDispatcherState :: DispatcherState
emptyDispatcherState = M.empty

addMeasurement :: Configuration
               -> Mac -> Measurement -> DispatcherState
               -> (Maybe Report, DispatcherState)
addMeasurement conf mac measurement state =
    if mac `S.member` macSet then updated else (Nothing, state)
  where
    macSet = configurationMacSet conf
    cstate = fromMaybe emptyCounterState $ M.lookup mac state
    env = CounterEnvironment conf mac
    counter' = runReaderT (counter measurement) env
    (report, cstate') = runState counter' cstate
    updated = (report, M.insert mac cstate' state)

resetCounter :: Mac -> DispatcherState -> DispatcherState
resetCounter mac = M.insert mac emptyCounterState

-- | Exposed run method, uses our monad stack internally
--
runDispatcher :: Configuration         -- ^ Configuration
              -> Logger                -- ^ Logger
              -> MVar DispatcherState  -- ^ Dispatcher state
              -> Chan Command          -- ^ In channel
              -> Chan Report           -- ^ Out channel
              -> IO ()                 -- ^ Blocks forever
runDispatcher conf logger mstate inChan outChan = do
    readChanLoop inChan $ \command -> case command of
        -- Add a measurement
        Measurement (t, m) -> do
            state <- takeMVar mstate
            let (report, state') = addMeasurement conf t m state

            -- Check and validate the report
            case report of
                Nothing -> return ()
                Just r  -> if (validateReport r)
                    then writeChan outChan r
                    else logger Info $
                            "CountVonCount.Dispatcher.dispatcherMeasurement: "
                            ++ "invalid: " ++ show (reportScore r)

            -- Write back the state
            putMVar mstate state'
