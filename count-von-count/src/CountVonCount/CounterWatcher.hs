module CountVonCount.CounterWatcher
    ( CounterWatcher
    , makeCounterWatcher
    , runCounterWatcher
    ) where

import Control.Concurrent.Chan.Strict (Chan, readChan, writeChan)
import Data.Monoid (mempty)


import CountVonCount.Counter
import CountVonCount.Types

data CounterWatcher = CounterWatcher
    { watcherTeam    :: String
    , watcherInChan  :: Chan Measurement
    , watcherOutChan :: Chan (Team, Lap)
    }

-- | Create a new watcher
--
makeCounterWatcher :: Team               -- ^ Identifier
                   -> Chan Measurement   -- ^ In Channel
                   -> Chan (Team, Lap)   -- ^ Out Channel
                   -> IO CounterWatcher  -- ^ Resulting watcher
makeCounterWatcher identifier inChan outChan = return $
    CounterWatcher identifier inChan outChan

-- | Run a watcher (blocks forever)
--
runCounterWatcher :: CounterWatcher -> IO ()
runCounterWatcher watcher = runCounterWatcher' mempty
  where
    team = watcherTeam watcher
    runCounterWatcher' counter = do
        measurement <- readChan $ watcherInChan watcher
        let (lap, counter') = tick measurement counter
        case lap of Nothing -> return ()
                    Just l  -> writeChan (watcherOutChan watcher) (team, l)
        runCounterWatcher' counter'
