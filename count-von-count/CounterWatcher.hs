module CounterWatcher where

import Control.Concurrent.Chan.Strict (Chan, readChan, writeChan)
import Data.Monoid (mempty)

import Control.Applicative ((<$>), (<*>), pure)

import Counter
import Types

data CounterWatcher = CounterWatcher
    { watcherIdentifier :: String
    , watcherInChan     :: Chan Measurement
    , watcherOutChan    :: Chan Lap
    }

-- | Create a new watcher
--
makeCounterWatcher :: String             -- ^ Identifier
                   -> Chan Measurement   -- ^ In Channel
                   -> Chan Lap           -- ^ Out Channel
                   -> IO CounterWatcher  -- ^ Resulting watcher
makeCounterWatcher identifier inChan outChan = return $
    CounterWatcher identifier inChan outChan

-- | Run a watcher (blocks forever)
--
runCounterWatcher :: CounterWatcher -> IO ()
runCounterWatcher watcher = runCounterWatcher' mempty
  where
    runCounterWatcher' counter = do
        measurement <- readChan $ watcherInChan watcher
        let (lap, counter') = tick measurement counter
        case lap of Nothing -> return ()
                    Just l  -> writeChan (watcherOutChan watcher) l
        runCounterWatcher' counter'
