-- | A dispatcher sends events to the right watchers
--
module CountVonCount.Dispatcher
    ( Dispatcher
    , makeDispatcher
    , runDispatcher
    ) where

import qualified Data.Map as M
import Data.Monoid (mempty)

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Strict

import CountVonCount.Types
import CountVonCount.CounterWatcher

data Dispatcher = Dispatcher
    { dispatcherInChan  :: Chan (Team, Measurement)
    , dispatcherOutChan :: Chan (Team, Lap)
    }

makeDispatcher :: Chan (Team, Measurement)  -- ^ In channel
               -> Chan (Team, Lap)          -- ^ Out channel
               -> IO Dispatcher             -- ^ New dispatcher
makeDispatcher inChan outChan = return $ Dispatcher inChan outChan

runDispatcher :: Dispatcher  -- ^ Dispatcher
              -> IO ()       -- ^ Blocks forever
runDispatcher dispatcher = runDispatcher' mempty
  where
    addWatcher map' team = do
        toWatcher <- newChan
        watcher <- makeCounterWatcher team toWatcher $
            dispatcherOutChan dispatcher
        _ <- forkIO $ runCounterWatcher watcher
        return $ (toWatcher, M.insert team toWatcher map')

    runDispatcher' map' = do
        (team, measurement) <- readChan $ dispatcherInChan dispatcher
        (chan, map'') <- case M.lookup team map' of
            Nothing -> addWatcher map' team
            Just c  -> return (c, map')
        writeChan chan measurement
        runDispatcher' map''
