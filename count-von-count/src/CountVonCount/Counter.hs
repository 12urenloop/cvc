-- | This module has a number of responsibilities:
--
-- 1. Receiving sensor events
--
-- 2. Grouping the relevant events by stick mac
--
-- 3. Calling the analyzer to process these events
--
module CountVonCount.Counter
    ( counter
    ) where

import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Foldable (forM_)

import CountVonCount.Config
import CountVonCount.Counter.Core
import CountVonCount.Counter.Map
import CountVonCount.Log
import CountVonCount.Persistence
import CountVonCount.Types

counter :: Config  -- TODO: more selective
        -> Log
        -> (Team -> CounterEvent -> IO ())
        -> Chan SensorEvent
        -> IO ()
counter conf logger handler chan = loop emptyCounterMap
  where
    step'     = step conf logger handler
    loop cmap = do
        event <- readChan chan
        cmap' <- step' event cmap
        loop cmap'

step :: Config
     -> Log
     -> (Team -> CounterEvent -> IO ())
     -> SensorEvent
     -> CounterMap
     -> IO CounterMap
step conf logger handler event cmap
    | ignoreBaton baton = return cmap
    | otherwise = do
        let (events, cmap') = stepCounterMap cl event cmap
        process events
        return cmap'
  where
    baton       = sensorBaton event
    cl          = configCircuitLength conf
    ignoreBaton = const False  -- TODO

    process []     = return ()
    process events = runPersistence $ do
        mteam <- getTeamByMac (batonMac . sensorBaton $ event)
        forM_ mteam $ \(ref, team) ->
            forM_ events $ \event' -> do
                liftIO $ do
                    handler team event'
                    logStr logger "Counter" $ "adding lap for " ++ show team
                when (isLap event') $ put ref $ addLap team
