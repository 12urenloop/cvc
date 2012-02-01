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
import Control.Monad.Trans (liftIO)
import Data.Foldable (forM_)

import CountVonCount.Counter.Core
import CountVonCount.Counter.Map
import CountVonCount.Log (Log)
import CountVonCount.Types
import qualified CountVonCount.Log as Log
import qualified CountVonCount.Persistence as P

counter :: Double
        -> Double
        -> Log
        -> (P.Team -> CounterEvent -> IO ())
        -> Chan SensorEvent
        -> IO ()
counter cl threshold logger handler chan = loop emptyCounterMap
  where
    step'     = step cl threshold logger handler
    loop cmap = do
        event <- readChan chan
        cmap' <- step' event cmap
        loop cmap'

step :: Double
     -> Double
     -> Log
     -> (P.Team -> CounterEvent -> IO ())
     -> SensorEvent
     -> CounterMap
     -> IO CounterMap
step cl threshold logger handler event cmap
    | ignoreBaton baton = return cmap
    | ignoreRssi event  = return cmap
    | otherwise = do
        let (events, cmap') = stepCounterMap cl event cmap
        process events
        return cmap'
  where
    baton       = sensorBaton event
    ignoreBaton = const False  -- TODO

    ignoreRssi event = sensorRssi event < threshold

    process []     = return ()
    process events = P.runPersistence $ do
        mteam <- P.getTeamByMac (batonMac . sensorBaton $ event)
        forM_ mteam $ \(ref, team) ->
            forM_ events $ \event' -> do
                liftIO $ do
                    handler team event'
                    Log.string logger $ case event' of
                        Progression _ s _ -> show team ++ " @ " ++ show s
                        Lap _ _           -> "Lap for " ++ show team

                -- Add the lap in the database
                case event' of
                    Lap timestamp _ -> P.addLap ref timestamp
                    _               -> return ()
