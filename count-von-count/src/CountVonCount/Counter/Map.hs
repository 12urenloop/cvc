-- | Provides an easy structure for multiple counters
{-# LANGUAGE BangPatterns #-}
module CountVonCount.Counter.Map
    ( CounterMap
    , emptyCounterMap
    , stepCounterMap
    , resetCounterMapFor
    , lookupCounterState
    , lastUpdatedBefore
    ) where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Data.Time (UTCTime)

import CountVonCount.Counter.Core
import CountVonCount.Sensor.Filter
import CountVonCount.Types

type CounterMap = Map Baton CounterState

emptyCounterMap :: CounterMap
emptyCounterMap = M.empty

stepCounterMap :: Double
               -> Double
               -> SensorEvent
               -> CounterMap
               -> ([CounterEvent], [String], CounterMap)
stepCounterMap circuitLength maxSpeed event !cmap =
    let state                = lookupCounterState baton cmap
        app                  = stepCounterState circuitLength maxSpeed event
        (es, tells, !state') = runCounterM app state
    in (es, map prepend tells, M.insert baton state' cmap)
  where
    baton       = sensorBaton event
    prepend str = show baton ++ ": " ++ str

-- | Resets the counter state for a single baton
resetCounterMapFor :: Baton
                   -> CounterMap
                   -> CounterMap
resetCounterMapFor = flip M.insert emptyCounterState

lookupCounterState :: Baton
                   -> CounterMap
                   -> CounterState
lookupCounterState baton = fromMaybe emptyCounterState . M.lookup baton

-- | Get a list of batons which were last updated before the given time
lastUpdatedBefore :: UTCTime -> CounterMap -> [Baton]
lastUpdatedBefore time cmap =
    [ baton
    | (baton, cstate) <- M.toList cmap
    , maybe False (< time) (counterLastUpdate cstate)
    ]
