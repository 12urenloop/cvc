-- | Provides an easy structure for multiple counters
{-# LANGUAGE BangPatterns #-}
module CountVonCount.Counter.Map
    ( CounterMap
    , emptyCounterMap
    , stepCounterMap
    , resetCounterMapFor
    ) where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import CountVonCount.Counter.Core
import CountVonCount.Sensor.Filter
import CountVonCount.Types

type CounterMap = Map Baton CounterState

emptyCounterMap :: CounterMap
emptyCounterMap = M.empty

stepCounterMap :: Double
               -> SensorEvent
               -> CounterMap
               -> ([CounterEvent], CounterMap)
stepCounterMap circuitLength event !cmap =
    let c             = fromMaybe emptyCounterState $ M.lookup baton cmap
        (events, !c') = stepCounterState circuitLength event c
    in (events, M.insert baton c' cmap)
  where
    baton = sensorBaton event

-- | Resets the counter state for a single baton
resetCounterMapFor :: Baton
                   -> CounterMap
                   -> CounterMap
resetCounterMapFor = flip M.insert emptyCounterState
