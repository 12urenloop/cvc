-- | Provides an easy structure for multiple counters
{-# LANGUAGE BangPatterns #-}
module CountVonCount.Counter.Map
    ( CounterMap
    , emptyCounterMap
    , stepCounterMap
    ) where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import CountVonCount.Counter.Core
import CountVonCount.Types

type CounterMap = Map Baton Counter

emptyCounterMap :: CounterMap
emptyCounterMap = M.empty

stepCounterMap :: Double
               -> SensorEvent
               -> CounterMap
               -> ([CounterEvent], CounterMap)
stepCounterMap circuitLength event !cmap =
    let c             = fromMaybe emptyCounter $ M.lookup baton cmap
        (events, !c') = stepCounter circuitLength event c
    in (events, M.insert baton c' cmap)
  where
    baton = sensorBaton event
