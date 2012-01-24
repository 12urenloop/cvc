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

type CounterMap = Map Mac Counter

emptyCounterMap :: CounterMap
emptyCounterMap = M.empty

stepCounterMap :: Double
               -> Mac
               -> SensorEvent
               -> CounterMap
               -> ([CounterEvent], CounterMap)
stepCounterMap circuitLength mac event !cmap =
    let c             = fromMaybe emptyCounter $ M.lookup mac cmap
        (events, !c') = stepCounter circuitLength event c
    in (events, M.insert mac c' cmap)
