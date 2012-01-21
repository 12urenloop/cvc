-- | This module has a number of responsibilities:
--
-- 1. Receiving sensor events
--
-- 2. Grouping the relevant events by stick mac
--
-- 3. Calling the analyzer to process these events
-- 
-- TODO: Filter mac addresses
module CountVonCount.Counter
    (
    ) where

import Data.Map (Map)
import qualified Data.Map as M

import CountVonCount.Counter.Core
import CountVonCount.Counter.Map
import CountVonCount.Types

counter :: Monad m
        => (Mac -> CounterEvent -> m ())
        -> CounterMap
        -> SensorEvent
        -> m CounterMap
counter handler cmap event = undefined
