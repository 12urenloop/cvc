-- | This module is responsible for analyzing and filtering stick events
module CountVonCount.Counter.Core
    ( CounterEvent (..)
    , isLap
    , Counter
    , emptyCounter
    , stepCounter
    ) where

import Data.Time (UTCTime, diffUTCTime)

import CountVonCount.Types

data CounterEvent
    = Progression UTCTime Station Double
    | Lap UTCTime
    deriving (Show)

isLap :: CounterEvent -> Bool
isLap (Lap _) = True
isLap _       = False

data Counter = Counter
    { counterEvents :: [SensorEvent]
    } deriving (Show)

emptyCounter :: Counter
emptyCounter = Counter {counterEvents = []}

stepCounter :: Double
            -> SensorEvent
            -> Counter
            -> ([CounterEvent], Counter)
stepCounter circuitLength event state
    -- First event received
    | null events             =
        ([], Counter [event])
    -- Still at the same station. Do nothing.
    | station == lastStation  =
        ([], state)
    -- Advanced at least one station, update.
    | position > lastPosition =
        ([Progression time station speed], Counter (event : events))
    -- At a lower position. Either a lap was made, or the sensor event was
    -- foobar. For a lap to be made, we consider a minimum number of stations
    -- and a minimum timespan.
    | falseLap                =
        ([], state)
    -- We have an actual lap!
    | otherwise               =
        ([Progression time station lapSpeed, Lap time], Counter [event])
  where
    Counter events                         = state
    SensorEvent time station               = event
    (SensorEvent lastTime lastStation : _) = events
    Station _ _ position                   = station
    Station _ _ lastPosition               = lastStation

    SensorEvent lapStart _ = last events
    lapTime                = time `diffUTCTime` lapStart

    falseLap = length events < minimumStations || lapTime < minimumLapTime

    speed    = (time `diffTime` lastTime) / (position - lastPosition)
    lapSpeed = (time `diffTime` lastTime) /
        (position - lastPosition + circuitLength)

    minimumStations = 2   -- TODO: configurable
    minimumLapTime  = 10  -- TODO: configurable

    diffTime t1 t2 = fromRational $ toRational $ t1 `diffUTCTime` t2
