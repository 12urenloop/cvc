-- | This module is responsible for analyzing and filtering stick events
module CountVonCount.Analyze
    ( Station (..)
    , AnalyzerEvent (..)
    , emptyAnalyzerState
    , stepAnalyzer
    ) where

import Data.Time (UTCTime, diffUTCTime)

import CountVonCount.Types

data AnalyzerEvent
    = Progression UTCTime Station Double
    | Lap UTCTime
    deriving (Show)

data AnalyzerState = AnalyzerState
    { analyzerEvents :: [SensorEvent]
    } deriving (Show)

emptyAnalyzerState :: AnalyzerState
emptyAnalyzerState = AnalyzerState {analyzerEvents = []}

stepAnalyzer :: SensorEvent
             -> AnalyzerState
             -> ([AnalyzerEvent], AnalyzerState)
stepAnalyzer event state
    -- First event received
    | null events             =
        ([], AnalyzerState [event])
    -- Still at the same station. Do nothing.
    | station == lastStation  =
        ([], state)
    -- Advanced at least one station, update.
    | position > lastPosition =
        ([Progression time station speed], AnalyzerState (event : events))
    -- At a lower position. Either a lap was made, or the sensor event was
    -- foobar. For a lap to be made, we consider a minimum number of stations
    -- and a minimum timespan.
    | falseLap                =
        ([], state)
    -- We have an actual lap!
    -- TODO: Return progression event as well
    | otherwise               =
        ([Lap time], AnalyzerState [event])
  where
    AnalyzerState events                   = state
    SensorEvent time station               = event
    (SensorEvent lastTime lastStation : _) = events
    Station _ position                     = station
    Station _ lastPosition                 = lastStation

    SensorEvent lapStart _ = last events
    lapTime                = time `diffUTCTime` lapStart

    falseLap = length events < minimumStations || lapTime < minimumLapTime

    speed = (time `diffTime` lastTime) / (position - lastPosition)

    minimumStations = 2   -- TODO: configurable
    minimumLapTime  = 30  -- TODO: configurable

    diffTime t1 t2 = fromRational $ toRational $ t2 `diffUTCTime` t1
