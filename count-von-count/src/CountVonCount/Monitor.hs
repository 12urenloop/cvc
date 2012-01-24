{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Monitor
    ( MonitorEvent (..)
    ) where

import Data.Text (Text)
import Data.Aeson (ToJSON (..), object, (.=))

import CountVonCount.Counter.Core
import CountVonCount.Persistence

data MonitorEvent
    = CounterEvent Team CounterEvent
    deriving (Show)

eventType :: MonitorEvent -> Text
eventType (CounterEvent _ (Lap _))             = "lap"
eventType (CounterEvent _ (Progression _ _ _)) = "progression"

instance ToJSON MonitorEvent where
    toJSON e = object $ ("type" .= eventType e) : case e of
        (CounterEvent team (Lap time))         ->
            ["team" .= team, "time" .= time]
        (CounterEvent team (Progression time station speed)) ->
            [ "team" .= team, "speed" .= time
            , "station" .= station, "speed" .= speed
            ]
