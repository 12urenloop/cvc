{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Feed
    ( FeedEvent (..)
    ) where

import Data.Text (Text)
import Data.Aeson (ToJSON (..), object, (.=))

import CountVonCount.Counter.Core
import CountVonCount.Persistence (Team (..))
import CountVonCount.Types

data FeedEvent
    = CounterEvent Team CounterEvent
    | BatonWatchdog [Baton]
    deriving (Show)

eventType :: FeedEvent -> Text
eventType (CounterEvent _ (Lap _ _))           = "lap"
eventType (CounterEvent _ (Progression _ _ _)) = "progression"
eventType (BatonWatchdog _)                    = "batons"

instance ToJSON FeedEvent where
    toJSON e = object $ ("type" .= eventType e) : case e of
        (CounterEvent team (Lap time lapTime))         ->
            ["team" .= team, "time" .= time, "lapTime" .= lapTime]
        (CounterEvent team (Progression time station speed)) ->
            [ "team" .= team, "time" .= time
            , "station" .= station, "speed" .= speed
            ]
        (BatonWatchdog dead) -> ["dead" .= dead]
