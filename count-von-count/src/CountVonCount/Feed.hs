{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Feed
    ( FeedEvent (..)
    ) where

import Data.Text (Text)
import Data.Aeson (ToJSON (..), object, (.=))

import CountVonCount.Counter.Core
import CountVonCount.Persistence

data FeedEvent
    = CounterEvent Team CounterEvent
    deriving (Show)

eventType :: FeedEvent -> Text
eventType (CounterEvent _ (Lap _))             = "lap"
eventType (CounterEvent _ (Progression _ _ _)) = "progression"

instance ToJSON FeedEvent where
    toJSON e = object $ ("type" .= eventType e) : case e of
        (CounterEvent team (Lap time))         ->
            ["team" .= team, "time" .= time]
        (CounterEvent team (Progression time station speed)) ->
            [ "team" .= team, "time" .= time
            , "station" .= station, "speed" .= speed
            ]
