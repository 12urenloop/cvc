{-# LANGUAGE OverloadedStrings #-}

module SensorEvent
    ( SensorEvent (..)
    ) where


--------------------------------------------------------------------------------
import Data.Aeson
import Data.Time            (UTCTime)


--------------------------------------------------------------------------------
import Types


--------------------------------------------------------------------------------
data SensorEvent = SensorEvent
    { sensorTime    :: UTCTime
    , sensorStation :: Mac
    , sensorBaton   :: Mac
    } deriving (Show)


--------------------------------------------------------------------------------
instance ToJSON SensorEvent where
    toJSON (SensorEvent time station baton) = object
        [ "timestamp" .= time
        , "station"   .= station
        , "baton"     .= baton
        ]
