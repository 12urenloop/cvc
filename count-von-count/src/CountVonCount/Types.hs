{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Types
    ( Mac
    , Station (..)
    , SensorEvent (..)
    , Baton (..)
    , batonName
    ) where

import Data.Time (UTCTime)

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)

type Mac = Text

data Station = Station
    { stationName     :: String
    , stationMac      :: Mac
    , stationPosition :: Double
    }

instance Show Station where
    show s = stationName s ++ " (" ++ (show $ stationPosition s) ++ "m)"

instance Eq Station where
    s1 == s2 = stationMac s1 == stationMac s2

instance ToJSON Station where
    toJSON (Station name mac position) = object
        ["name" .= name, "mac" .= mac, "position" .= position]

data SensorEvent = SensorEvent UTCTime Station
    deriving (Show)

data Baton = Baton
    { batonMac  :: Mac
    , batonNr   :: Int
    } deriving (Eq, Show)

instance ToJSON Baton where
    toJSON (Baton mac nr) = object ["mac" .= mac, "nr" .= nr]

batonName :: Baton -> String
batonName = ("Baton " ++) . show . batonNr
