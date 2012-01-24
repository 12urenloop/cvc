{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Types
    ( Mac
    , Station (..)
    , SensorEvent (..)
    , Baton (..)
    , batonName
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Yaml

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

instance FromJSON Station where
    parseJSON (Object o) = Station <$>
                           o .: "name" <*>
                           o .: "mac" <*>
                           o .: "position"
    parseJSON _ = mzero



data SensorEvent = SensorEvent UTCTime Station
    deriving (Show)

data Baton = Baton
    { batonMac  :: Mac
    , batonNr   :: Int
    } deriving (Eq, Show)

instance ToJSON Baton where
    toJSON (Baton mac nr) = object ["mac" .= mac, "nr" .= nr]

instance FromJSON Baton where
    parseJSON (Object o) = Baton <$>
                           o .: "mac" <*>
                           o .: "nr"
    parseJSON _ = mzero

batonName :: Baton -> String
batonName = ("Baton " ++) . show . batonNr
