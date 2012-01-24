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
import Text.Printf (printf)

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as A

type Mac = Text

data Station = Station
    { stationName     :: String
    , stationMac      :: Mac
    , stationPosition :: Double
    }

instance Show Station where
    show s = printf "%s (%.0fm)" (stationName s) (stationPosition s)

instance Eq Station where
    s1 == s2 = stationMac s1 == stationMac s2

instance Ord Station where
    l `compare` r = stationMac l `compare` stationMac r

instance ToJSON Station where
    toJSON (Station name mac position) = A.object
        ["name" .= name, "mac" .= mac, "position" .= position]

instance FromJSON Station where
    parseJSON (A.Object o) = Station <$>
        o .: "name" <*> o .: "mac" <*> o .: "position"
    parseJSON _ = mzero

data SensorEvent = SensorEvent
    { sensorTime    :: UTCTime
    , sensorStation :: Station
    , sensorBaton   :: Baton
    } deriving (Show)

data Baton = Baton
    { batonMac  :: Mac
    , batonNr   :: Int
    } deriving (Eq, Show)

instance Ord Baton where
  l `compare` r = batonNr l `compare` batonNr r

instance ToJSON Baton where
    toJSON (Baton mac nr) = A.object ["mac" .= mac, "nr" .= nr]

instance FromJSON Baton where
    parseJSON (A.Object o) = Baton <$> o .: "mac" <*> o .: "nr"
    parseJSON _ = mzero

batonName :: Baton -> String
batonName = ("Baton " ++) . show . batonNr
