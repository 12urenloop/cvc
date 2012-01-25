{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Types
    ( Mac
    , Station (..)
    , SensorEvent (..)
    , toReplay
    , Baton (..)
    , batonName
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)
import Data.List (intercalate)
import Data.Ord (comparing)
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import Data.Text (Text)
import Data.Time (UTCTime, formatTime)
import qualified Data.Aeson as A
import qualified Data.Text as T

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
    compare = comparing stationPosition

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

-- | Format a 'SensorEvent' in order to be readable by the replay log
toReplay :: SensorEvent -> String
toReplay event = intercalate ","
    [ "REPLAY"
    , formatTime defaultTimeLocale "%s" (sensorTime event)
    , T.unpack $ stationMac (sensorStation event)
    , T.unpack $ batonMac (sensorBaton event)
    ]

data Baton = Baton
    { batonMac  :: Mac
    , batonNr   :: Int
    } deriving (Eq, Show)

instance Ord Baton where
    compare = comparing batonNr

instance ToJSON Baton where
    toJSON (Baton mac nr) = A.object ["mac" .= mac, "nr" .= nr]

instance FromJSON Baton where
    parseJSON (A.Object o) = Baton <$> o .: "mac" <*> o .: "nr"
    parseJSON _ = mzero

batonName :: Baton -> String
batonName = ("Baton " ++) . show . batonNr
