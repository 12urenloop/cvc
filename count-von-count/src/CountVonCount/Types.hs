module CountVonCount.Types
    ( Mac
    , Station (..)
    , SensorEvent (..)
    , Baton (..)
    , batonName
    ) where

import Data.Time (UTCTime)

import qualified Data.ByteString as B

type Mac = B.ByteString

data Station = Station
    { stationMac      :: Mac
    , stationPosition :: Double
    } deriving (Show)

instance Eq Station where
    s1 == s2 = stationMac s1 == stationMac s2

data SensorEvent = SensorEvent UTCTime Station
    deriving (Show)

data Baton = Baton
    { batonMac  :: Mac
    , batonNr   :: Int
    } deriving (Eq, Show)

batonName :: Baton -> String
batonName = ("Baton " ++) . show . batonNr
