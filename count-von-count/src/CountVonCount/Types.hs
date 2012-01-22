module CountVonCount.Types
    ( Mac
    , Station (..)
    , SensorEvent (..)
    ) where

import Data.Time (UTCTime)

import qualified Data.ByteString as B

type Mac = B.ByteString

data Station = Station
    { stationMac      :: B.ByteString
    , stationPosition :: Double
    } deriving (Show)

instance Eq Station where
    s1 == s2 = stationMac s1 == stationMac s2

data SensorEvent = SensorEvent UTCTime Station
    deriving (Show)
