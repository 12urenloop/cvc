{-# LANGUAGE DeriveGeneric #-}

module CountVonCount.Translator.SensorEvent
    ( SensorEvent (..)
    ) where

--------------------------------------------------------------------------------
import CountVonCount.Types
import Data.Time            (UTCTime)
import GHC.Generics         (Generic)
--------------------------------------------------------------------------------

data SensorEvent = SensorEvent
    { sensorTime    :: UTCTime
    , sensorStation :: Mac
    , sensorBaton   :: Mac
    , sensorRssi    :: Double
    } deriving (Show, Generic)
