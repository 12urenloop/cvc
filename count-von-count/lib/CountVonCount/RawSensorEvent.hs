--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module CountVonCount.RawSensorEvent
    ( RawSensorEvent (..)
    ) where

--------------------------------------------------------------------------------
import           CountVonCount.Types
import           Data.Time           (UTCTime)
import           Data.Typeable       (Typeable)
--------------------------------------------------------------------------------
data RawSensorEvent = RawSensorEvent
    { rawSensorTime    :: UTCTime
    , rawSensorStation :: Mac
    , rawSensorBaton   :: Mac
    , rawSensorRssi    :: Double
    } deriving (Show, Typeable)

