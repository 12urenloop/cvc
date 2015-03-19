--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module  CountVonCount.Protocol
    ( RawSensorEvent (..)
    , Protocol (..)
    ) where

--------------------------------------------------------------------------------
import qualified Data.ByteString         as B
import           Data.Time               (UTCTime)
import           System.IO.Streams

--------------------------------------------------------------------------------
import           CountVonCount.Log
import           CountVonCount.Types
import           Data.Typeable           (Typeable)

--------------------------------------------------------------------------------
data RawSensorEvent = RawSensorEvent
    { rawSensorTime    :: UTCTime
    , rawSensorStation :: Mac
    , rawSensorBaton   :: Mac
    , rawSensorRssi    :: Double
    } deriving (Show, Typeable)

data Protocol = Protocol {
    output :: OutputStream B.ByteString -> IO (),
    input  :: Log -> InputStream B.ByteString
                -> IO (InputStream RawSensorEvent)
}

