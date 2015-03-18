--------------------------------------------------------------------------------
-- | Communication with sensors (i.e. Gyrid)
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module CountVonCount.Sensor
    ( RawSensorEvent (..)
    , listen
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent               (forkIO)
import           Control.Monad                    (forever)
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Char8            ()
import           Data.Time                        (UTCTime)
import           Data.Typeable                    (Typeable)
import           Network                          (PortID (..))
import qualified Network                          as N
import qualified Network.Socket                   as S
import           System.IO.Streams.Network        (socketToStreams)

--------------------------------------------------------------------------------
import           CountVonCount.Protocol
import           CountVonCount.EventBase
import           CountVonCount.Log
import           CountVonCount.Types
import           CountVonCount.Util


--------------------------------------------------------------------------------
data RawSensorEvent = RawSensorEvent
    { rawSensorTime    :: UTCTime
    , rawSensorStation :: Mac
    , rawSensorBaton   :: Mac
    , rawSensorRssi    :: Double
    } deriving (Show, Typeable)


--------------------------------------------------------------------------------
type Payload = ByteString


--------------------------------------------------------------------------------
listen :: Protocol
       -> Log
       -> EventBase
       -> Int
       -> IO ()
listen protocol logger eventBase port = do
    sock <- N.listenOn (PortNumber $ fromIntegral port)

    forever $ do
        (conn, addr)        <- S.accept sock
        (inBytes, outBytes) <- socketToStreams conn

        _ <- forkIO $ isolate_ logger "Sensor send config" $ do
            string logger "CountVonCount.Sensor.listen"
                $ "Socket connected to " ++ show addr
            output protocol logger eventBase outBytes

        _ <- forkIO $ isolate_ logger "Sensor receive" $ do
            input protocol logger eventBase inBytes
            string logger "CountVonCount.Sensor.listen"
                $ "Socket gracefully disconnected (client was " ++ show addr ++ ")"
            S.sClose conn
        return ()
