--------------------------------------------------------------------------------
-- | Communication with sensors (i.e. Gyrid)
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module CountVonCount.Sensor
    ( RawSensorEvent (..)
    , listen
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent               (forkIO)
import           Control.Monad                    (forever, liftM)
import qualified Data.Attoparsec                  as A
import           Data.ByteString.Char8            ()
import qualified Data.ByteString.Char8            as BC
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (ord)
import           Data.Foldable                    (forM_)
import           Data.Time                        (UTCTime, getCurrentTime)
import           Data.Typeable                    (Typeable)
import qualified Gyrid.Bluetooth_DataRaw          as BDR
import           Gyrid.Msg                        (Msg, type')
import qualified Gyrid.Msg                        as M
import           Gyrid.Msg.Type                   (Type (..))
import           Network                          (PortID (..))
import qualified Network                          as N
import qualified Network.Socket                   as S
import qualified System.IO.Streams.Attoparsec     as SA
import qualified System.IO.Streams.ByteString     as SB
import qualified System.IO.Streams.Combinators    as SC
import           System.IO.Streams.List           as SL
import           System.IO.Streams.Network        (socketToStreams)
import qualified Text.ProtocolBuffers.WireMessage as WM


--------------------------------------------------------------------------------
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
listen :: Log
       -> EventBase
       -> Int
       -> IO ()
listen logger eventBase port = do
    sock <- N.listenOn (PortNumber $ fromIntegral port)

    forever $ do
        (conn, _) <- S.accept sock
        (incoming, _) <- socketToStreams conn
        _ <- forkIO $ isolate_ logger "Sensor send config" $ do
            --S.sendAll conn "MSG,enable_rssi,true\r\n"
            --S.sendAll conn "MSG,enable_cache,false\r\n"
            return ()
        _ <- forkIO $ isolate_ logger "Sensor receive" $ do
            (incoming', count) <- SB.countInput incoming
            messageStream <- SA.parserToInputStream message incoming'
            _ <- SC.mapM_ (handleMessage logger eventBase) messageStream
            --listy <- SL.toList messageStream
            --string logger "CountVonCount.Sensor.listen" (show listy)
            count' <- count
            string logger "CountVonCount.Sensor.listen" (show count')
            string logger "CountVonCount.Sensor.listen" "Socket gracefully disconnected"
            S.sClose conn
        return ()

--------------------------------------------------------------------------------
handleMessage :: Log
              -> EventBase
              -> Msg
              -> IO ()
handleMessage logger eventBase msg
  | type' msg == Type_BLUETOOTH_DATARAW = do
        string logger "CountVonCount.Sensor.handleMessage" (show $ type' msg)
        time <- getCurrentTime
        let mSensorEvent = messageToEvent time msg
        forM_ mSensorEvent $ publish eventBase
  | otherwise = do
        string logger "CountVonCount.Sensor.handleMessage" (show $ type' msg)
  where
    parseMac' = parseMac . BC.concat . BL.toChunks
    messageToEvent :: UTCTime -> Msg -> Maybe RawSensorEvent
    messageToEvent time msg' = do
        bdr    <- M.bluetooth_dataRaw msg'
        sensor <- BDR.hwid bdr
        baton  <- BDR.sensorMac bdr
        rssi   <- BDR.rssi bdr
        return $ RawSensorEvent time (parseMac' sensor) (parseMac' baton) (fromIntegral rssi)

--------------------------------------------------------------------------------
message :: A.Parser (Maybe Msg)
message = do
    l     <- lengthParser
    bytes <- A.take l
    case WM.messageGet (BL.fromChunks [bytes]) of
         Left _         -> return Nothing
         Right (msg, _) -> return $ Just msg
    --return $ Just l
  where
    lengthParser = BC.foldl (\a c -> 8*a + ord c) 0 `liftM` A.take 2

