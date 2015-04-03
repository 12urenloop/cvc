--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Protocols.Gyrid
    ( gyridInput
    , gyridOutput
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative              ((<$>))
import           Control.Monad                    (guard, liftM)
import           Data.Bits                        (shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as B
import           Data.ByteString.Char8            ()
import qualified Data.ByteString.Char8            as BC
import qualified Data.ByteString.Lazy             as BL
import           Data.Time                        (UTCTime, getCurrentTime)
import qualified Gyrid.Bluetooth_DataRaw          as BDR
import           Gyrid.Msg                        (Msg, type')
import qualified Gyrid.Msg                        as Msg
import           Gyrid.Msg.Type                   (Type (..))
import qualified Gyrid.RequestCaching             as RC
import qualified Gyrid.RequestStartdata           as RSD
import qualified System.IO.Streams                as Streams
import qualified Text.ProtocolBuffers.WireMessage as WM

--------------------------------------------------------------------------------
import           CountVonCount.Log
import           CountVonCount.RawSensorEvent
import           CountVonCount.Types
--------------------------------------------------------------------------------
type Payload = ByteString

--------------------------------------------------------------------------------
gyridOutput :: Streams.OutputStream B.ByteString -> IO ()
gyridOutput outStream = do
    outPayload          <- int16Sender outStream
    outMsgs             <- messageSender outPayload
    Streams.write dataMessage  outMsgs
    Streams.write cacheMessage outMsgs
    return ()

--------------------------------------------------------------------------------
gyridInput :: Log
    -> Streams.InputStream B.ByteString
    -> IO (Streams.InputStream RawSensorEvent)
gyridInput logger inStream = do
    inPayload           <- int16Receiver inStream
    inMsgs              <- messageReceiver logger inPayload
    Streams.mapM mToEvent inMsgs >>= Streams.mapMaybe id
        where mToEvent m = liftM (messageToEvent m) getCurrentTime

--------------------------------------------------------------------------------
mkMsg :: Type -> Msg
mkMsg t = Msg.Msg { type' = t
                    , Msg.requestStartdata = Nothing
                    , Msg.ack = Nothing
                    , Msg.cached = Nothing
                    , Msg.bluetooth_dataIO = Nothing
                    , Msg.bluetooth_dataRaw = Nothing
                    , Msg.bluetooth_stateInquiry = Nothing
                    , Msg.wifi_stateFrequency = Nothing
                    , Msg.wifi_stateFrequencyLoop = Nothing
                    , Msg.wifi_dataRaw = Nothing
                    , Msg.wifi_dataDevRaw = Nothing
                    , Msg.wifi_dataIO = Nothing
                    , Msg.info = Nothing
                    , Msg.stateScanning = Nothing
                    , Msg.stateGyrid = Nothing
                    , Msg.stateAntenna = Nothing
                    , Msg.uptime = Nothing
                    , Msg.requestKeepalive = Nothing
                    , Msg.requestUptime = Nothing
                    , Msg.requestCaching = Nothing
                    , Msg.requestState = Nothing
                    , Msg.hostname = Nothing
                    , Msg.antennaTurn = Nothing
                    , Msg.scanPattern = Nothing
                    , Msg.success = Nothing }

--------------------------------------------------------------------------------
dataMessage :: Maybe Msg
dataMessage = Just $ (mkMsg Type_REQUEST_STARTDATA)
    { Msg.requestStartdata = Just RSD.RequestStartdata
        { RSD.enableData         = Just True
        , RSD.enableBluetoothRaw = Just True
        , RSD.enableWifiRaw      = Just False
        , RSD.enableWifiDevRaw   = Just False
        , RSD.enableSensorMac    = Just True } }

cacheMessage :: Maybe Msg
cacheMessage = Just $ (mkMsg Type_REQUEST_CACHING)
    { Msg.requestCaching = Just RC.RequestCaching
        { RC.enableCaching = Just False
        , RC.clearCache = Just True
        , RC.pushCache = Nothing } }


--------------------------------------------------------------------------------
messageToEvent :: Msg -> UTCTime -> Maybe RawSensorEvent
messageToEvent msg time = do
    guard (type' msg == Type_BLUETOOTH_DATARAW)
    bdr    <- Msg.bluetooth_dataRaw msg
    sensor <- BDR.sensorMac bdr
    baton  <- BDR.hwid bdr
    rssi   <- BDR.rssi bdr
    return $ RawSensorEvent time
        (parseHexMac sensor) (parseHexMac baton) (fromIntegral rssi)


--------------------------------------------------------------------------------
-- | See <http://twistedmatrix.com/documents/8.2.0/api/twisted.protocols.basic.Int16StringReceiver.html>
int16Receiver
    :: Streams.InputStream ByteString
    -> IO (Streams.InputStream Payload)
int16Receiver is = Streams.makeInputStream $ do
    eof <- Streams.atEOF is
    if eof
        then return Nothing
        else do
            header <- Streams.readExactly 2 is
            let h0  = fromIntegral $ header `B.index` 0
                h1  = fromIntegral $ header `B.index` 1
                len = (h0 `shiftL` 8) .|. h1
            Just <$> Streams.readExactly len is


--------------------------------------------------------------------------------
-- | See <http://twistedmatrix.com/documents/8.2.0/api/twisted.protocols.basic.Int16StringReceiver.html>
int16Sender
    :: Streams.OutputStream Payload
    -> IO (Streams.OutputStream ByteString)
int16Sender = Streams.contramap $ \bs ->
    let len = B.length bs
        h0  = fromIntegral $ (len .&. 0xff00) `shiftR` 8
        h1  = fromIntegral $ len .&. 0x00ff
    in  B.pack [h0, h1] `B.append` bs


--------------------------------------------------------------------------------
messageReceiver
    :: Log
    -> Streams.InputStream Payload
    -> IO (Streams.InputStream Msg)
messageReceiver logger is = Streams.makeInputStream readNextMsg
  where
    readNextMsg = do
        mbPayload <- Streams.read is
        case mbPayload of
            Nothing      -> return Nothing
            Just payload -> case WM.messageGet (BL.fromChunks [payload]) of
                Right (msg, "") -> return $ Just msg
                Left  ermsg     -> do
                    string logger "CountVonCount.Sensor.messageReceiver" ermsg
                    readNextMsg
                _               -> readNextMsg

--------------------------------------------------------------------------------
messageSender
    :: Streams.OutputStream Payload
    -> IO (Streams.OutputStream Msg)
messageSender = Streams.contramap $ BC.concat . BL.toChunks . WM.messagePut
