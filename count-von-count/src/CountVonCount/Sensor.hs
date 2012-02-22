-- | Communication with sensors (i.e. Gyrid)
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module CountVonCount.Sensor
    ( RawSensorEvent (..)
    , toReplay
    , listen
    ) where

import Control.Applicative ((*>))
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Foldable (forM_)
import Data.List (intercalate)
import Data.Monoid (mappend)
import System.Locale (defaultTimeLocale)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.Enumerator (Iteratee, ($$), (=$))
import Data.Time (UTCTime, formatTime, getCurrentTime, parseTime)
import Network (PortID(..))
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Enumerator as AE
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network as N
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S
import qualified Network.Socket.Enumerator as SE

import CountVonCount.Log
import CountVonCount.Types
import CountVonCount.Util

data RawSensorEvent = RawSensorEvent
    { rawSensorTime    :: UTCTime
    , rawSensorStation :: Mac
    , rawSensorBaton   :: Mac
    , rawSensorRssi    :: Double
    } deriving (Show)

-- | Format a 'SensorEvent' in order to be readable by the replay log
toReplay :: RawSensorEvent -> String
toReplay (RawSensorEvent time station baton rssi) = intercalate ","
    [ "REPLAY"
    , formatTime defaultTimeLocale "%s" time
    , T.unpack station
    , T.unpack baton
    , show rssi
    ]

listen :: Log
       -> Int
       -> (RawSensorEvent -> IO ())
       -> IO ()
listen logger port handler = do
    sock <- N.listenOn (PortNumber $ fromIntegral port)

    forever $ do
        (conn, _) <- S.accept sock
        _ <- forkIO $ isolate logger "Sensor send config" $ do
            S.sendAll conn "MSG,enable_rssi,true\r\n"
            S.sendAll conn "MSG,enable_cache,false\r\n"
        _ <- forkIO $ isolate logger "Sensor receive" $ do
            E.run_ $ SE.enumSocket 256 conn $$
                E.sequence (AE.iterParser gyrid) =$ receive logger handler
            S.sClose conn
        return ()

receive :: Log
        -> (RawSensorEvent -> IO ())
        -> Iteratee Gyrid IO ()
receive logger handler = do
    g <- EL.head
    case g of
        Nothing    -> liftIO $ string logger "Socket gracefully disconnected"
        Just event -> do
            time <- liftIO getCurrentTime
            let sensorEvent = case event of
                    Replay t s b r -> Just $ RawSensorEvent t s b r
                    Event s b r    -> Just $ RawSensorEvent time s b r
                    Ignored        -> Nothing
            forM_ sensorEvent $
                liftIO . isolate logger "Sensor handler" . handler
            receive logger handler

data Gyrid
    = Event Mac Mac Double
    | Replay UTCTime Mac Mac Double
    | Ignored
    deriving (Show)

gyrid :: A.Parser Gyrid
gyrid = do
    line <- lineParser
    return $ case BC.split ',' line of
        ("MSG" : _)                -> Ignored
        ("INFO" : _)               -> Ignored
        ["REPLAY", !t, !s, !b, !r] ->
            case parseTime defaultTimeLocale "%s" (BC.unpack t) of
                Just t' -> Replay t' (parseMac s) (parseMac b) (toDouble r)
                _       -> Ignored
        [!s, _, !b, !r]            ->
            Event (parseMac s) (parseMac b) (toDouble r)
        _                                 -> Ignored
  where
    newline x  = x `B.elem` "\r\n"
    lineParser = A.skipWhile newline *> A.takeWhile (not . newline)

    toDouble = read . BC.unpack

-- | Transform a mac without @:@ delimiters to one a mac with @:@ delimiters
parseMac :: ByteString -> Mac
parseMac bs = T.decodeUtf8 $ if ':' `BC.elem` bs then bs else parseMac' bs
  where
    parseMac' bs' = case BC.splitAt 2 bs' of
        (h, "")   -> h
        (h, rest) -> h `mappend` ":" `mappend` parseMac' rest
