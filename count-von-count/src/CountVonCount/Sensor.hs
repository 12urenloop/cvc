-- | Communication with sensors (i.e. Gyrid)
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module CountVonCount.Sensor
    ( listen
    ) where

import Control.Applicative ((*>))
import Control.Arrow ((&&&))
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Foldable (forM_)
import Data.Map (Map)
import Data.Monoid (mappend)
import Data.Time (UTCTime, getCurrentTime, parseTime)
import System.Locale (defaultTimeLocale)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.Enumerator (Iteratee, ($$), (=$))
import Network (PortID(..))
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Enumerator as AE
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Map as M
import qualified Data.Text.Encoding as T
import qualified Network as N
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S
import qualified Network.Socket.Enumerator as SE

import CountVonCount.Types

data SensorEnv = SensorEnv
    { stationMap    :: Map Mac Station
    , batonMap      :: Map Mac Baton
    , sensorHandler :: SensorEvent -> IO ()
    }

listen :: Int
       -> [Station]
       -> [Baton]
       -> (SensorEvent -> IO ())
       -> IO ()
listen port stations batons handler = do
    putStrLn "Sensor: listening..."

    sock <- N.listenOn (PortNumber $ fromIntegral port)

    forever $ do
        (conn, _) <- S.accept sock
        _ <- forkIO $ ignore $ do
            S.sendAll conn "MSG,enable_rssi,true\r\n"
            S.sendAll conn "MSG,enable_cache,false\r\n"
        _ <- forkIO $ do
            E.run_ $ SE.enumSocket 256 conn $$
                E.sequence (AE.iterParser gyrid) =$ receive env
            S.sClose conn
        return ()
  where
    ignore x = catch x $ const $ return ()

    env   = SensorEnv stMap bMap handler
    stMap = M.fromList $ fmap (stationMac &&& id) stations
    bMap  = M.fromList $ fmap (batonMac &&& id)   batons

receive :: SensorEnv
        -> Iteratee Gyrid IO ()
receive env = do
    g <- EL.head
    case g of
        Nothing    -> return ()
        Just event -> do
            time <- liftIO getCurrentTime
            let sensorEvent = case event of
                    Replay time' station mac -> makeEvent time' station mac
                    Event station mac        -> makeEvent time station mac
                    Ignored                  -> Nothing
            forM_ sensorEvent $ liftIO . sensorHandler env
            receive env
  where
    makeEvent time station baton = do
        st <- M.lookup station $ stationMap env
        bt <- M.lookup baton   $ batonMap   env
        return $ SensorEvent time st bt

data Gyrid
    = Event Mac Mac
    | Replay UTCTime Mac Mac
    | Ignored
    deriving (Show)

gyrid :: A.Parser Gyrid
gyrid = do
    line <- lineParser
    return $ case BC.split ',' line of
        ("MSG" : _)                       -> Ignored
        ("INFO" : _)                      -> Ignored
        ["REPLAY", !time, !station, !mac] ->
            case parseTime defaultTimeLocale "%s" (BC.unpack time) of
                Just t -> Replay t (parseMac station) (parseMac mac)
                _      -> Ignored
        [!station, _, !mac, !strength]            ->
            if (read $ BC.unpack strength) > threshold
            then Event (parseMac station) (parseMac mac)
            else Ignored
        _                                 -> Ignored
  where
    newline x  = x `B.elem` "\r\n"
    lineParser = A.skipWhile newline *> A.takeWhile (not . newline)
    threshold :: Double
    threshold  = 20.0 -- TODO: configurable

-- | Transform a mac without @:@ delimiters to one a mac with @:@ delimiters
parseMac :: ByteString -> Mac
parseMac bs = T.decodeUtf8 $ if ':' `BC.elem` bs then bs else parseMac' bs
  where
    parseMac' bs' = case BC.splitAt 2 bs' of
        (h, "")   -> h
        (h, rest) -> h `mappend` ":" `mappend` parseMac' rest
