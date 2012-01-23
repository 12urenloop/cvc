-- | Communication with sensors (i.e. Gyrid)
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module CountVonCount.Sensor
    ( listen
    ) where

import Control.Applicative ((*>))
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Monoid (mappend)
import Data.Time (UTCTime, getCurrentTime, parseTime)
import System.Locale (defaultTimeLocale)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.Enumerator (Iteratee, ($$), (=$))
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Enumerator as AE
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S
import qualified Network.Socket.Enumerator as SE

import CountVonCount.Types

listen :: Int
       -> (UTCTime -> Mac -> Mac -> IO ())
       -> IO ()
listen port handler = do
    putStrLn $ "Sensor: listening..."

    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    _    <- S.setSocketOption sock S.ReuseAddr 1
    host <- S.inet_addr "0.0.0.0"
    S.bindSocket sock $ S.SockAddrInet (fromIntegral port) host
    S.listen sock 5

    forever $ do
        (conn, _) <- S.accept sock
        _ <- forkIO $ ignore $ do
            S.sendAll conn "MSG,enable_rssi,true\r\n"
            S.sendAll conn "MSG,enable_cache,false\r\n"
        _ <- forkIO $ do
            E.run_ $ SE.enumSocket 256 conn $$
                E.sequence (AE.iterParser gyrid) =$ receive handler
            S.sClose conn
        return ()
  where
    ignore x = catch x $ const $ return ()

receive :: (UTCTime -> Mac -> Mac -> IO ())
        -> Iteratee Gyrid IO ()
receive handler = do
    g <- EL.head
    case g of
        Nothing                        -> return ()
        Just Ignored                   -> receive handler
        Just (Replay time station mac) -> do
            liftIO $ handler time station mac
            receive handler
        Just (Event station mac)       -> do
            time <- liftIO getCurrentTime
            liftIO $ handler time station mac
            receive handler

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
                Just t -> Replay t (addColons station) (addColons mac)
                _      -> Ignored
        [!station, _, !mac, _]            ->
            Event (addColons station) (addColons mac)
        _                                 -> Ignored
  where
    newline x  = x `B.elem` "\r\n"
    lineParser = A.skipWhile newline *> A.takeWhile (not . newline)

-- | Transform a mac without @:@ delimiters to one a mac with @:@ delimiters
addColons :: ByteString -> Mac
addColons bs = if ':' `BC.elem` bs then bs else addColons' bs
  where
    addColons' bs' = case BC.splitAt 2 bs' of
        (h, "")   -> h
        (h, rest) -> h `mappend` ":" `mappend` addColons rest
