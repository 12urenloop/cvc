-- | Listens on a certain port to receive the input
--
{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module CountVonCount.Receiver
    ( socketReceiver
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forever, unless)
import Data.Time (getCurrentTime, formatTime)
import Data.Monoid (mempty, mappend)
import System.Locale (defaultTimeLocale)
import Network.Socket.ByteString (recv)
import Network.Socket ( AddrInfo (..), AddrInfoFlag (..), SocketType (..)
                      , getAddrInfo, withSocketsDo, addrFamily, addrAddress
                      , defaultHints, defaultProtocol, listen, bindSocket
                      , socket, accept, sClose, Socket
                      )

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC

import CountVonCount.Types
import CountVonCount.FiniteChan
import CountVonCount.Configuration
import CountVonCount.Configuration.StationMap

socketReceiver :: Configuration -> Logger
               -> FiniteChan (Mac, Measurement) -> IO ()
socketReceiver conf logger chan = withSocketsDo $ do
    -- Obtain addres info structure
    let port = show $ configurationListenPort conf
    (serverAddr : _) <- getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
        Nothing (Just port)

    -- Log port
    logger $ "CountVonCount.Receiver.socketReceiver: Listening on port " ++ port

    -- Create and bind socket
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    bindSocket sock (addrAddress serverAddr)
    listen sock 5

    -- Server loop
    forever $ do
        (conn, _) <- accept sock
        receiveLines conn consumer
        sClose conn
  where
    stationMap = configurationStationMap conf

    consumer line = case SBC.words line of
        [station, mac] -> do
            timestamp <- currentTime
            case mapStation stationMap station of
                Just pos -> writeFiniteChan chan (mac, (timestamp, pos))
                Nothing  -> return ()
        _ -> logger $  "CountVonCount.Receiver.socketReceiver: Could not "
                    ++ "parse: " ++ show line

    currentTime :: IO Timestamp
    currentTime = read . formatTime defaultTimeLocale "%s" <$> getCurrentTime

-- | Read all lines from the socket. We assume lines are separated by a single
-- newline character. Every time we get a line, we run the given consumer
-- function on it.
--
receiveLines :: Socket -> (ByteString -> IO ()) -> IO ()
receiveLines sock consumer = receive mempty
  where
    -- Receive some data from the socket (blocking). The single parameter is a
    -- chunk of data we already got, but it's not a finished line yet (i.e. more
    -- characters we still need to receive might belong to the same line).
    receive prev = do
        chunk <- recv sock 1024
        -- If we don't receive any data, we can stop. In this case, we just run
        -- the consumer once more on the trailing line (which might not end in a
        -- newline). Otherwise, we consume the received chunk.
        if SB.null chunk then unless (SB.null prev) (consumer prev)
                         else consume (mappend prev chunk)

    -- Consume the chunk, breaking it apart in lines, and consuming them one by
    -- one. When we run out of data, we call receive to get more of it.
    consume chunk = case SBC.break (== '\n') chunk of
        (chunk', "") -> receive chunk'
        (line, rest) -> do
            consumer line
            consume (SB.drop 1 rest)
