-- | Listens on a certain port to receive the input
--
module CountVonCount.Receiver.Socket
    ( socketReceiver
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forever)
import Data.Time (getCurrentTime, formatTime)
import System.Locale (defaultTimeLocale)
import Network.Socket.ByteString (recv)
import Network.Socket ( AddrInfo (..), AddrInfoFlag (..), SocketType (..)
                      , getAddrInfo, withSocketsDo, addrFamily, addrAddress
                      , defaultHints, defaultProtocol, listen, bindSocket
                      , socket, accept, sClose
                      )

import qualified Data.ByteString.Char8 as SBC

import CountVonCount.Types
import CountVonCount.Receiver
import CountVonCount.FiniteChan
import CountVonCount.Configuration

socketReceiver :: Configuration -> Logger -> Receiver
socketReceiver configuration logger chan = withSocketsDo $ do
    -- Obtain addres info structure
    let port = show $ configurationListenPort configuration
    (serverAddr : _) <- getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
        Nothing (Just port)

    -- Log port
    logger $ "Listening on port " ++ port

    -- Create and bind socket
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    bindSocket sock (addrAddress serverAddr)
    listen sock 5

    -- Server loop
    forever $ do
        (conn, _) <- accept sock
        receive conn
  where
    receive sock = do
        bs <- recv sock 1024
        let [mac, _, position] = words $ SBC.unpack bs
        timestamp <- currentTime
        writeFiniteChan chan (mac, (timestamp, read position))
        sClose sock

    currentTime :: IO Timestamp
    currentTime = read . formatTime defaultTimeLocale "%s" <$> getCurrentTime
