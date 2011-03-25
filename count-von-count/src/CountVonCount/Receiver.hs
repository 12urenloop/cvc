-- | Listens on a certain port to receive the input
--
module CountVonCount.Receiver
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
        receive conn
  where
    stationMap = configurationStationMap conf

    receive sock = do
        bs <- recv sock 1024
        let [station, mac] = words $ SBC.unpack bs
        timestamp <- currentTime
        case mapStation stationMap station of
            Just position -> writeFiniteChan chan (mac, (timestamp, position))
            Nothing       -> return ()
        sClose sock

    currentTime :: IO Timestamp
    currentTime = read . formatTime defaultTimeLocale "%s" <$> getCurrentTime
