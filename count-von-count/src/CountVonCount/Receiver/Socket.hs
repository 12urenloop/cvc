-- | Listens on a certain port to receive the input
--
module CountVonCount.Receiver.Socket
    ( socketReceiver
    ) where

import Control.Monad (forever)
import Network.Socket.ByteString (recv)
import Network.Socket ( AddrInfo (..), AddrInfoFlag (..), SocketType (..)
                      , getAddrInfo, withSocketsDo, addrFamily, addrAddress
                      , defaultHints, defaultProtocol, listen, bindSocket
                      , socket, accept, sClose
                      )

import qualified Data.ByteString.Char8 as SBC

import CountVonCount.Receiver
import CountVonCount.FiniteChan

socketReceiver :: Int -> Receiver
socketReceiver port chan = withSocketsDo $ do
    -- Obtain addres info structure
    (serverAddr : _) <- getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
        Nothing (Just $ show port)

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
        let [team, timestamp, position] = words $ SBC.unpack bs
        writeFiniteChan chan (team, (read timestamp, read position))
        sClose sock
