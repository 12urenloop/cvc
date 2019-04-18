--------------------------------------------------------------------------------
-- | Communication with sensors (i.e. Gyrid)
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Sensor
    ( listen
    ) where

--------------------------------------------------------------------------------
import           Control.Concurrent        (forkIO)
import           Control.Monad             (forever)
import qualified Data.Foldable             as F
import           Data.ByteString.Char8     ()
import qualified Network.Socket            as S
import qualified System.IO.Streams         as Streams
import           System.IO.Streams.Network (socketToStreams)

--------------------------------------------------------------------------------
import           CountVonCount.EventBase
import           CountVonCount.Log
import           CountVonCount.Protocol
import           CountVonCount.Util
--------------------------------------------------------------------------------

listen :: Protocol
       -> Log
       -> EventBase
       -> Int
       -> IO ()
listen protocol logger eventBase port = S.withSocketsDo $ do
    sock <- listenOn port

    forever $ do
        (conn, addr)        <- S.accept sock
        (inBytes, outBytes) <- socketToStreams conn

        _ <- forkIO $ isolate_ logger "Sensor send config" $ do
            string logger "CountVonCount.Sensor.listen"
                $ "Socket connected to " ++ show addr
            output protocol outBytes

        _ <- forkIO $ isolate_ logger "Sensor receive" $ do
            publisher <- Streams.makeOutputStream $ F.mapM_ $ publish eventBase
            input protocol logger inBytes >>= Streams.connectTo publisher
            string logger "CountVonCount.Sensor.listen"
                $ "Socket gracefully disconnected (client was " ++ show addr ++ ")"
            S.close conn
        return ()
  where
    listenOn p = do
        let hints = S.defaultHints {
                    S.addrFlags = [S.AI_PASSIVE]
                  , S.addrSocketType = S.Stream
                  }
        addr:_ <- S.getAddrInfo (Just hints) Nothing (Just $ show p)
        sock <- S.socket (S.addrFamily addr)
                         (S.addrSocketType addr)
                         (S.addrProtocol addr)
        S.setSocketOption sock S.ReuseAddr 1
        -- If the prefork technique is not used,
        -- set CloseOnExec for the security reasons.
        let fd = S.fdSocket sock
        S.setCloseOnExecIfNeeded fd
        S.bind sock (S.addrAddress addr)
        S.listen sock 10
        return sock

