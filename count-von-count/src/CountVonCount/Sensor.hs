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
import           Network                   (PortID (..))
import qualified Network                   as N
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
listen protocol logger eventBase port = do
    sock <- N.listenOn (PortNumber $ fromIntegral port)

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
            S.sClose conn
        return ()

