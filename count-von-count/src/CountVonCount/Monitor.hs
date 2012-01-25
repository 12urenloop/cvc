module CountVonCount.Monitor
    ( newMonitor
    , runMonitor
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (forM,forM_,forever)
import Data.IORef
import System.Exit (ExitCode (ExitSuccess))
import System.IO
import System.Process (runInteractiveCommand,system)

import CountVonCount.Config
import CountVonCount.Types

type Monitor = [(Station,StationState)]

data StationState = StationState
    { online :: IORef Bool
--    , usbOK :: IORef Bool
--    , connected :: IORef Bool
--    , load :: IORef Double
    }

newMonitor :: Config -> IO Monitor
newMonitor config =
    forM (configStations config) $ \host -> do
        state <- StationState <$>
            newIORef False
--            newIORef False <*>
--            newIORef False <*>
--            newIORef 0.0
        return (host, state)

-- TODO: how to send events when stuff changes
runMonitor :: Monitor -> IO ()
runMonitor monitor = forever $ do
    forM_ monitor $ \(station, state) -> do
        pingHost station state
        connect station
    threadDelay 10000000 -- 10 seconds

pingHost :: Station -> StationState -> IO ()
pingHost station state = do
    code <- system $ "ping -c 1 " ++ stationName station ++ " >/dev/null 2>&1"
    writeIORef (online state) $ code == ExitSuccess

connect :: Station -> IO ()
connect station = do
    let sshCommand = "ssh " ++ stationName station ++ " sh"

    (pin, pout, perr, _) <- runInteractiveCommand sshCommand
    mapM_ (flip hSetBinaryMode False) [pin, pout, perr]
    hSetBuffering pin LineBuffering
    hSetBuffering pout LineBuffering

    hPutStrLn pin "echo hello"
    hGetLine pout >>= print
    hClose pin
