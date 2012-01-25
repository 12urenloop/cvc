module CountVonCount.Monitor
    ( newMonitor
    , runMonitor
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (forM,forM_,forever,when)
import Data.IORef
import System.Exit (ExitCode (ExitSuccess))
import System.IO
import System.Process (runInteractiveCommand,system,ProcessHandle)

import CountVonCount.Config
import CountVonCount.Types

type Monitor = [(Station, StationState)]

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
        putStrLn $ "Checking " ++ (show station)
        hostOnline <- pingHost station
        when hostOnline $ do
            conn <- newConnection station
            load <- checkLoad conn
            closeConnection conn
            putStrLn $ show (hostOnline, load)
        return ()

    threadDelay 10000000 -- 10 seconds

data Connection = Conn Handle Handle Handle ProcessHandle

newConnection :: Station -> IO Connection
newConnection station = do
    let sshCommand = "ssh " ++ stationName station ++ " sh"
    (pin, pout, perr, pid) <- runInteractiveCommand sshCommand

    -- configure handles
    mapM_ (flip hSetBinaryMode False) [pin, pout, perr]
    hSetBuffering pin LineBuffering
    hSetBuffering pout LineBuffering

    return $ Conn pin pout perr pid

closeConnection :: Connection -> IO ()
closeConnection conn = do
    return ()

runCommand :: Connection -> String -> IO String
runCommand (Conn pin pout _ _) cmd = do
    hPutStrLn pin cmd
    hGetLine pout

pingHost :: Station -> IO Bool
pingHost station = do
    code <- system $ "ping -c 1 " ++ stationName station ++ " >/dev/null 2>&1"
    return $ code == ExitSuccess

checkLoad :: Connection -> IO Double
checkLoad conn = do
    load <- runCommand conn "uptime"
    return 0.0
