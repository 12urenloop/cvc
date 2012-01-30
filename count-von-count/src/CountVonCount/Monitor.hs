module CountVonCount.Monitor
    ( newMonitor
    , runMonitor
    , MonitorEvent (..)
    , StationState
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (forM,forM_,forever,when)
import Data.IORef
import System.Exit (ExitCode (ExitSuccess))
import System.IO
import System.Process (runInteractiveCommand,system,ProcessHandle)

import CountVonCount.Types

type Monitor = [(Station, IORef StationState)]

data StationState = StationState
    { online :: Bool
    , usbOK :: Bool
    , connected :: Bool
    , load :: Double
    } deriving (Show, Eq)

data MonitorEvent = StateChanged Station StationState

newMonitor :: [Station] -> IO Monitor
newMonitor stations =
    forM stations $ \station -> do
        state <- newIORef $ StationState False False False 0.0
        return (station, state)

runMonitor :: Monitor
           -> (MonitorEvent -> IO ())
           -> IO ()
runMonitor monitor handler = forever $ do
    forM_ monitor $ \(station, state) -> do
        -- TODO: abstract using fclabels?
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
