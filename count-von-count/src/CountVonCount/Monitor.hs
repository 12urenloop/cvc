module CountVonCount.Monitor
    ( monitor
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Concurrent (threadDelay)
import Control.Monad (forM,forM_,forever)
import Data.IORef
import System.Exit (ExitCode (ExitSuccess))
import System.Process (system)

import CountVonCount.Config
import CountVonCount.Types

type Monitor =  [(Station,StationState)]

data StationState = StationState
    { online :: IORef Bool
    , usbOK :: IORef Bool
    , connected :: IORef Bool
    , load :: IORef Double
    }

-- TODO: figure out how clients can access the information in Monitor
monitor :: Config -> IO ()
monitor config = do
    state <- initialState config
    forever $ do
        forM_ state $ checkHost
        threadDelay 10000000 -- 10 seconds

initialState :: Config -> IO Monitor
initialState config =
    forM (configStations config) $ \host -> do
        state <- StationState <$>
            newIORef False <*>
            newIORef False <*>
            newIORef False <*>
            newIORef 0.0
        return (host, state)

checkHost :: (Station, StationState) -> IO ()
checkHost (station, state) = do
    code <- system $ "ping -c 1 " ++ stationName station ++ " >/dev/null 2>&1"
    writeIORef (online state) $ code == ExitSuccess
