-- | Finite channels
--
module CountVonCount.FiniteChan
    ( FiniteChan
    , newFiniteChan
    , dupFiniteChan
    , runFiniteChan
    , writeFiniteChan
    , endFiniteChan
    , waitFiniteChan
    ) where

import Control.Applicative ((<$>), (<*>), pure)

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan , dupChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

import CountVonCount.Types

data FiniteChan a = FiniteChan
    { finiteName   :: String
    , finiteLogger :: Logger
    , finiteChan   :: Chan (Maybe a)
    , finiteSync   :: MVar ()
    }

instance Show (FiniteChan a) where
    show fc = "FiniteChan " ++ finiteName fc

newFiniteChan :: String -> Logger -> IO (FiniteChan a)
newFiniteChan name logger = FiniteChan <$> pure name
                                       <*> pure logger
                                       <*> newChan
                                       <*> newEmptyMVar

dupFiniteChan :: String -> FiniteChan a -> IO (FiniteChan a)
dupFiniteChan name (FiniteChan _ l c _) = FiniteChan <$> pure name
                                                     <*> pure l
                                                     <*> dupChan c
                                                     <*> newEmptyMVar

writeFiniteChan :: FiniteChan a -> a -> IO ()
writeFiniteChan chan = writeChan (finiteChan chan) . Just

endFiniteChan :: FiniteChan a -> IO ()
endFiniteChan chan = do
    writeChan (finiteChan chan) Nothing
    finiteLogger chan $
        "CountVonCount.FiniteChan.endFiniteChan: Closing " ++ show chan

waitFiniteChan :: FiniteChan a -> IO ()
waitFiniteChan chan = do
    finiteLogger chan $  "CountVonCount.FiniteChan.waitFiniteChan: "
                      ++ "Waiting for end of " ++ show chan
    takeMVar $ finiteSync chan
    finiteLogger chan $  "CountVonCount.FiniteChan.waitFiniteChan: "
                      ++ "Cleanly closed " ++ show chan

readFiniteChan :: FiniteChan a -> IO (Maybe a)
readFiniteChan chan = do
    mx <- readChan $ finiteChan chan
    case mx of
        Just x  -> return $ Just x
        Nothing -> do
            finiteLogger chan $  "CountVonCount.FiniteChan.readFiniteChan: "
                              ++ "Reached end of " ++ show chan
            putMVar (finiteSync chan) () >> return Nothing

runFiniteChan :: FiniteChan a
              -> s
              -> (a -> s -> IO s)
              -> IO s
runFiniteChan chan initial f = go initial
  where
    go state = do
        mx <- readFiniteChan chan
        case mx of Nothing -> return state
                   Just x  -> f x state >>= go
