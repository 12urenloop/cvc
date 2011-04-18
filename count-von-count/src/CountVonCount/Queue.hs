-- | Queue IO actions
--
module CountVonCount.Queue
    ( Retry (..)
    , Retryable
    , Queue
    , makeQueue
    , push
    , assumeSuccesful
    , wrapIOException
    ) where

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Control.Monad (forever)
import Control.Exception (try, IOException)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, modifyMVar_)
import Control.Applicative ((<$>))

import CountVonCount.Types

-- | An action either succeeds or fails
--
data Retry = Done | Retry
           deriving (Show, Eq, Ord)

-- | A possibly failing action
--
type Retryable = IO Retry

-- | Queue HTTP requests
--
newtype Queue = Queue {unQueue :: MVar (Seq Retryable)}

-- | Create an empty queue, given a delay in seconds
--
makeQueue :: Int -> IO Queue
makeQueue delay = do
    queue <- Queue <$> newMVar S.empty
    _ <- forkIO $ forever $ threadDelay (delay * 1000000) >> pop queue
    return queue

-- | Push a request onto the queue
--
push :: Queue -> Retryable -> IO ()
push queue request = do
    modifyMVar_ (unQueue queue) $ return . (|> request)
    pop queue

-- | Try to pop a request
--
pop :: Queue -> IO ()
pop queue = do
    -- HAHA THIS LOOKS LIKE A STAIRCASE
    q <- takeMVar mvar
    if S.null q then putMVar mvar S.empty
                else do r <- q `S.index` 0
                        case r of Retry -> putMVar mvar q
                                  Done  -> do putMVar mvar (S.drop 1 q)
                                              pop queue
  where
    mvar = unQueue queue

-- | Assume that the IO action is succesful
--
assumeSuccesful :: IO () -> Retryable
assumeSuccesful action = action >> return Done

-- | Wrap the failing action to also fail on IO exceptions
--
wrapIOException :: Logger -> Retryable -> Retryable
wrapIOException logger failing = do
    result <- try failing
    case result of Left e -> failed e >> return Retry
                   Right r -> return r
  where
    failed :: IOException -> IO ()
    failed e = logger $ "CountVonCount.Queue.wrapIOException: " ++ show e
