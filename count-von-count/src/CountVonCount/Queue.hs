-- | Queue IO actions
--
module CountVonCount.Queue
    ( Retry (..)
    , Retryable
    , Queue
    , makeQueue
    , push
    , assumeSuccesful
    , wrapSomeException
    ) where

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Control.Monad (forever)
import Control.Exception (try, SomeException)
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

-- | Queue IO actions
--
newtype Queue = Queue {unQueue :: MVar (Seq Retryable)}

-- | Create an empty queue, given a delay in seconds
--
makeQueue :: Int -> IO Queue
makeQueue delay = do
    queue <- Queue <$> newMVar S.empty
    _ <- forkIO $ forever $ do
        threadDelay (delay * 1000000)
        _ <- forkIO $ pop queue
        return ()
    return queue

-- | Push an action onto the queue
--
push :: Queue -> Retryable -> IO ()
push queue retryable = do
    modifyMVar_ (unQueue queue) $ return . (|> retryable)
    pop queue

-- | Try to pop a retryable
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
wrapSomeException :: Logger -> Retryable -> Retryable
wrapSomeException logger failing = do
    result <- try failing
    case result of Left e -> failed e >> return Retry
                   Right r -> return r
  where
    failed :: SomeException -> IO ()
    failed e = logger Error $
        "CountVonCount.Queue.wrapSomeException: " ++ show e
