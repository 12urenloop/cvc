-- | Queue IO actions
--
module CountVonCount.Queue
    ( Queue
    , makeQueue
    , push
    , someRequest
    ) where

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, modifyMVar_)
import Control.Applicative ((<$>))

import System.Directory (doesFileExist)

type Request = IO Bool

-- | Queue HTTP requests
--
newtype Queue = Queue {unQueue :: MVar (Seq Request)}

-- | Create an empty queue, given a delay in seconds
--
makeQueue :: Int -> IO Queue
makeQueue delay = do
    queue <- Queue <$> newMVar S.empty
    _ <- forkIO $ forever $ threadDelay (delay * 1000000) >> pop queue
    return queue

-- | Push a request onto the queue
--
push :: Queue -> Request -> IO ()
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
                        if failed r then putMVar mvar q
                                    else do putMVar mvar (S.drop 1 q)
                                            pop queue
  where
    mvar = unQueue queue
    failed = not

-- | Debugging
--
someRequest :: String -> Request
someRequest string = do
    e <- doesFileExist "/tmp/foo"
    if e then putStrLn string >> return True
         else return False
