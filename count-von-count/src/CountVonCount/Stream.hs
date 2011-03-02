-- | Steam-based parallelisation abstractions
--
{-# LANGUAGE GADTs #-}
module CountVonCount.Stream
    ( Stream
    , statelessStream
    , statefulStream
    , filterStream
    , (>>>>)
    , forkStream
    , sink
    ) where

import Control.Concurrent.Chan.Strict (Chan, readChan, writeChan, newChan)
import Control.Concurrent (forkIO)
import Control.Monad (forM_, forever)

import Control.DeepSeq (NFData)

-- | Streams with internal, unexposed states
--
data Stream a b where
    Filter :: s -> (a -> s -> IO ([b], s)) -> Stream a b
    Composition :: NFData x => Stream a x -> Stream x b -> Stream a b

-- | Create a stateless stream
--
statelessStream :: (a -> IO [b]) -> Stream a b
statelessStream f = statefulStream () $ \x _ -> do
    x' <- f x
    return (x', ())

-- | Create a stateful stream
--
statefulStream :: s -> (a -> s -> IO ([b], s)) -> Stream a b
statefulStream = Filter

-- | Filtering stream
--
filterStream :: (a -> Bool) -> Stream a a
filterStream f = statelessStream $ \x -> return $ if f x then [x] else []

(>>>>) :: NFData x => Stream a x -> Stream x b -> Stream a b
(>>>>) = Composition

-- | Run a stream, blocks forever
--
forkStream :: (NFData a, NFData b)
           => Stream a b -> Chan a -> Chan b -> IO ()
forkStream stream inChan outChan = case stream of
    Filter initial f -> do
        let loop state = do x <- readChan inChan
                            (x', state') <- f x state
                            forM_ x' $ writeChan outChan
                            loop state'
        _ <- forkIO $ loop initial
        return ()
    Composition f g -> do
        communication <- newChan
        forkStream f inChan communication
        forkStream g communication outChan

-- | Connect a stream to a sink
--
sink :: (NFData a, NFData b)
     => Stream a b    -- ^ Stream to sink
     -> Chan a        -- ^ Input channel
     -> (b -> IO ())  -- ^ Sink
     -> IO ()         -- ^ Blocks forever
sink stream inChan sink' = do
    outChan <- newChan
    forkStream stream inChan outChan
    forever $ sink' =<< readChan outChan
