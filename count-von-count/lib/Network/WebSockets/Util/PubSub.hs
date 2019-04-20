-- | This is a simple utility module to implement a publish-subscribe pattern.
-- Note that this only allows communication in a single direction: pusing data
-- from the server to connected clients (browsers).
--
-- Usage:
--
-- * Create a new 'PubSub' handle using 'newPubSub'
--
-- * Subscribe your clients using the 'subscribe' call
--
-- * Push new updates from the server using the 'publishText' call
--
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.WebSockets.Util.PubSub
    ( PubSub
    , newPubSub
    , publishText
    , subscribe
    ) where

import           Control.Applicative     ((<$>))
import qualified Control.Concurrent.MVar as MV
import           Control.Exception       (IOException, handle)
import           Control.Monad           (foldM, forever)
import           Data.IntMap             (IntMap)
import qualified Data.IntMap             as IM
import           Data.List               (foldl')
import           Network.WebSockets
import           Prelude

data PubSub_ = PubSub_
    { pubSubNextId :: Int
    , pubSubConns  :: IntMap Connection
    }

addClient :: Connection -> PubSub_ -> (PubSub_, Int)
addClient conn (PubSub_ nid conns) =
    (PubSub_ (nid + 1) (IM.insert nid conn conns), nid)

removeClient :: Int -> PubSub_ -> PubSub_
removeClient ref ps = ps {pubSubConns = IM.delete ref (pubSubConns ps)}

-- | A handle which keeps track of subscribed clients
newtype PubSub = PubSub (MV.MVar PubSub_)

-- | Create a new 'PubSub' handle, with no clients initally connected
newPubSub :: IO PubSub
newPubSub = PubSub <$> MV.newMVar PubSub_
    { pubSubNextId = 0
    , pubSubConns  = IM.empty
    }

-- | Broadcast a message to all connected clients
publishText :: WebSocketsData a => PubSub -> a -> IO ()
publishText (PubSub mvar) msg = MV.modifyMVar_ mvar $ \pubSub -> do
    -- Take care to detect and remove broken clients
    broken <- foldM publish' [] (IM.toList $ pubSubConns pubSub)
    return $ foldl' (flip removeClient) pubSub broken
  where
    -- Publish the message to a single client, add it to the broken list if an
    -- IOException occurs
    publish' broken (i, c) =
        handle (\(_ :: IOException) -> return (i : broken)) $ do
            sendTextData c msg
            return broken

-- | Blocks forever
subscribe :: PubSub -> Connection -> IO ()
subscribe (PubSub mvar) conn = do
    ref <- MV.modifyMVar mvar $ return . addClient conn
    handle (onConnectionException ref) loop
  where
    onConnectionException :: Int -> ConnectionException -> IO ()
    onConnectionException ref _ = MV.modifyMVar_ mvar $ return . removeClient ref

    loop :: IO ()
    loop = forever $ do
        _ <- receiveDataMessage conn
        return ()
