-- | Allows clients (browsers) to register for server events
{-# LANGUAGE Rank2Types #-}
module Network.WebSockets.PubSub
    ( PubSub
    , newPubSub
    , publish
    , subscribe
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_, forever)
import Control.Monad.Trans (liftIO)
import Data.IntMap (IntMap)
import qualified Control.Concurrent.MVar as MV

import qualified Data.Aeson as A
import qualified Data.IntMap as IM
import qualified Network.WebSockets as WS

data JsonSink = JsonSink (forall a. A.ToJSON a => a -> IO ())

data PubSub_ = PubSub_
    { pubSubNextId :: Int
    , pubSubSinks  :: IntMap JsonSink
    }

addClient :: WS.TextProtocol p => WS.Sink p -> PubSub_ -> (PubSub_, Int)
addClient sink (PubSub_ nid sinks) =
    (PubSub_ (nid + 1) (IM.insert nid sink' sinks), nid)
  where
    sink' = JsonSink $ WS.sendSink sink . WS.textData . A.encode

removeClient :: Int -> PubSub_ -> PubSub_
removeClient ref ps = ps {pubSubSinks = IM.delete ref (pubSubSinks ps)}

newtype PubSub = PubSub (MV.MVar PubSub_)

newPubSub :: IO PubSub
newPubSub = PubSub <$> MV.newMVar PubSub_
    { pubSubNextId  = 0
    , pubSubSinks  = IM.empty
    }

publish :: A.ToJSON a => PubSub -> a -> IO ()
publish (PubSub mvar) x = do
    sinks <- pubSubSinks <$> MV.readMVar mvar 
    forM_ (IM.toList sinks) $ \(_, JsonSink s) -> s x

-- | Blocks forever
subscribe :: WS.TextProtocol p => PubSub -> WS.WebSockets p ()
subscribe (PubSub mvar) = do
    sink <- WS.getSink
    ref  <- liftIO $ MV.modifyMVar mvar $ return . addClient sink
    WS.catchWsError loop $ const $ liftIO $
        MV.modifyMVar_ mvar $ return . removeClient ref
  where
    loop = forever $ do
        _ <- WS.receiveDataMessage
        return ()
