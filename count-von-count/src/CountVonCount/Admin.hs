-- | Admin interface
--
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, BangPatterns #-}
module CountVonCount.Admin where

import Control.Applicative (Applicative, Alternative, (<$>))
import Control.Concurrent.MVar
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Reader
import Data.Map (Map)
import Data.Monoid (mappend)
import qualified Data.Map as M

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SBC
import Text.Blaze (Html)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Snap.Http.Server (httpServe, emptyConfig, addListen, ConfigListen (..))
import Snap.Types ( MonadSnap (..), Snap, modifyResponse, route, setHeader
                  , getParam, writeLBS, addHeader, ifTop, redirect
                  )

import CountVonCount.Dispatcher
import CountVonCount.Configuration
import qualified CountVonCount.Admin.Views as Views

type Count = Map ByteString Int

data AppEnvironment = AppEnvironment
    { appDispatcherState :: MVar DispatcherState
    , appConfiguration   :: Configuration
    }

newtype App a = App {unApp :: ReaderT AppEnvironment Snap a}
              deriving ( Functor, Monad, Applicative
                       , Alternative, MonadIO, MonadCatchIO
                       , MonadPlus, MonadReader AppEnvironment
                       )

instance MonadSnap App where
    liftSnap = App . lift

-- | Set page refresh in seconds
--
setRefresh :: Int -> App ()
setRefresh = modifyResponse . setHeader "Refresh" . SBC.pack . show

respondBlaze :: Html -> App ()
respondBlaze html = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml html

root :: App ()
root = do
    state <- liftIO . readMVar . appDispatcherState =<< ask
    conf <- appConfiguration <$> ask
    setRefresh 5
    respondBlaze $ Views.root conf state

reset :: App ()
reset = do
    state <- appDispatcherState <$> ask
    sure <- getParam "sure"
    when (sure == Just "on") $ liftIO $
        modifyMVar_ state $ return . resetAllCounters
    redirect "/"

mac :: App ()
mac = do
    state <- liftIO . readMVar . appDispatcherState =<< ask
    conf <- appConfiguration <$> ask
    Just mac' <- getParam "mac"
    setRefresh 5
    respondBlaze $ Views.mac conf mac' $ state M.! mac'

macReset :: App ()
macReset = do
    state <- appDispatcherState <$> ask
    Just mac' <- getParam "mac"
    sure <- getParam "sure"
    when (sure == Just "on") $ liftIO $
        modifyMVar_ state $ return . resetCounter mac'
    redirect $ "/" `mappend` mac'

app :: App ()
app = route
    [ ("/",           ifTop root)
    , ("/reset",      reset)
    , ("/:mac",       mac)
    , ("/:mac/reset", macReset)
    ]

runAdmin :: Configuration -> MVar DispatcherState -> IO ()
runAdmin conf dispatcherState = do
    httpServe config $ runReaderT (unApp app) env
  where
    port = configurationAdminPort conf
    config = addListen (ListenHttp "0.0.0.0" port) emptyConfig
    env = AppEnvironment dispatcherState conf
