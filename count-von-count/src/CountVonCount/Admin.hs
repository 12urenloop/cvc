-- | Admin interface
--
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, BangPatterns #-}
module CountVonCount.Admin where

import Control.Applicative (Applicative, Alternative)
import Control.Concurrent.MVar
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Reader
import Data.Map (Map)
import Data.Monoid (mappend)
import qualified Data.Map as M

import Data.ByteString (ByteString)
import Text.Blaze (Html)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Snap.Http.Server (httpServe, emptyConfig, addListen, ConfigListen (..))
import Snap.Types ( MonadSnap (..), Snap, modifyResponse, route
                  , getParam, writeLBS, addHeader, ifTop, redirect
                  )

import CountVonCount.Dispatcher
import CountVonCount.Configuration
import qualified CountVonCount.Admin.Views as Views

type Count = Map ByteString Int

newtype App a = App {unApp :: ReaderT (MVar DispatcherState) Snap a}
              deriving ( Functor, Monad, Applicative
                       , Alternative, MonadIO, MonadCatchIO
                       , MonadPlus, MonadReader (MVar DispatcherState)
                       )

instance MonadSnap App where
    liftSnap = App . lift

respondBlaze :: Html -> App ()
respondBlaze html = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml html

root :: App ()
root = do
    state <- liftIO . readMVar =<< ask
    respondBlaze $ Views.root state

mac :: App ()
mac = do
    state <- liftIO . readMVar =<< ask
    Just mac' <- getParam "mac"
    respondBlaze $ Views.mac mac' $ state M.! mac'

macReset :: App ()
macReset = do
    state <- ask
    Just mac' <- getParam "mac"
    sure <- getParam "sure"
    when (sure == Just "on") $ liftIO $
        modifyMVar_ state $ return . resetCounter mac'
    redirect $ "/" `mappend` mac'

app :: App ()
app = route
    [ ("/",           ifTop root)
    , ("/:mac",       mac)
    , ("/:mac/reset", macReset)
    ]

runAdmin :: Configuration -> MVar DispatcherState -> IO ()
runAdmin conf dispatcherState = do
    httpServe config $ runReaderT (unApp app) dispatcherState
  where
    port = configurationAdminPort conf
    config = addListen (ListenHttp "0.0.0.0" port) emptyConfig
