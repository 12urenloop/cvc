-- | A web server that happily consumes every request
--
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, BangPatterns #-}
module Main where

-- import Control.Monad (MonadPlus)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Applicative (Applicative, Alternative, (<$>))
import System.Environment (getArgs)
import Control.Monad.Reader
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Concurrent.MVar

import Data.ByteString (ByteString)

import Snap.Types -- (Snap, modifyResponse, setResponseCode, route)
import Snap.Http.Server (httpServe, emptyConfig, addListen, ConfigListen (..))
import Text.Blaze.Renderer.Utf8 (renderHtml)

import Types
import qualified Views as Views

newtype App a = App {unApp :: ReaderT (MVar Count) Snap a}
              deriving ( Functor, Monad, Applicative
                       , Alternative, MonadIO, MonadCatchIO
                       , MonadPlus, MonadReader (MVar Count)
                       )

instance MonadSnap App where
    liftSnap = App . lift

lapsIncrease :: App ()
lapsIncrease = do
    Just mac <- getParam "mac"
    count <- ask
    liftIO $ modifyMVar_ count $ \count' ->
        let x = (fromMaybe 0 $ M.lookup mac count') + 1
        in x `seq` return $ M.insert mac x count'
    modifyResponse $ setResponseCode 204

overview :: App ()
overview = do
    count <- liftIO . readMVar =<< ask
    modifyResponse $ setHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml $ Views.overview count

app :: App ()
app = route
    [ ("/dr.beaker/api/0.1/:mac/laps/increase", lapsIncrease)
    , ("/overview", overview)
    ]

main :: IO ()
main = do
    env <- newMVar M.empty
    [port] <- map read <$> getArgs
    let config = addListen (ListenHttp "0.0.0.0" port) emptyConfig
    httpServe config $ runReaderT (unApp app) env
