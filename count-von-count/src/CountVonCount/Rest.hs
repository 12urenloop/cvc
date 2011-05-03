-- | Rest API connectivity
--
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Rest
    ( runRest
    ) where

import Control.Arrow (second)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan)
import Data.Monoid (mappend)

import Network.HTTP.Types (methodPut, renderSimpleQuery)
import Network.HTTP.Enumerator
import qualified Data.ByteString.Char8 as SBC

import CountVonCount.Configuration
import CountVonCount.Configuration.Rest
import CountVonCount.Chan
import CountVonCount.Types
import CountVonCount.Queue

runRest :: Configuration  -- ^ Configuration
        -> Logger         -- ^ Logger
        -> Chan Report    -- ^ Out channel to push
        -> IO ()          -- ^ Blocks forever
runRest conf logger chan = do
    -- Create a queue for the requests and a HTTP connection manager
    queue <- makeQueue 2

    -- Infinitely...
    readChanLoop chan $ \report -> do
        -- Log about the received report
        logger Info $ "CountVonCount.Rest.runRest: Received report: " ++
            "Name = " ++ SBC.unpack (prettifyMac (reportMac report) conf) ++
            "Mac = " ++ show (reportMac report) ++ ", " ++
            "Score = " ++ show (reportScore report)

        -- In another thread, perform the rest call and log the result
        let request = makeRequest conf report
        _ <- forkIO $ push queue $ do
            manager <- newManager
            r <- wrapRequest logger manager request
            closeManager manager
            return r
        return ()


makeRequest :: Configuration -> Report -> Request IO
makeRequest configuration report = Request
    { method = methodPut
    , secure = False
    , checkCerts = const (return True)
    , host = restHost rest
    , port = restPort rest
    , path = path'
    , queryString = []
    , requestHeaders = [("Connection", "close")]
    , requestBody = RequestBodyBS (renderSimpleQuery False params)
    }
  where
    rest = configurationRest configuration
    Line _ speed = reportRegression report
    params = map (second SBC.pack) $
        ("speed", show speed) :
        map ((,) "warning") (takeWarnings $ reportScore report)
    takeWarnings (Warning w) = w
    takeWarnings _           = []
    path' = "/" `mappend` restPath rest `mappend` "/api/0.1/"
                `mappend` reportMac report `mappend` "/laps/increase"

wrapRequest :: Logger -> Manager -> Request IO -> Retryable
wrapRequest logger manager request = wrapSomeException logger $ do
    response <- httpLbs request manager
    let code = statusCode response
    logger Info $  "CountVonCount.Rest.runRest: Made call to the REST "
                ++ "API, return code: " ++ show code
    return $ if isOk code then Done else Retry
  where
    isOk code = code >= 200 && code < 300
