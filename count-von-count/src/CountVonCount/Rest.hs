-- | Rest API connectivity
--
module CountVonCount.Rest
    ( runRest
    ) where

import Text.Printf (printf)

import Control.Concurrent (forkIO)
import Network.URI (URI, parseURI)
import Network.HTTP ( simpleHTTP, Request (Request), RequestMethod (PUT)
                    , Response (..)
                    )

import CountVonCount.Configuration
import CountVonCount.Configuration.Rest
import CountVonCount.FiniteChan
import CountVonCount.Types
import CountVonCount.Queue

runRest :: Configuration      -- ^ Configuration
        -> Logger             -- ^ Logger
        -> FiniteChan Report  -- ^ Out channel to push
        -> IO ()              -- ^ Blocks forever
runRest conf logger chan = do
    -- Create a queue for the requests
    queue <- makeQueue 2

    -- Infinitely...
    runFiniteChan chan () $
        \report () -> withMaybe (makeUrl conf $ reportMac report) $ \url -> do
            let Line _ speed = reportRegression report
                params = printf "speed=%f&suspicious=false" speed
                request = Request url PUT [] (params :: String)

            -- Log about the received report
            logger $ "CountVonCount.Rest.runRest: Received report: " ++
                "Mac = " ++ show (reportMac report) ++ ", " ++
                "Score = " ++ show (reportScore report) ++ ", " ++
                "Speed = " ++ show speed

            -- In another thread, perform the rest call and log the result
            _ <- forkIO $ push queue $ wrapRequest logger request
            return ()
  where
    withMaybe Nothing  _ = return ()
    withMaybe (Just x) f = f x

wrapRequest :: Logger -> Request String -> Failing
wrapRequest logger request = wrapIOException logger $ do
    result <- simpleHTTP request
    case result of
        Left connError -> do
            logger $ "CountVonCount.Rest.runRest: Could not connect to " ++
                     "REST API: " ++ show connError
            return False
        Right rsp -> do
            logger $ "CountVonCount.Rest.runRest: Made call to the REST " ++
                     "API, return code: " ++ showResponseCode (rspCode rsp)
            return $ isOk $ rspCode rsp
  where
    showResponseCode (x, y, z) = printf "%d%d%d" x y z
    isOk (x, _, _) = x == 2

makeUrl :: Configuration -> String -> Maybe URI
makeUrl conf mac = parseURI $
    printf "http://%s:%d/%s/api/0.1/%s/laps/increase"
        (restHost $ configurationRest conf)
        (restPort $ configurationRest conf)
        (restPath $ configurationRest conf)
        mac
