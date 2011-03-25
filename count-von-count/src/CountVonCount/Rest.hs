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

runRest :: Configuration      -- ^ Configuration
        -> Logger             -- ^ Logger
        -> FiniteChan Report  -- ^ Out channel to push
        -> IO ()              -- ^ Blocks forever
runRest conf logger chan = runFiniteChan chan () $
    \report () -> withMaybe (makeUrl conf $ reportMac report) $ \url -> do
        let Line _ speed = reportRegression report
            params = printf "speed=%f&suspicious=false" speed
            request = Request url PUT [] (params :: String)

        -- In another thread, perform the rest call and log the result
        _ <- forkIO $ do
            result <- simpleHTTP request
            case result of
                Left connError -> logger $
                    "CountVonCount.Rest.runRest: Could not connect to " ++
                    "REST API: " ++ show connError
                Right rsp -> logger $
                    "CountVonCount.Rest.runRest: Made call to the REST API, " ++
                    "return code: " ++ showResponseCode (rspCode rsp)

        return ()
  where
    withMaybe Nothing  _ = return ()
    withMaybe (Just x) f = f x
    showResponseCode (x, y, z) = printf "%d%d%d" x y z

makeUrl :: Configuration -> String -> Maybe URI
makeUrl conf mac = parseURI $
    printf "http://%s:%d/%s/api/0.1/%s/laps/increase"
        (restHost $ configurationRest conf)
        (restPort $ configurationRest conf)
        (restPath $ configurationRest conf)
        mac
