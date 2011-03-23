-- | Rest API connectivity
--
module CountVonCount.Rest
    ( runRest
    ) where

import Text.Printf (printf)

import Network.URI (URI, parseURI)
import Network.HTTP (simpleHTTP, Request (Request), RequestMethod (PUT))

import CountVonCount.Configuration
import CountVonCount.Configuration.Rest
import CountVonCount.FiniteChan
import CountVonCount.Types

runRest :: Configuration                       -- ^ Configuration
        -> Logger                              -- ^ Logger
        -> FiniteChan (Timestamp, Mac, Score)  -- ^ Out channel to push
        -> IO ()                               -- ^ Blocks forever
runRest conf logger chan = runFiniteChan chan () $
    \(timestamp, mac, score) () -> withMaybe (makeUrl conf mac) $ \url -> do
        let params = printf "speed=5&suspicious=false"
            request = Request url PUT [] (params :: String)
        -- _ <- simpleHTTP request
        logger $ show (timestamp, mac, score)
  where
    withMaybe Nothing  _ = return ()
    withMaybe (Just x) f = f x

makeUrl :: Configuration -> String -> Maybe URI
makeUrl conf mac = parseURI $
    printf "http://%s:%d/%s/api/0.1/%s/laps/increase"
        (restHost $ configurationRest conf)
        (restPort $ configurationRest conf)
        (restPath $ configurationRest conf)
        mac
