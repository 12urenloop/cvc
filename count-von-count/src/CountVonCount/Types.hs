--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Types
    ( Mac
    , Handler
    , handler
    , callHandler
    ) where


--------------------------------------------------------------------------------
import           Data.Text          (Text)


--------------------------------------------------------------------------------
import           CountVonCount.Log
import           CountVonCount.Util


--------------------------------------------------------------------------------
type Mac = Text


--------------------------------------------------------------------------------
data Handler a = Handler (Log -> a -> IO ())


--------------------------------------------------------------------------------
handler :: String -> (a -> IO ()) -> Handler a
handler name f = Handler $ \logger -> isolate_ logger ("Handler " ++ name) . f


--------------------------------------------------------------------------------
callHandler :: Log -> Handler a -> a -> IO ()
callHandler logger (Handler f) = f logger
