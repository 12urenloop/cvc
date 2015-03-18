--------------------------------------------------------------------------------
module  CountVonCount.Protocol
    ( Protocol (..)
    ) where

--------------------------------------------------------------------------------
import qualified Data.ByteString         as B
import           System.IO.Streams

--------------------------------------------------------------------------------
import           CountVonCount.EventBase
import           CountVonCount.Log

--------------------------------------------------------------------------------
data Protocol = Protocol {
    input  :: Log -> EventBase -> InputStream B.ByteString  -> IO (),
    output :: Log -> EventBase -> OutputStream B.ByteString -> IO ()
}

