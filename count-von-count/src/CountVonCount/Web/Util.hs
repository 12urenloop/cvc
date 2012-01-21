module CountVonCount.Web.Util
    ( refFromParam
    ) where

import Data.ByteString (ByteString)
import qualified Snap.Core as Snap
import qualified Data.ByteString.Char8 as BC

import CountVonCount.Persistence

refFromParam :: ByteString -> Snap.Snap (Ref a)
refFromParam name = do
    Just param <- Snap.getParam name
    return $ refFromString $ BC.unpack param
