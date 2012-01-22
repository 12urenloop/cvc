module CountVonCount.Web.Util
    ( refFromParam
    ) where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import qualified Snap.Core as Snap
import qualified Data.ByteString.Char8 as BC

import CountVonCount.Persistence

refFromParam :: Snap.MonadSnap m => ByteString -> m (Maybe (Ref a))
refFromParam name = do
    param <- Snap.getParam name
    return $ refFromString . BC.unpack <$> param
