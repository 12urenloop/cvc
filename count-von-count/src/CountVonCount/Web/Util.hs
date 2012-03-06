{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web.Util
    ( refFromParam
    , readParam
	, json
    ) where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import qualified Snap.Core as Snap
import qualified Snap.Util.Readable as Snap
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC

import CountVonCount.Persistence

refFromParam :: Snap.MonadSnap m => ByteString -> m (Maybe (Ref a))
refFromParam name = do
    param <- Snap.getParam name
    return $ refFromString . BC.unpack <$> param

readParam :: (Snap.Readable b, Snap.MonadSnap f) => ByteString -> f (Maybe b)
readParam param = (>>= Snap.fromBS) `fmap` Snap.getParam param

json :: (A.ToJSON a, Snap.MonadSnap m) => a -> m ()
json x = do
    Snap.modifyResponse $ Snap.setContentType "application/json"
    Snap.writeLBS $ A.encode x
