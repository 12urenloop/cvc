{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Web.Util
    ( refFromParam
    , json
    ) where

import           Control.Applicative       ((<$>))
import qualified Data.Aeson                as A
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BC
import qualified Snap.Core                 as Snap
import           Prelude

import CountVonCount.Persistence

refFromParam :: Snap.MonadSnap m => ByteString -> m (Maybe (Ref a))
refFromParam name = do
    param <- Snap.getParam name
    return $ refFromString . BC.unpack <$> param

json :: (A.ToJSON a, Snap.MonadSnap m) => a -> m ()
json x = do
    Snap.modifyResponse $ Snap.setContentType "application/json"
    Snap.writeLBS $ A.encode x
