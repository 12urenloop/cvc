-- | Configuration utilities
--
{-# LANGUAGE FlexibleContexts #-}
module CountVonCount.Configuration.Util
    ( lookupString
    , loadMap
    ) where

import Control.Monad (forM)
import Control.Applicative ((<$>))
import Data.Map (Map)
import qualified Data.Map as M

import Control.Failure (Failure)
import Data.Object ( Object, lookupScalar, fromMapping, ObjectExtractError
                   , fromScalar
                   )
import Data.Object.Yaml ( YamlScalar, YamlObject, toYamlScalar, fromYamlScalar
                        , IsYamlScalar
                        )

lookupString :: (Functor m, Failure ObjectExtractError m, IsYamlScalar a)
             => String                                        -- ^ Key
             -> [(YamlScalar, Object YamlScalar YamlScalar)]  -- ^ Mapping
             -> m a                                           -- ^ Result
lookupString k m = fromYamlScalar <$> lookupScalar (toYamlScalar k) m

loadMap :: (Failure ObjectExtractError m, IsYamlScalar k, IsYamlScalar v, Ord k)
        => YamlObject -> m (Map k v)
loadMap object = do
    mapping <- fromMapping object
    tuples <- forM mapping $ \(k, v) -> do
        v' <- fromScalar v
        return (fromYamlScalar k, fromYamlScalar v')
    return $ M.fromList tuples
