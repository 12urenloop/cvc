-- | Configuration utilities
--
module CountVonCount.Configuration.Util
    ( lookupString
    ) where

import Control.Applicative ((<$>))

import Data.Object (Object, lookupScalar)
import Data.Object.Yaml (YamlScalar, toYamlScalar, fromYamlScalar)

lookupString :: String                                        -- ^ Key
             -> [(YamlScalar, Object YamlScalar YamlScalar)]  -- ^ Mapping
             -> Maybe String                                  -- ^ Result
lookupString k m = fromYamlScalar <$> lookupScalar (toYamlScalar k) m
