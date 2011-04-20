-- | Configuration utilities
--
module CountVonCount.Configuration.Util
    ( lookupString
    ) where

import Control.Applicative ((<$>))

import Data.Object (Object, lookupScalar)
import Data.Object.Yaml (YamlScalar, toYamlScalar, fromYamlScalar, IsYamlScalar)

lookupString :: IsYamlScalar a
             => String                                        -- ^ Key
             -> [(YamlScalar, Object YamlScalar YamlScalar)]  -- ^ Mapping
             -> Maybe a                                       -- ^ Result
lookupString k m = fromYamlScalar <$> lookupScalar (toYamlScalar k) m
