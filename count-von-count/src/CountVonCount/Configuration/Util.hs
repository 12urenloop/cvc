-- | Configuration utilities
--
{-# LANGUAGE FlexibleContexts #-}
module CountVonCount.Configuration.Util
    ( lookupString
    ) where

import Control.Applicative ((<$>))

import Control.Failure (Failure)
import Data.Object (Object, lookupScalar, ObjectExtractError)
import Data.Object.Yaml (YamlScalar, toYamlScalar, fromYamlScalar, IsYamlScalar)

lookupString :: (Functor m, Failure ObjectExtractError m, IsYamlScalar a)
             => String                                        -- ^ Key
             -> [(YamlScalar, Object YamlScalar YamlScalar)]  -- ^ Mapping
             -> m a                                           -- ^ Result
lookupString k m = fromYamlScalar <$> lookupScalar (toYamlScalar k) m
