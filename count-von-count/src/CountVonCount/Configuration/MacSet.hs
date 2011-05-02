-- | Set of allowed mac addresses
--
{-# LANGUAGE FlexibleContexts #-}
module CountVonCount.Configuration.MacSet
    ( loadMacSet
    ) where

import Data.Set (Set)
import qualified Data.Set as S

import Control.Failure (Failure)
import Data.Object (fromSequence, fromScalar, ObjectExtractError)
import Data.Object.Yaml (YamlObject, fromYamlScalar)

import CountVonCount.Types

loadMacSet :: Failure ObjectExtractError m
           => YamlObject -> m (Set Mac)
loadMacSet object = do
    scalars <- mapM fromScalar =<< fromSequence object
    return $ S.fromList $ map fromYamlScalar scalars 
