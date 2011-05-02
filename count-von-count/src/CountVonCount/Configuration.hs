-- | Global configuration
--
{-# LANGUAGE FlexibleContexts #-}
module CountVonCount.Configuration
    ( Configuration (..)
    , stationPosition
    , loadConfiguration
    , loadConfigurationFromFile
    ) where

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Monad (forM)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Control.Failure (Failure)
import Data.Object ( lookupObject, fromMapping, ObjectExtractError, fromScalar
                   , fromSequence
                   )
import Data.Object.Yaml ( YamlObject, toYamlScalar, decodeFile, fromYamlScalar
                        )

import CountVonCount.Types
import CountVonCount.Configuration.Rest
import CountVonCount.Configuration.Criteria
import CountVonCount.Configuration.Util

data Configuration = Configuration
    { configurationRest       :: RestConfiguration
    , configurationStationMap :: Map Station Position
    , configurationCsvLog     :: FilePath
    , configurationCriteria   :: [Criterium]
    , configurationMacSet     :: Set Mac
    , configurationListenPort :: Int
    , configurationAdminPort  :: Int
    , configurationVerbosity  :: Verbosity
    }

stationPosition :: Station -> Configuration -> Maybe Position
stationPosition station = M.lookup station . configurationStationMap

loadConfiguration :: (Applicative m, Failure ObjectExtractError m)
                  => YamlObject -> m Configuration
loadConfiguration object = do
    m <- fromMapping object
    Configuration <$> load loadRestConfiguration "Rest API" m
                  <*> load loadStations "Station map" m
                  <*> lookupString "CSV log" m
                  <*> load loadCriteria "Criteria" m
                  <*> load loadMacs "Mac set" m
                  <*> fmap read (lookupString "Listen port" m)
                  <*> fmap read (lookupString "Admin port" m)
                  <*> fmap read (lookupString "Verbosity" m)
  where
    load f k m = f =<< lookupObject (toYamlScalar k) m

loadMacs :: Failure ObjectExtractError m
           => YamlObject -> m (Set Mac)
loadMacs object = do
    scalars <- mapM fromScalar =<< fromSequence object
    return $ S.fromList $ map fromYamlScalar scalars 

loadStations :: Failure ObjectExtractError m
             => YamlObject -> m (Map Station Position)
loadStations object = do
    mapping <- fromMapping object
    tuples <- forM mapping $ \(k, v) -> do
        v' <- fromScalar v
        return (fromYamlScalar k, read $ fromYamlScalar v')
    return $ M.fromList tuples

loadConfigurationFromFile :: FilePath -> IO Configuration
loadConfigurationFromFile path = do
    yaml <- decodeFile path 
    loadConfiguration =<< yaml
