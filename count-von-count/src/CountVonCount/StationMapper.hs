-- | Module that maps station names to actual positions
--
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module CountVonCount.StationMapper
    ( StationMapper
    , mapStation
    , loadStationMapper
    ) where

import Control.Monad (forM)
import Data.Monoid (Monoid)
import Data.Map (Map)
import qualified Data.Map as M

import Data.Object (fromMapping, fromScalar)
import Data.Object.Yaml (YamlObject, fromYamlScalar)

import CountVonCount.Types

newtype StationMapper = StationMapper (Map Station Position)
                      deriving (Show, Monoid)

loadStationMapper :: YamlObject -> Maybe StationMapper
loadStationMapper object = do
    mapping <- fromMapping object
    tuples <- forM mapping $ \(k, v) -> do
        v' <- fromScalar v
        return (fromYamlScalar k, read $ fromYamlScalar v')
    return $ StationMapper $ M.fromList tuples

mapStation :: StationMapper -> Station -> Maybe Position
mapStation (StationMapper m) station = M.lookup station m
