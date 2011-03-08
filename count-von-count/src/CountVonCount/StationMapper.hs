-- | Module that maps station names to actual positions
--
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module CountVonCount.StationMapper
    ( StationMapper
    , mapStation
    ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero, forM)
import Data.Monoid (Monoid)
import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Text as T
import Data.Aeson (FromJSON (..), Value (..))

import CountVonCount.Types

newtype StationMapper = StationMapper (Map Station Position)
                      deriving (Show, Monoid)

instance FromJSON StationMapper where
    parseJSON (Object m) = fmap (StationMapper . M.fromList) $
        forM (M.toList m) $ \(s, p) -> (,) <$> pure (T.unpack s) <*> parseJSON p
    parseJSON _ = mzero

mapStation :: StationMapper -> Station -> Maybe Position
mapStation (StationMapper m) station = M.lookup station m
