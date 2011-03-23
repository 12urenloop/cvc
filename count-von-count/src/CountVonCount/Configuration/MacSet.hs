-- | Set of allowed mac addresses
--
module CountVonCount.Configuration.MacSet
    ( loadMacSet
    ) where

import Data.Set (Set)
import qualified Data.Set as S

import Data.Object (fromSequence, fromScalar)
import Data.Object.Yaml (YamlObject, fromYamlScalar)

import CountVonCount.Types

loadMacSet :: YamlObject -> Maybe (Set Mac)
loadMacSet object = do
    scalars <- mapM fromScalar =<< fromSequence object
    return $ S.fromList $ map fromYamlScalar scalars 
