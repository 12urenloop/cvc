-- | Very high-level management utilities. These are usually called from within
-- the web application.
module CountVonCount.Management
    ( findBaton
    , assignBaton
    ) where

import Control.Monad.Trans (liftIO)
import Data.Foldable (forM_)
import Data.List (find)

import CountVonCount.Counter
import CountVonCount.Persistence
import CountVonCount.Types

findBaton :: Mac -> [Baton] -> Maybe Baton
findBaton mac = find ((== mac) . batonMac)

assignBaton :: Counter -> [Baton] -> Baton -> Ref Team -> IO ()
assignBaton counter batons baton teamRef = runPersistence $ do
    team <- get teamRef

    -- Reset the old baton, if needed
    forM_ (teamBaton team >>= flip findBaton batons) $ \b ->
        liftIO $ resetCounterFor b counter

    -- Reset the new baton
    liftIO $ resetCounterFor baton counter

    put teamRef $ team {teamBaton = Just (batonMac baton)}
