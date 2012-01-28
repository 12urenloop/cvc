#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Data.Time (addUTCTime, getCurrentTime)

import qualified Data.Text as T

import CountVonCount.Persistence

main :: IO ()
main = runPersistence $ do
    teams <- getAll :: Persistence [(Ref Team, Team)]
    now   <- liftIO getCurrentTime
    forM_ [1 :: Int .. 100] $ \num ->
        forM_ (map fst teams) $ \ref -> do
            let timestamp = fromIntegral num `addUTCTime` now
                reason    = "Herp derp " `T.append` T.pack (show num)
            addLaps ref timestamp reason 1
