{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module CountVonCount.Persistence.Core
    ( Persistence
    , runPersistence

    , Ref
    , refToString
    , refFromString

    , IsDocument (..)
    , put
    , get
    , add
    , getAll

    , deleteAll
    ) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad (void)
import Control.Monad.Trans (MonadIO, liftIO)

import qualified Database.MongoDB as MDB

type Persistence = MDB.Action IO

runPersistence :: MonadIO m => Persistence a -> m a
runPersistence p = liftIO $ do
    -- TODO: show a more descriptive error message when failing?
    -- TODO: pool and re-use connections
    pipe <- MDB.runIOE $ MDB.connect $ MDB.host "127.0.0.1"
    x    <- MDB.access pipe MDB.master "count-von-count" p
    MDB.close pipe
    return $ either (error . show) id x

type Ref a = MDB.Value

refToString :: Ref a -> String
refToString = show

refFromString :: String -> Ref a
refFromString = MDB.ObjId . read

class IsDocument a where
    collection   :: a -> MDB.Collection
    toDocument   :: a -> MDB.Document
    fromDocument :: MDB.Document -> a

add :: IsDocument d => d -> Persistence (Ref d)
add x = MDB.insert (collection x) $ toDocument x

put :: IsDocument d => Ref d -> d -> Persistence ()
put r x = void $ MDB.save (collection x) $ ("_id"  MDB.:= r) : toDocument x

get :: forall d. IsDocument d => Ref d -> Persistence d
get r = fromDocument <$>
    MDB.fetch (MDB.select ["_id" MDB.:= r] (collection x))
  where
    x = undefined :: d

getAll :: forall d. IsDocument d => Persistence [(Ref d, d)]
getAll = do
    cursor <- MDB.find $ MDB.select [] $ collection x
    docs   <- MDB.rest cursor
    return $ map (MDB.valueAt "_id" &&& fromDocument) docs
  where
    x = undefined :: d

-- | Use with extreme care
deleteAll :: forall d. IsDocument d => d -> Persistence ()
deleteAll x = do
    _ <- MDB.dropCollection $ collection x
    return ()
