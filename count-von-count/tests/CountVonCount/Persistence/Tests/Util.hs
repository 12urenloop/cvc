--------------------------------------------------------------------------------
module CountVonCount.Persistence.Tests.Util
    ( testDatabase
    ) where


--------------------------------------------------------------------------------
import           CountVonCount.Persistence


--------------------------------------------------------------------------------
testDatabase :: (Database -> IO a) -> IO a
testDatabase f = do
    db <- newDatabase ":memory:"
    r  <- f db
    deleteAll db
    return r
