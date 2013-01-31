--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           CountVonCount.Persistence


--------------------------------------------------------------------------------
main :: IO ()
main = do
    db <- newDatabase "count-von-count.db"
    deleteAll db
    addStation db "station 1" "00:00:00:00:01:00" 0
    addStation db "station 2" "00:00:00:00:02:00" 100
    addStation db "station 3" "00:00:00:00:03:00" 200
    addStation db "station 4" "00:00:00:00:04:00" 300
    closeDatabase db
