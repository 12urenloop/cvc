module Main
    ( main
    ) where

import CountVonCount.Analyze ()
import CountVonCount.Persistence ()
import qualified CountVonCount.Web as Web

main :: IO ()
main = do
    putStrLn "Hello world"
    Web.serve
