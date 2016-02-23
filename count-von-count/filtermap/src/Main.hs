{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson            (decodeStrict)
import qualified Data.ByteString       as BS
import qualified Data.HashMap.Strict   as M
import qualified Data.Text             as T
import           Lens.Family2          (over)
import           Pipes
import           Pipes.Aeson.Unchecked (decoded)
import qualified Pipes.ByteString      as P (stdin, stdout)
import           System.Environment


type Obj = M.HashMap T.Text T.Text

main :: IO ()
main = do
    [key, target, file] <- getArgs
    Just obj <- decodeStrict <$> BS.readFile file
    let f = filterMap (T.pack key) (T.pack target) obj
    runEffect $ over decoded (>-> mapMaybe f) P.stdin >-> P.stdout

filterMap :: T.Text -> T.Text -> Obj -> Obj -> Maybe Obj
filterMap key target mapping obj = do
    newValue <- M.lookup key obj >>= flip M.lookup mapping
    return $ M.insert target newValue $ M.delete key obj

mapMaybe :: Monad m => (a -> Maybe b) -> Pipe a b m r
mapMaybe f = for cat $ maybe (return ()) yield . f
