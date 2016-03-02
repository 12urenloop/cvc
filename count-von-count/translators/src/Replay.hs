{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as BS
import           Data.Text
import           Data.Text.Encoding
import           Data.Time
import           System.Exit                      (exitFailure)
import           System.IO                        (hPutStrLn, stderr)

import           Pipes
import qualified Pipes.Attoparsec                 as P
import qualified Pipes.ByteString                 as P
import           Pipes.Lift

import           Observation

main :: IO ()
main = do
    res <- runEffect . runErrorP $
        errorP (P.parsed observation P.stdin)
        >-> encodeObjects
        >-> P.stdout
    case res of
      Right _     -> return ()
      Left (e, _) -> hPutStrLn stderr (P.peMessage e) >> exitFailure

encodeObjects :: (ToJSON a, Monad m) => Pipe a BS.ByteString m r
encodeObjects = for cat (P.fromLazy . encode)

observation :: A.Parser Observation
observation = Observation <$> time  <* A.char ','
                          <*  field <* A.char ','
                          <*> text  <* A.char ','
                          <*  field <* A.char ','
                          <*> text  <* A.char ','
                          <*  field <* A.char ','
                          <*  field <* A.char '\n'

field :: A.Parser BS.ByteString
field = A.takeWhile (`BS.notElem` ",\r\n")

text :: A.Parser Text
text = decodeUtf8 <$> field

time :: A.Parser UTCTime
time = do
    str <- BS.unpack <$> field
    case parseTimeM True defaultTimeLocale "%H:%M:%S" str of
        Just res -> return res
        Nothing  -> fail "invalid timestamp"
