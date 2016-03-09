{-# LANGUAGE BangPatterns #-}

import qualified Data.Aeson                    as JSON
import           Control.Monad                 (liftM)
{- import           Control.Applicative           ((<$>)) -}
import qualified Data.Attoparsec               as A
import qualified Data.ByteString               as B
import           Data.Time                     (UTCTime, getCurrentTime)
import qualified System.IO.Streams             as Streams
import           System.IO.Streams.Handle      (stdin, stdout)
import qualified System.IO.Streams.Combinators as Streams
import           System.IO.Streams.Attoparsec  (parserToInputStream)
--------------------------------------------------------------------------------
import           CountVonCount.Translator.SensorEvent  (SensorEvent(..))
import           CountVonCount.Types                   (Mac, parseMac)

main :: IO ()
main = do
    sensorEventStream <- csvInput stdin
    outputStream <- Streams.map JSON.encode sensorEventStream
    Streams.supply outputStream stdout

--------------------------------------------------------------------------------
instance JSON.ToJSON SensorEvent

--------------------------------------------------------------------------------
csvInput :: Streams.InputStream B.ByteString
    -> IO (Streams.InputStream SensorEvent)
csvInput inStream = do
    gyridStream <- parserToInputStream (Just <$> gyrid) inStream
    Streams.mapM gToEvent gyridStream >>= Streams.mapMaybe id
        where gToEvent g = liftM (gyridToEvent g) getCurrentTime

--------------------------------------------------------------------------------
gyridToEvent :: Gyrid -> UTCTime -> Maybe SensorEvent
gyridToEvent Ignored       _    = Nothing
gyridToEvent (Event s b d) time = Just $ SensorEvent time s b d

--------------------------------------------------------------------------------
data Gyrid
    = Event Mac Mac Double
    | Ignored
    deriving (Show)

--------------------------------------------------------------------------------
gyrid :: A.Parser Gyrid
gyrid = do
    line <- lineParser
    return $ case B.split ',' line of
        ("MSG" : _)     -> Ignored
        ("INFO" : _)    -> Ignored
        [!s, _, !b, !r] ->
            Event (parseMac s) (parseMac b) (toDouble r)
        _ -> Ignored
    where
        newline x = x `B.elem` "\r\n"
        lineParser = A.skipWhile newline *> A.takeWhile (not . newline)
        toDouble = read . B.unpack
