--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Protocols.CSV
    ( csvInput
    , csvOutput
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative          ((*>), (<$>))
import           Control.Monad                (liftM)
import qualified Data.Attoparsec              as A
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BC
import           Data.Time                    (UTCTime, getCurrentTime)
import qualified System.IO.Streams            as Streams
import           System.IO.Streams.Attoparsec

--------------------------------------------------------------------------------
import           CountVonCount.Log
import           CountVonCount.RawSensorEvent
import           CountVonCount.Types

--------------------------------------------------------------------------------
csvOutput :: Streams.OutputStream B.ByteString -> IO ()
csvOutput _ = return ()

--------------------------------------------------------------------------------
csvInput :: Log
    -> Streams.InputStream B.ByteString
    -> IO (Streams.InputStream RawSensorEvent)
csvInput _ inStream = do
    gyridStream <- parserToInputStream (Just <$> gyrid) inStream
    Streams.mapM gToEvent gyridStream >>= Streams.mapMaybe id
        where gToEvent g = liftM (gyridToEvent g) getCurrentTime

--------------------------------------------------------------------------------
gyridToEvent :: Gyrid -> UTCTime -> Maybe RawSensorEvent
gyridToEvent Ignored _          = Nothing
gyridToEvent (Event s b d) time = Just $ RawSensorEvent time s b d

--------------------------------------------------------------------------------
data Gyrid
    = Event Mac Mac Double
    | Ignored
    deriving (Show)

--------------------------------------------------------------------------------
gyrid :: A.Parser Gyrid
gyrid = do
    line <- lineParser
    return $ case BC.split ',' line of
        ("MSG" : _)     -> Ignored
        ("INFO" : _)    -> Ignored
        [!s, _, !b, !r] ->
            Event (parseMac s) (parseMac b) (toDouble r)
        _ -> Ignored
    where
        newline x = x `B.elem` "\r\n"
        lineParser = A.skipWhile newline *> A.takeWhile (not . newline)
        toDouble = read . BC.unpack
