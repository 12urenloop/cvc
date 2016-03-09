{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}


--------------------------------------------------------------------------------
import Data.Aeson
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as B
import           Data.Time                        (getCurrentTime)

import           Pipes
import qualified Pipes.Aeson.Unchecked                     as P
import qualified Pipes.Attoparsec                 as P
import qualified Pipes.ByteString                 as P
import qualified Pipes.Prelude as P


--------------------------------------------------------------------------------
import           SensorEvent                      (SensorEvent (..))
import           Types                            (Mac, parseMac)


--------------------------------------------------------------------------------
main :: IO ()
main = runEffect $ gyrids >-> P.mapM gyridToEvent >-> encodeJSON >-> P.stdout


--------------------------------------------------------------------------------
gyrids :: MonadIO m => Producer Gyrid m ()
gyrids = P.parsed gyrid P.stdin >-> catMaybes >> return ()


--------------------------------------------------------------------------------
encodeJSON :: (ToJSON a, Monad m) => Pipe a B.ByteString m r
encodeJSON = for cat $ P.encode


--------------------------------------------------------------------------------
gyridToEvent :: Gyrid -> IO SensorEvent
gyridToEvent (Event s b _) = do
    time <- getCurrentTime
    return $ SensorEvent time s b


--------------------------------------------------------------------------------
catMaybes :: Monad m => Pipe (Maybe a) a m r
catMaybes = for cat $ maybe (return ()) (yield)


--------------------------------------------------------------------------------
data Gyrid = Event Mac Mac Double
    deriving (Show)


--------------------------------------------------------------------------------
gyrid :: A.Parser (Maybe Gyrid)
gyrid = do
    line <- lineParser
    return $ case B.split ',' line of
        ("MSG" : _)     -> Nothing
        ("INFO" : _)    -> Nothing
        [!s, _, !b, !r] -> Just $
            Event (parseMac s) (parseMac b) (toDouble r)
        _ -> Nothing
    where
        newline x = x `B.elem` "\r\n"
        lineParser = A.skipWhile newline *> A.takeWhile (not . newline)
        toDouble = read . B.unpack
