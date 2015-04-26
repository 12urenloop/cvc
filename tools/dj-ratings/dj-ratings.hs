--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative    ((<$>), (<*>))
import           Control.Monad          (forM, forM_)
import           Control.Monad.Error    (catchError)
import           Data.List              (partition, sortBy)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (catMaybes)
import           Data.Number.Erf        (invnormcdf)
import           Data.Ord               (comparing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (UTCTime, formatTime)
import           Database.SQLite.Simple (FromRow (..))
import qualified Database.SQLite.Simple as Sql
import           System.Environment     (getArgs)
import           System.Locale          (defaultTimeLocale)
import           Text.Printf            (printf)
import           Text.Regex.PCRE        ((=~))


--------------------------------------------------------------------------------
djs2015 :: [Dj]
djs2015 =
    -- TODO add a base rating by us?
    [ Dj "Semper Fi"                      1 (Time 11 35) (Time 12 35)
    , Dj "SRGN"                           2 (Time 12 35) (Time 13 35)
    , Dj "Thomassive"                     3 (Time 13 35) (Time 14 35)
    , Dj "B2B-Major"                      4 (Time 14 35) (Time 15 35)
    , Dj "Clatz & Helios"                 5 (Time 15 35) (Time 16 35)
    , Dj "Wasted On Sound"                6 (Time 16 35) (Time 17 35)

    , Dj "JEKA 1"                         7 (Time 18 00) (Time 18 35)
    , Dj "JEKA 2"                         8 (Time 18 35) (Time 19 10)
    , Dj "JEKA 3"                         9 (Time 19 10) (Time 19 45)
    , Dj "JEKA 4"                        10 (Time 19 45) (Time 20 20)
    , Dj "JEKA 5"                        11 (Time 20 20) (Time 20 55)
    ]


--------------------------------------------------------------------------------
data Time = Time Int Int deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
data Dj = Dj Text Int Time Time deriving (Show)


--------------------------------------------------------------------------------
instance Eq Dj where
    (Dj _ x _ _) == (Dj _ y _ _) = x == y


--------------------------------------------------------------------------------
instance Ord Dj where
    compare (Dj _ x _ _) (Dj _ y _ _) = compare x y


--------------------------------------------------------------------------------
ciLowerBound :: Double -> Int -> Double -> Double
ciLowerBound pos n confidence
    | n == 0    = 0
    | otherwise =
        (phat + z * z / (2 * n') -
            z * sqrt ((phat * (1 - phat) + z * z / (4 * n')) / n')) /
        (1 + z * z / n')
  where
    n'   = fromIntegral n
    z    = invnormcdf (1 - (1 - confidence) / 2)
    phat = pos / n'


--------------------------------------------------------------------------------
type Sender = Text


--------------------------------------------------------------------------------
timeFromUTCTime :: UTCTime -> Time
timeFromUTCTime t = case words (formatTime defaultTimeLocale "%H %M" t) of
    [h, m] -> Time (read h) (read m)
    hm     -> error $ "timeFromUTCTime: unexpected result: " ++ show hm


--------------------------------------------------------------------------------
data Sms = Sms
    { smsSender :: Sender
    , smsText   :: Text
    , smsTime   :: Time
    } deriving (Show)


--------------------------------------------------------------------------------
instance FromRow Sms where
    fromRow = Sms <$>
        Sql.field <*> Sql.field <*> (timeFromUTCTime <$> Sql.field)


--------------------------------------------------------------------------------
data HotNot = Hot | Not deriving (Eq, Show)


--------------------------------------------------------------------------------
data Vote = Vote
    { voteSender :: Sender
    , voteDj     :: Dj
    , voteHotNot :: HotNot
    } deriving (Show)


--------------------------------------------------------------------------------
voteFromSms :: [Dj] -> Sms -> Either String Vote
voteFromSms djs (Sms sender text time) =
    case T.unpack (T.toLower text) =~ ("(hot|not)\\s*(\\d*)" :: String) of
        ([_, hotnot, i] : _) -> do
            hn <- case hotnot of
                "hot" -> Right Hot
                "not" -> Right Not
                _     -> Left $ "No hot/not: " ++ hotnot
            dj <- flip catchError (const djByTime) $ djByNr (T.pack i)
            return $ Vote sender dj hn
        _ -> Left $ "Couldn't parse: " ++ T.unpack text
  where
    djByNr :: Text -> Either String Dj
    djByNr n = case [dj | dj@(Dj _ i _ _) <- djs, show i == T.unpack n] of
        (dj : _) -> Right dj
        _        -> Left $ "DJ not found: " ++ T.unpack n

    djByTime = case [dj | dj@(Dj _ _ x y) <- djs, time >= x, time < y] of
        (dj : _) -> Right dj
        _        -> Left $ "No DJ playing at: " ++ show time


--------------------------------------------------------------------------------
data Rating = Rating
    { ratingCilb :: Double
    , ratingHots :: Int
    , ratingNots :: Int
    } deriving (Show)


--------------------------------------------------------------------------------
rateDjs :: [Dj] -> [Vote] -> [(Dj, Rating)]
rateDjs djs votes = reverse $ sortBy (comparing (ratingCilb . snd))
    [(dj, rating hotnots) | (dj, hotnots) <- M.toList perDj]
  where
    perDj :: Map Dj (Map Sender HotNot)
    perDj = M.fromListWith M.union $
        [ (dj, M.singleton sender hotnot)
        | Vote sender dj hotnot <- votes
        ] ++
        [ (dj, M.empty) | dj <- djs]  -- Makes sure the DJ is there even if
                                      -- there are no votes

    rating :: Map Sender HotNot -> Rating
    rating hotnots =
        let (hs, ns)   = partition (== Hot) $ map snd $ M.toList hotnots
            (hs', ns') = (length hs, length ns)
        in Rating
            { ratingCilb = ciLowerBound (fromIntegral hs') (hs' + ns') 0.95
            , ratingHots = hs'
            , ratingNots = ns'
            }


--------------------------------------------------------------------------------
ratings :: FilePath -> IO ()
ratings filePath = do
    conn <- Sql.open filePath

    -- Get SMSs
    smss <- Sql.query_ conn
        "SELECT SenderNumber, TextDecoded, ReceivingDateTime FROM inbox"

    -- Parse into votes
    votes <- fmap catMaybes $ forM smss $ \s -> case voteFromSms djs2015 s of
        Left err -> putStrLn ("Warning: " ++ err) >> return Nothing
        Right v  -> do
            putStrLn $ (T.unpack $ smsText s) ++ " => " ++
                show (voteHotNot v) ++ " " ++ (show $ voteDj v)
            return (Just v)

    -- Calculate and print rating
    let rating = zip [1 :: Int ..] $ rateDjs djs2015 votes
    forM_ rating $ \(i, (Dj name _ _ _, r)) ->
        putStrLn $ printf "%2d: %s (%.3f: %d HOT, %d NOT)"
            i (T.unpack name) (ratingCilb r) (ratingHots r) (ratingNots r)

    Sql.close conn


--------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    case args of
        (x : _) -> ratings x
        _       -> ratings "/var/spool/gammu/db.sqlite"
