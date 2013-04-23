--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative    ((<$>), (<*>))
import           Control.Monad          (forM, forM_)
import           Control.Monad.Error    ()
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
import           System.Locale          (defaultTimeLocale)
import           Text.Printf            (printf)


--------------------------------------------------------------------------------
djs2013 :: [Dj]
djs2013 =
    -- TODO add a base rating by us?
    [ Dj "Bassick"      1 (Time 10 30) (Time 11 30)
    , Dj "LSGHN"        2 (Time 11 30) (Time 12 30)
    , Dj "Oscar Mike"   3 (Time 12 30) (Time 13 30)
    , Dj "Pulp Mixion"  4 (Time 13 30) (Time 14 30)
    , Dj "Supernovazz"  5 (Time 14 30) (Time 15 30)
    , Dj "Brains Off"   6 (Time 15 30) (Time 16 30)
    , Dj "Cultivar"     7 (Time 16 30) (Time 17 30)
    , Dj "Blueshades"   8 (Time 17 30) (Time 18 30)
    , Dj "Louis Gara"   9 (Time 18 30) (Time 19 30)
    , Dj "Dropzilla"   10 (Time 19 30) (Time 20 30)
    , Dj "DJ Ace"      11 (Time 20 30) (Time 21 30)
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
data Sms = Sms Sender Text Time deriving (Show)


--------------------------------------------------------------------------------
instance FromRow Sms where
    fromRow = Sms <$>
        Sql.field <*> Sql.field <*> (timeFromUTCTime <$> Sql.field)


--------------------------------------------------------------------------------
data HotNot = Hot | Not deriving (Eq, Show)


--------------------------------------------------------------------------------
data Vote = Vote Sender Dj HotNot
    deriving (Show)


--------------------------------------------------------------------------------
voteFromSms :: [Dj] -> Sms -> Either String Vote
voteFromSms djs (Sms sender text time) = case T.words (T.toLower text) of
    ["hot", i] -> djByNr i >>= \dj -> Right (Vote sender dj Hot)
    ["not", i] -> djByNr i >>= \dj -> Right (Vote sender dj Not)
    ["hot"]    -> djByTime >>= \dj -> Right (Vote sender dj Hot)
    ["not"]    -> djByTime >>= \dj -> Right (Vote sender dj Not)
    _          -> Left $ "Couldn't parse sms: " ++ T.unpack text
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
main :: IO ()
main = do
    conn <- Sql.open "/var/spool/gammu/db.sqlite"

    -- Get SMSs
    smss <- Sql.query_ conn
        "SELECT SenderNumber, TextDecoded, ReceivingDateTime FROM inbox"

    -- Parse into votes
    votes <- fmap catMaybes $ forM smss $ \s -> case voteFromSms djs2013 s of
        Left err -> putStrLn ("Warning: " ++ err) >> return Nothing
        Right v  -> return (Just v)

    -- Calculate and print rating
    let rating = zip [1 :: Int ..] $ rateDjs djs2013 votes
    forM_ rating $ \(i, (Dj name _ _ _, r)) ->
        putStrLn $ printf "%2d: %s (%.3f: %d HOT, %d NOT)"
            i (T.unpack name) (ratingCilb r) (ratingHots r) (ratingNots r)

    Sql.close conn
