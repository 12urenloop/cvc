--------------------------------------------------------------------------------
-- | Very high-level management utilities. These are usually called from within
-- the web application.
module CountVonCount.Management
    ( assignBaton
    , assignment
    , addBonus
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Control.Arrow             ((&&&))
import           Control.Monad.Trans       (liftIO)
import           Data.Foldable             (forM_)
import           Data.List                 (sort)
import qualified Data.Map                  as M
import           Data.Maybe                (mapMaybe)
import           Data.Text                 (Text)
import           Data.Time                 (getCurrentTime)
import           Prelude


--------------------------------------------------------------------------------
import           CountVonCount.Counter
import           CountVonCount.EventBase   (EventBase)
import qualified CountVonCount.EventBase   as EventBase
import           CountVonCount.Persistence


--------------------------------------------------------------------------------
assignBaton :: Database -> Counter -> Ref Baton -> Ref Team -> IO ()
assignBaton db counter baton teamRef = do
    team <- getTeam db teamRef

    -- Reset the old baton, if needed
    forM_ (teamBaton team) $ \b -> liftIO $ resetCounterFor b counter

    -- Reset the new baton
    liftIO $ resetCounterFor baton counter

    setTeamBaton db teamRef $ Just baton


--------------------------------------------------------------------------------
assignment :: Database -> IO ([(Team, Maybe Baton)], [Baton])
assignment db = do
    teams  <- sort <$> getAllTeams db
    batons <- sort <$> getAllBatons db
    let batonMap   = M.fromList $ map (batonId &&& id) batons
        withBatons = flip map teams $ \team ->
            (team, teamBaton team >>= flip M.lookup batonMap)
        freeBatons = map snd $ M.toList $ foldl (flip M.delete) batonMap $
            mapMaybe teamBaton teams

    return (withBatons, sort freeBatons)


--------------------------------------------------------------------------------
addBonus :: EventBase -> Database -> Ref Team -> Text
         -> Int -> IO ()
addBonus eventBase db ref reason laps = do
    timestamp <- liftIO getCurrentTime
    lapRef    <- addLaps db ref timestamp (Just reason) laps
    lap       <- getLap db lapRef
    team      <- getTeam db ref
    EventBase.publish eventBase $ LapEvent team lap
