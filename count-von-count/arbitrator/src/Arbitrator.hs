{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Data.Aeson

import           Pipes
import qualified Pipes.Aeson               as A
import qualified Pipes.Aeson.Unchecked     as AU
import qualified Pipes.Attoparsec          as P
import qualified Pipes.ByteString          as P (stdin, stdout)
import qualified Pipes.Lift                as P
import qualified Pipes.Prelude             as P

import           Control.Monad.State
import           Control.Monad.Trans.Error
import qualified Data.ByteString           as BS
import           Data.List                 (intercalate)
import           Lens.Family2              (view)
import           System.Exit
import           System.IO                 (hPutStrLn, stderr)

import           Counter.Core
import           Counter.Map
import           Sighting

main :: IO ()
main = do
    res <- runArbitratorT emptyCounterMap . runEffect . P.runErrorP $ count
    case res of
      Right _ -> exitSuccess
      Left (e, _) -> do
          hPutStrLn stderr $ case e of
            A.AttoparsecError (P.ParsingError ctx msg) ->
                intercalate ": " $ ctx ++ [msg]
            A.FromJSONError msg -> msg
          exitFailure


type Err m = ErrorT (A.DecodingError, Producer BS.ByteString m ()) m


observations :: MonadIO m => Producer Sighting (Err (ArbitratorT m)) ()
observations = P.errorP $ view AU.decoded P.stdin



count :: MonadIO m => Effect (Err (ArbitratorT m)) ()
count = observations
        >-> P.mapM (lift . step)
        >-> P.concat
        >-> for cat  (AU.encode . toJSON)
        >-> P.stdout


newtype ArbitratorT m a = ArbitratorT (StateT CounterMap m a)
    deriving (Functor, Applicative, Monad, MonadState CounterMap, MonadIO)


runArbitratorT :: Monad m => CounterMap -> ArbitratorT m a -> m a
runArbitratorT counter (ArbitratorT a) = evalStateT a counter


step :: MonadIO m => Sighting -> ArbitratorT m [CounterCoreEvent]
step sighting = do
    counterMap <- get
    let (events, tells, map') = stepCounterMap maxSpeed sighting counterMap
    put map'
    liftIO $ mapM_ (hPutStrLn stderr) tells
    return events


-- TODO
maxSpeed :: Double
maxSpeed = 1
