{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson

import           Pipes
import qualified Pipes.Aeson.Unchecked as A
import qualified Pipes.ByteString      as P (stdin, stdout)
import qualified Pipes.Prelude         as P

import           Control.Monad.State
import           Lens.Family2          (view)

import           Counter.Core
import           Counter.Map
import           Sighting


main :: IO ()
main = runArbitratorT emptyCounterMap $ runEffect count


observations :: MonadIO m => Producer Sighting m ()
observations = view A.decoded P.stdin >> return ()



count :: MonadIO m => Effect (ArbitratorT m) ()
count = observations
        >-> P.mapM step
        >-> P.concat
        >-> for cat  (A.encode . toJSON)
        >-> P.stdout


newtype ArbitratorT m a = ArbitratorT (StateT CounterMap m a)
    deriving (Functor, Applicative, Monad, MonadState CounterMap, MonadIO)

runArbitratorT :: Monad m => CounterMap -> ArbitratorT m a -> m a
runArbitratorT counter (ArbitratorT a) = evalStateT a counter



-- TODO: tells
step :: Monad m => Sighting -> ArbitratorT m [CounterCoreEvent]
step sighting = do
    counterMap <- get
    let (events, tells, map') = stepCounterMap maxSpeed sighting counterMap
    put map'
    return events

-- TODO
maxSpeed :: Double
maxSpeed = 1
