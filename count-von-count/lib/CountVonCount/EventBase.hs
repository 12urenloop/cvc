--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module CountVonCount.EventBase
    ( EventBase
    , newEventBase
    , subscribe
    , publish
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)
import           Data.IORef          (IORef, atomicModifyIORef, newIORef,
                                      readIORef)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Typeable       (TypeRep, Typeable, cast, typeOf)
import           Prelude


--------------------------------------------------------------------------------
import           CountVonCount.Log   (Log)
import qualified CountVonCount.Log   as Log
import           CountVonCount.Util


--------------------------------------------------------------------------------
data Handler = forall a. Typeable a => Handler String (a -> IO ())


--------------------------------------------------------------------------------
data EventBase = EventBase
    { eventBaseLog       :: Log
    , eventBaseListeners :: IORef (Map TypeRep [Handler])
    }


--------------------------------------------------------------------------------
newEventBase :: Log -> IO EventBase
newEventBase logger = EventBase logger <$> newIORef M.empty


--------------------------------------------------------------------------------
subscribe :: forall a. Typeable a
          => EventBase -> String -> (a -> IO ()) -> IO ()
subscribe eb name f = atomicModifyIORef (eventBaseListeners eb) $ \m ->
    (M.insertWith (++) trep [Handler name f] m, ())
  where
    trep = typeOf (undefined :: a)


--------------------------------------------------------------------------------
publish :: Typeable a => EventBase -> a -> IO ()
publish eb x = do
    listeners <- readIORef $ eventBaseListeners eb
    case M.lookup (typeOf x) listeners of
        Nothing       -> Log.string (eventBaseLog eb)
            "CountVonCount.EventBase.publish" $
            "Warning: no event handlers registered for type " ++ show (typeOf x)
        Just handlers -> forM_ handlers $ \(Handler name f) -> case cast x of
            Nothing -> return ()  -- Should never happen
            Just x' -> isolate_ (eventBaseLog eb) ("Handler " ++ name) (f x')
