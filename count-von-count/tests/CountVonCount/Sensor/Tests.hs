--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Sensor.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            ((<$>))
import           Control.Concurrent             (forkIO, killThread,
                                                 threadDelay)
import           Control.Monad                  (forM_)
import           System.IO                      (hClose, hFlush, hPutStrLn)


--------------------------------------------------------------------------------
import           Data.IORef                     (atomicModifyIORef, newIORef,
                                                 readIORef)
import           Network                        (PortID (..), connectTo)
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assert)


--------------------------------------------------------------------------------
import           CountVonCount.EventBase
import qualified CountVonCount.Log              as Log
import           CountVonCount.Sensor
import           CountVonCount.Protocols.CSV


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "CountVonCount.Sensor.Tests"
    [ testCase "sensor listen" $ assert sensorTest
    ]


--------------------------------------------------------------------------------
sensorTest :: IO Bool
sensorTest = do
    logger    <- Log.open "/dev/null" False
    ref       <- newIORef []
    eventBase <- newEventBase logger
    subscribe eventBase "prepend handler" $ \raw ->
        atomicModifyIORef ref $ \rs -> (raw : rs, ())

    threadId <- forkIO $ listen csv logger eventBase port
    threadDelay 100000
    handle <- connectTo "127.0.0.1" (PortNumber $ fromIntegral port)
    forM_ inputs $ \i -> hPutStrLn handle i >> hFlush handle
    hClose handle
    threadDelay 100000
    outputs <- reverse <$> readIORef ref
    killThread threadId
    return $ and $ zipWith (=~=) outputs expected
  where
    port = 12390

    -- Don't take time into account here
    (RawSensorEvent _ s1 b1 r1) =~= (RawSensorEvent _ s2 b2 r2) =
        s1 == s2 && b1 == b2 && abs (r1 - r2) < 0.001

    inputs =
        [ "000000000000,herp,100000000000,10"
        , "So I asked my north korean friend how his life was going."
        , "000000000000,derp,200000400000,-80"
        , "He said: can't complain."
        ]

    expected =
        [ RawSensorEvent undefined "00:00:00:00:00:00" "10:00:00:00:00:00" 10
        , RawSensorEvent undefined "00:00:00:00:00:00" "20:00:00:40:00:00" (-80)
        ]
