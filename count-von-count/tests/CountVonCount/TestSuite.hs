import Test.Framework (defaultMain)

import qualified CountVonCount.Counter.Core.Tests
import qualified CountVonCount.Persistence.Tests
import qualified CountVonCount.Sensor.Tests
import qualified CountVonCount.Sensor.Filter.Tests

main :: IO ()
main = defaultMain
    [ CountVonCount.Counter.Core.Tests.tests
    , CountVonCount.Persistence.Tests.tests
    , CountVonCount.Sensor.Tests.tests
    , CountVonCount.Sensor.Filter.Tests.tests
    ]
