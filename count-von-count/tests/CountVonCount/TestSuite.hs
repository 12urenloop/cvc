import Test.Framework (defaultMain)

import qualified CountVonCount.Counter.Tests
import qualified CountVonCount.Counter.Core.Tests
import qualified CountVonCount.Persistence.Tests
import qualified CountVonCount.Sensor.Tests
import qualified CountVonCount.Sensor.Filter.Tests
import qualified CountVonCount.Util.Tests

main :: IO ()
main = defaultMain
    [ CountVonCount.Counter.Tests.tests
    , CountVonCount.Counter.Core.Tests.tests
    , CountVonCount.Persistence.Tests.tests
    , CountVonCount.Sensor.Filter.Tests.tests
    , CountVonCount.Sensor.Tests.tests
    , CountVonCount.Util.Tests.tests
    ]
