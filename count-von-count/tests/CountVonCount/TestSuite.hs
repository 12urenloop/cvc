import Test.Framework (defaultMain)

import qualified CountVonCount.Counter.Core.Tests
import qualified CountVonCount.Persistence.Tests

main :: IO ()
main = defaultMain
    [ CountVonCount.Counter.Core.Tests.tests
    , CountVonCount.Persistence.Tests.tests
    ]
