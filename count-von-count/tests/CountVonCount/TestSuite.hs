import Test.Framework (defaultMain)

import qualified CountVonCount.Analyze.Tests
import qualified CountVonCount.Persistence.Tests

main :: IO ()
main = defaultMain
    [ CountVonCount.Analyze.Tests.tests
    , CountVonCount.Persistence.Tests.tests
    ]
