import Test.Framework (defaultMain)

import qualified CountVonCount.Analyze.Tests

main :: IO ()
main = defaultMain
    [ CountVonCount.Analyze.Tests.tests
    ]
