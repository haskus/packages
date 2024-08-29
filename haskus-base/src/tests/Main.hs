import Test.Tasty

import Haskus.Tests.Format.Binary
import Haskus.Tests.Data
import Haskus.Tests.Utils

main :: IO ()
main = defaultMain $ testGroup "haskus"
  [ testsBinary
  , testsData
  , testsUtils
  ]
