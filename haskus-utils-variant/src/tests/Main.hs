import Test.Tasty

import Variant
import EADT

main :: IO ()
main = defaultMain $ testGroup "utils-variant"
  [ testsVariant
  , testsEADT
  ]
