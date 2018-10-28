import Test.Tasty

import Variant

main :: IO ()
main = defaultMain $ testGroup "utils-variant"
   [ testsVariant
   ]
