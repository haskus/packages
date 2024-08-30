module Haskus.Tests.Data where

import Test.Tasty

import Haskus.Tests.Data.Variant
import Haskus.Tests.Data.EADT

testsData :: TestTree
testsData = testGroup "data"
  [ testsVariant
  , testsEADT
  ]
