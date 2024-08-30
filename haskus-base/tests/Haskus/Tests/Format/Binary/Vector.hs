{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Haskus.Tests.Format.Binary.Vector
   ( testsVector
   )
where

import Prelude hiding (concat, replicate, take, drop)

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Haskus.Utils.Maybe
import Haskus.Utils.HList
import Haskus.Binary.Vector
import Haskus.Number.Word
import Haskus.Binary.Bits

v1234 :: Vector 4 Word32
v1234 = fromJust $ fromList [1,2,3,4]

v567 :: Vector 3 Word32
v567 = fromJust $ fromList [5,6,7]

testsVector :: TestTree
testsVector = testGroup "Vector" $
   [ testProperty "toList . fromList == id" $
         toList v1234 == [1,2,3,4]

   , testProperty "toList (fromList []) == []" $
         toList (fromJust (fromList []) :: Vector 0 Word64) == []

   , testProperty "fromFilledList: shorter input" $
         toList (fromFilledList 5 [1,2,3,4] :: Vector 8 Word32) == [1,2,3,4,5,5,5,5]

   , testProperty "fromFilledList: longer input" $
         toList (fromFilledList 5 [1,2,3,4] :: Vector 3 Word32) == [1,2,3]

   , testProperty "fromFilledList: equal input" $
         toList (fromFilledList 5 [1,2,3,4] :: Vector 4 Word32) == [1,2,3,4]

   , testProperty "fromFilledListZ: shorter input" $
         toList (fromFilledListZ 5 [1,2,3,4] :: Vector 8 Word32) == [1,2,3,4,5,5,5,5]

   , testProperty "fromFilledListZ: longer input" $
         toList (fromFilledListZ 5 [1,2,3,4] :: Vector 3 Word32) == [1,2,5]

   , testProperty "fromFilledListZ: equal input" $
         toList (fromFilledListZ 5 [1,2,3,4] :: Vector 4 Word32) == [1,2,3,5]

   , testProperty "take less" $
         toList (take @2 v1234) == [1,2]

   , testProperty "take equal" $
         toList (take @4 v1234) == [1,2,3,4]

   , testProperty "drop less" $
         toList (drop @2 v1234) == [3,4]

   , testProperty "drop equal" $
         toList (drop @4 v1234) == []

   , testProperty "index" $
         index @2 v1234 == 3

   , testProperty "replicate" $
         toList (replicate 5 :: Vector 4 Word32) == [5,5,5,5]

   , testProperty "concat two vectors" $
         toList (concat (v1234 `HCons` v567 `HCons` HNil)) == [1,2,3,4,5,6,7]

   , testProperty "concat 4 vectors" $
         toList (concat (v1234 `HCons` v567 `HCons` v567 `HCons` v1234 `HCons` HNil)) == [1,2,3,4,5,6,7,5,6,7,1,2,3,4]

   , testProperty "rotateR/rotateL" $
         (\n -> (v1234 `rotateR` n) `rotateL` n == v1234)

   , testProperty "bit/testBit" $
         (\n -> n >= 320 || testBit (bit n :: Vector 10 Word32) n)
   ]
