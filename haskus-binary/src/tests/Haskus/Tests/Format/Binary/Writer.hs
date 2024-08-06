module Haskus.Tests.Format.Binary.Writer
  ( testsWriter
  )
where

import Prelude hiding (concat, replicate, take, drop)

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Haskus.Memory.Writer
import Data.Bits

testsWriter :: TestTree
testsWriter = testGroup "Writer" $
   [ testProperty "swriteU8 a == swriteU8 a" $
         \a ->
          let w1 = swriteU8 a
              w2 = swriteU8 a
          in w1 `sizedWriterEq` w2
   , testProperty "swriteU8 {a,b,c,d} == swriteU32BE abcd" $
         \a b c d ->
          let w1 = swriteU8 a <> swriteU8 b <> swriteU8 c <> swriteU8 d
              w2 = swriteU32BE ((fromIntegral a `shiftL` 24)
                      .|. (fromIntegral b `shiftL` 16)
                      .|. (fromIntegral c `shiftL` 8)
                      .|. (fromIntegral d))
          in w1 `sizedWriterEq` w2
   ]
