module Haskus.Tests.Format.Binary.Writer
  ( testsWriter
  )
where

import Prelude hiding (concat, replicate, take, drop)

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Haskus.Memory.Writer.SizedWriter
import Data.Bits

testsWriter :: TestTree
testsWriter = testGroup "Writer" $
   [ testProperty "writeU8 a == writeU8 a" $
         \a ->
          let w1 = writeU8 a
              w2 = writeU8 a
          in w1 `sizedWriterEq` w2
   , testProperty "writeU8 {a,b,c,d} == writeU32BE abcd" $
         \a b c d ->
          let w1 = writeU8 a <> writeU8 b <> writeU8 c <> writeU8 d
              w2 = writeU32BE ((fromIntegral a `shiftL` 24)
                      .|. (fromIntegral b `shiftL` 16)
                      .|. (fromIntegral c `shiftL` 8)
                      .|. (fromIntegral d))
          in w1 `sizedWriterEq` w2
   ]
