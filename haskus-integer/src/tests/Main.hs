{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "MachDeps.h"

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Haskus.Number.Natural
import Data.Bits

instance Arbitrary Natural where
   arbitrary = fmap (fromInteger . abs) arbitrary
   shrink n  = [n `shiftR` WORD_SIZE_IN_BITS]

newtype BigNatural = BigNatural Natural deriving Show

instance Arbitrary BigNatural where
   arbitrary = fmap (BigNatural . naturalFromLimbsMS) arbitrary
   shrink (BigNatural n)  = [BigNatural (n `shiftR` WORD_SIZE_IN_BITS)]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
   [ QC.testProperty "x - x == naturalFromWord 0" $
      \x -> x - x == naturalFromWord 0

   , QC.testProperty "BIG: x - x == naturalFromWord 0" $
      \(BigNatural x) -> x - x == naturalFromWord 0

   , QC.testProperty "naturalFromWord x << WS == naturalFromLimbsMS [x,0]" $
      \x -> naturalFromWord x `shiftL` WORD_SIZE_IN_BITS == naturalFromLimbsMS [x,0]

   , QC.testProperty "naturalFromWord x >> WS == 0" $
      \x -> naturalFromWord x `shiftR` WORD_SIZE_IN_BITS == 0

   , QC.testProperty "Popcount x = sum (fmap popCount (limbs x))" $
      \xs -> naturalPopCount (naturalFromLimbsMS xs) == fromIntegral (sum (fmap popCount xs))

   , QC.testProperty "x*y = x+x+..+x" $
      \x y -> (x :: Natural) * naturalFromWord (fromIntegral (abs y)) == sum (replicate (abs y) x)

   , QC.testProperty "x*y = y*x" $
      \x y -> (x :: Natural) * y == y * x

   , QC.testProperty "BIG-SMALL: x*y = y*x" $
      \(BigNatural x) y -> x * y == y * x

   , QC.testProperty "BIG: x*y = y*x" $
      \(BigNatural x) (BigNatural y) -> x * y == y * x

   , QC.testProperty "x+y = y+x" $
      \x y -> (x :: Natural) + y == y + x

   , QC.testProperty "BIG-SMALL: x+y = y+x" $
      \(BigNatural x) y -> x + y == y + x

   , QC.testProperty "SMALL-BIG: x+y = y+x" $
      \x (BigNatural y) -> x + y == y + x

   , QC.testProperty "BIG: x+y = y+x" $
      \(BigNatural x) (BigNatural y) -> x + y == y + x

   , QC.testProperty "Round-trip to Integer" $
      \x -> naturalFromInteger (naturalToInteger x) == x

   , QC.testProperty "BIG: round-trip to Integer" $
      \(BigNatural x) -> naturalFromInteger (naturalToInteger x) == x

   , QC.testProperty "x+y {Integer} ~ x+y {Natural}" $
      \x y -> x + y == naturalFromInteger (naturalToInteger x + naturalToInteger y)

   , QC.testProperty "BIG: x+y {Integer} ~ x+y {Natural}" $
      \(BigNatural x) (BigNatural y) -> x + y == naturalFromInteger (naturalToInteger x + naturalToInteger y)

   , QC.testProperty "x*y {Integer} ~ x*y {Natural}" $
      \x y -> x * y == naturalFromInteger (naturalToInteger x * naturalToInteger y)

   , QC.testProperty "BIG: x*y {Integer} ~ x*y {Natural}" $
      \(BigNatural x) (BigNatural y) -> x * y == naturalFromInteger (naturalToInteger x * naturalToInteger y)
   ]

