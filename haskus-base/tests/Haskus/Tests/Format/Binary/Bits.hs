{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskus.Tests.Format.Binary.Bits 
   ( testsBits
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck (Arbitrary(..),testProperty)
import Test.QuickCheck.Gen (elements,choose,vectorOf)

import Haskus.Tests.Common
import Haskus.Utils.Flow

import Haskus.Binary.Bits.Put
import Haskus.Binary.Bits.Get
import Haskus.Binary.Bits.Order
import Haskus.Binary.Bits.Reverse
import Haskus.Binary.Bits

import Haskus.Binary.Buffer
import Haskus.Binary.Get
import Haskus.Binary.Put
import Haskus.Number.VariableLength
import Haskus.Number.Word
import Haskus.Number.Int

testsBits :: TestTree
testsBits = testGroup "Binary bits" $
   [ testGroup "Finite"
      [ testGroup "Finite bitSize"
         [ testProperty "bitSize Word8"  (bitSize (5 :: Word8)  == (8 :: Word))
         , testProperty "bitSize Word16" (bitSize (5 :: Word16) == (16 :: Word))
         , testProperty "bitSize Word32" (bitSize (5 :: Word32) == (32 :: Word))
         , testProperty "bitSize Word64" (bitSize (5 :: Word64) == (64 :: Word))
         , testProperty "bitSize Int8"   (bitSize (5 :: Int8)   == (8 :: Word))
         , testProperty "bitSize Int16"  (bitSize (5 :: Int16)  == (16 :: Word))
         , testProperty "bitSize Int32"  (bitSize (5 :: Int32)  == (32 :: Word))
         , testProperty "bitSize Int64"  (bitSize (5 :: Int64)  == (64 :: Word))
         ]
      , testGroup "Finite zeroBits"
         [ testProperty "zeroBits Word8"  ((zeroBits :: Word8)  == 0)
         , testProperty "zeroBits Word16" ((zeroBits :: Word16) == 0)
         , testProperty "zeroBits Word32" ((zeroBits :: Word32) == 0)
         , testProperty "zeroBits Word64" ((zeroBits :: Word64) == 0)
         , testProperty "zeroBits Int8"   ((zeroBits :: Int8)   == 0)
         , testProperty "zeroBits Int16"  ((zeroBits :: Int16)  == 0)
         , testProperty "zeroBits Int32"  ((zeroBits :: Int32)  == 0)
         , testProperty "zeroBits Int64"  ((zeroBits :: Int64)  == 0)
         ]
      , testGroup "Finite oneBits"
         [ testProperty "oneBits Word8"  ((oneBits :: Word8)  == 0xff)
         , testProperty "oneBits Word16" ((oneBits :: Word16) == 0xffff)
         , testProperty "oneBits Word32" ((oneBits :: Word32) == 0xffffffff)
         , testProperty "oneBits Word64" ((oneBits :: Word64) == 0xffffffffffffffff)
         , testProperty "oneBits Int8"   ((oneBits :: Int8)   == -1)
         , testProperty "oneBits Int16"  ((oneBits :: Int16)  == -1)
         , testProperty "oneBits Int32"  ((oneBits :: Int32)  == -1)
         , testProperty "oneBits Int64"  ((oneBits :: Int64)  == -1)
         ]
      , testGroup "Finite countLeadingZeros"
         [ testProperty "countLeadingZeros Word8"  (countLeadingZeros (0x10 :: Word8)  == 3)
         , testProperty "countLeadingZeros Word16" (countLeadingZeros (0x1000 :: Word16) == 3)
         , testProperty "countLeadingZeros Word32" (countLeadingZeros (0x10000000 :: Word32) == 3)
         , testProperty "countLeadingZeros Word64" (countLeadingZeros (0x1000000000000000 :: Word64) == 3)
         , testProperty "countLeadingZeros Int8"   (countLeadingZeros (0x10 :: Int8)  == 3)
         , testProperty "countLeadingZeros Int16"  (countLeadingZeros (0x1000 :: Int16) == 3)
         , testProperty "countLeadingZeros Int32"  (countLeadingZeros (0x10000000 :: Int32) == 3)
         , testProperty "countLeadingZeros Int64"  (countLeadingZeros (0x1000000000000000 :: Int64) == 3)
         , testProperty "countLeadingZeros Word8"  (countLeadingZeros (0x0 :: Word8)  == 8)
         , testProperty "countLeadingZeros Word16" (countLeadingZeros (0x0 :: Word16) == 16)
         , testProperty "countLeadingZeros Word32" (countLeadingZeros (0x0 :: Word32) == 32)
         , testProperty "countLeadingZeros Word64" (countLeadingZeros (0x0 :: Word64) == 64)
         , testProperty "countLeadingZeros Int8"   (countLeadingZeros (0x0 :: Int8)  == 8)
         , testProperty "countLeadingZeros Int16"  (countLeadingZeros (0x0 :: Int16) == 16)
         , testProperty "countLeadingZeros Int32"  (countLeadingZeros (0x0 :: Int32) == 32)
         , testProperty "countLeadingZeros Int64"  (countLeadingZeros (0x0 :: Int64) == 64)
         ]
      , testGroup "Finite countTrailingZeros"
         [ testProperty "countTrailingZeros Word8"  (countTrailingZeros (0x10 :: Word8)  == 4)
         , testProperty "countTrailingZeros Word16" (countTrailingZeros (0x1000 :: Word16) == 3*4)
         , testProperty "countTrailingZeros Word32" (countTrailingZeros (0x10000000 :: Word32) == 7*4)
         , testProperty "countTrailingZeros Word64" (countTrailingZeros (0x1000000000000000 :: Word64) == 15*4)
         , testProperty "countTrailingZeros Word"   (countTrailingZeros (0x10000000 :: Word) == 7*4)
         , testProperty "countTrailingZeros Int8"   (countTrailingZeros (0x10 :: Int8)  == 4)
         , testProperty "countTrailingZeros Int16"  (countTrailingZeros (0x1000 :: Int16) == 3*4)
         , testProperty "countTrailingZeros Int32"  (countTrailingZeros (0x10000000 :: Int32) == 7*4)
         , testProperty "countTrailingZeros Int64"  (countTrailingZeros (0x1000000000000000 :: Int64) == 15*4)
         , testProperty "countTrailingZeros Int"    (countTrailingZeros (0x10000000 :: Int) == 7*4)
         , testProperty "countTrailingZeros Word8"  (countTrailingZeros (0x0 :: Word8)  == 8)
         , testProperty "countTrailingZeros Word16" (countTrailingZeros (0x0 :: Word16) == 16)
         , testProperty "countTrailingZeros Word32" (countTrailingZeros (0x0 :: Word32) == 32)
         , testProperty "countTrailingZeros Word64" (countTrailingZeros (0x0 :: Word64) == 64)
         , testProperty "countTrailingZeros Int8"   (countTrailingZeros (0x0 :: Int8)  == 8)
         , testProperty "countTrailingZeros Int16"  (countTrailingZeros (0x0 :: Int16) == 16)
         , testProperty "countTrailingZeros Int32"  (countTrailingZeros (0x0 :: Int32) == 32)
         , testProperty "countTrailingZeros Int64"  (countTrailingZeros (0x0 :: Int64) == 64)
         ]
      ]
   , testGroup "Bitwise"
      [ testGroup "AND"
         [ testProperty "and Word8"  (0x04 .&. (5 :: Word8)  == 0x04)
         , testProperty "and Word16" (0x04 .&. (5 :: Word16) == 0x04)
         , testProperty "and Word32" (0x04 .&. (5 :: Word32) == 0x04)
         , testProperty "and Word64" (0x04 .&. (5 :: Word64) == 0x04)
         , testProperty "and Int8"   (0x04 .&. (5 :: Int8)   == 0x04)
         , testProperty "and Int16"  (0x04 .&. (5 :: Int16)  == 0x04)
         , testProperty "and Int32"  (0x04 .&. (5 :: Int32)  == 0x04)
         , testProperty "and Int64"  (0x04 .&. (5 :: Int64)  == 0x04)
         ]
      , testGroup "OR"
         [ testProperty "or Word8"  (0x02 .|. (5 :: Word8)  == 0x07)
         , testProperty "or Word16" (0x02 .|. (5 :: Word16) == 0x07)
         , testProperty "or Word32" (0x02 .|. (5 :: Word32) == 0x07)
         , testProperty "or Word64" (0x02 .|. (5 :: Word64) == 0x07)
         , testProperty "or Int8"   (0x02 .|. (5 :: Int8)   == 0x07)
         , testProperty "or Int16"  (0x02 .|. (5 :: Int16)  == 0x07)
         , testProperty "or Int32"  (0x02 .|. (5 :: Int32)  == 0x07)
         , testProperty "or Int64"  (0x02 .|. (5 :: Int64)  == 0x07)
         ]
      , testGroup "XOR"
         [ testProperty "xor Word8"  (0x03 `xor` (5 :: Word8)  == 0x06)
         , testProperty "xor Word16" (0x03 `xor` (5 :: Word16) == 0x06)
         , testProperty "xor Word32" (0x03 `xor` (5 :: Word32) == 0x06)
         , testProperty "xor Word64" (0x03 `xor` (5 :: Word64) == 0x06)
         , testProperty "xor Int8"   (0x03 `xor` (5 :: Int8)   == 0x06)
         , testProperty "xor Int16"  (0x03 `xor` (5 :: Int16)  == 0x06)
         , testProperty "xor Int32"  (0x03 `xor` (5 :: Int32)  == 0x06)
         , testProperty "xor Int64"  (0x03 `xor` (5 :: Int64)  == 0x06)
         ]
      , testGroup "complement"
         [ testProperty "complement Word8"  (complement (2 :: Word8)  == maxBound - 2)
         , testProperty "complement Word16" (complement (2 :: Word16) == maxBound - 2)
         , testProperty "complement Word32" (complement (2 :: Word32) == maxBound - 2)
         , testProperty "complement Word64" (complement (2 :: Word64) == maxBound - 2)
         , testProperty "complement Int8"   (complement (2 :: Int8)   == -3)
         , testProperty "complement Int16"  (complement (2 :: Int16)  == -3)
         , testProperty "complement Int32"  (complement (2 :: Int32)  == -3)
         , testProperty "complement Int64"  (complement (2 :: Int64)  == -3)
         ]
      ]
   , testGroup "Shift"
      [ testGroup "shiftR"
         [ testProperty "shiftR Word8"  ((0b00101000 :: Word8)  `shiftR` 2 == 0b00001010)
         , testProperty "shiftR Word16" ((0b00101000 :: Word16) `shiftR` 2 == 0b00001010)
         , testProperty "shiftR Word32" ((0b00101000 :: Word32) `shiftR` 2 == 0b00001010)
         , testProperty "shiftR Word64" ((0b00101000 :: Word64) `shiftR` 2 == 0b00001010)
         , testProperty "shiftR Int8"   ((0b00101000 :: Int8)   `shiftR` 2 == 0b00001010)
         , testProperty "shiftR Int16"  ((0b00101000 :: Int16)  `shiftR` 2 == 0b00001010)
         , testProperty "shiftR Int32"  ((0b00101000 :: Int32)  `shiftR` 2 == 0b00001010)
         , testProperty "shiftR Int64"  ((0b00101000 :: Int64)  `shiftR` 2 == 0b00001010)
         , testProperty "shiftR Int"    (((-1) :: Int)    `shiftR` 1 == fromIntegral (maxBound `div` 2 :: Word))
         , testProperty "shiftR Int8"   (((-1) :: Int8)   `shiftR` 1 == fromIntegral (maxBound `div` 2 :: Word8))
         , testProperty "shiftR Int16"  (((-1) :: Int16)  `shiftR` 1 == fromIntegral (maxBound `div` 2 :: Word16))
         , testProperty "shiftR Int32"  (((-1) :: Int32)  `shiftR` 1 == fromIntegral (maxBound `div` 2 :: Word32))
         , testProperty "shiftR Int64"  (((-1) :: Int64)  `shiftR` 1 == fromIntegral (maxBound `div` 2 :: Word64))
         ]
      , testGroup "shiftL"
         [ testProperty "shiftL Word8"  ((0b00101000 :: Word8)  `shiftL` 2 == 0b10100000)
         , testProperty "shiftL Word16" ((0b00101000 :: Word16) `shiftL` 2 == 0b10100000)
         , testProperty "shiftL Word32" ((0b00101000 :: Word32) `shiftL` 2 == 0b10100000)
         , testProperty "shiftL Word64" ((0b00101000 :: Word64) `shiftL` 2 == 0b10100000)
         , testProperty "shiftL Int8"   ((0b00011000 :: Int8)   `shiftL` 2 == 0b01100000)
         , testProperty "shiftL Int16"  ((0b00101000 :: Int16)  `shiftL` 2 == 0b10100000)
         , testProperty "shiftL Int32"  ((0b00101000 :: Int32)  `shiftL` 2 == 0b10100000)
         , testProperty "shiftL Int64"  ((0b00101000 :: Int64)  `shiftL` 2 == 0b10100000)
         , testProperty "shiftL Int8"   ((0b00101000 :: Int8)   `shiftL` 3 == 0b01000000)
         , testProperty "shiftL -1 Int"    (((-1) :: Int)    `shiftL` 1 == -2)
         , testProperty "shiftL -1 Int8"   (((-1) :: Int8)   `shiftL` 1 == -2)
         , testProperty "shiftL -1 Int16"  (((-1) :: Int16)  `shiftL` 1 == -2)
         , testProperty "shiftL -1 Int32"  (((-1) :: Int32)  `shiftL` 1 == -2)
         , testProperty "shiftL -1 Int64"  (((-1) :: Int64)  `shiftL` 1 == -2)
         , testProperty "signedShiftL Int8"   ((0b00101000 :: Int8)   `signedShiftL` 3 == 0b01000000)
         ]
      ]
   , testGroup "Rotate"
      [ testGroup "rotate i . rotate (n-i) == id"
         [ testProperty "rotate Word8"  (\(x :: Word8)  i -> (x `rotate` i) `rotate` (bitSize x - i) == x)
         , testProperty "rotate Word16" (\(x :: Word16) i -> (x `rotate` i) `rotate` (bitSize x - i) == x)
         , testProperty "rotate Word32" (\(x :: Word32) i -> (x `rotate` i) `rotate` (bitSize x - i) == x)
         , testProperty "rotate Word64" (\(x :: Word64) i -> (x `rotate` i) `rotate` (bitSize x - i) == x)
         , testProperty "rotate Int8"   (\(x :: Int8)   i -> (x `rotate` i) `rotate` (bitSize x - i) == x)
         , testProperty "rotate Int16"  (\(x :: Int16)  i -> (x `rotate` i) `rotate` (bitSize x - i) == x)
         , testProperty "rotate Int32"  (\(x :: Int32)  i -> (x `rotate` i) `rotate` (bitSize x - i) == x)
         , testProperty "rotate Int64"  (\(x :: Int64)  i -> (x `rotate` i) `rotate` (bitSize x - i) == x)
         ]
      ]
   , testGroup "Bits to/from string"
      [ testProperty "Bits from string \"01010011\" (Word8)" (bitsFromString "01010011" == (83 :: Word8))


      -- Test that a random BitString (i.e. a string with length 64 and only
      -- composed of 0s and 1s) can be converted into a Word64 and back into a string
      , testProperty "Bits from string reverse (Word64)" <|
         \(BitString s) -> bitsToString (bitsFromString s :: Word64) == s

      , testProperty "Bits to string (Word8)"            (prop_bits_to_string :: Word8  -> Bool)
      , testProperty "Bits to string (Word16)"           (prop_bits_to_string :: Word16 -> Bool)
      , testProperty "Bits to string (Word32)"           (prop_bits_to_string :: Word32 -> Bool)
      , testProperty "Bits to string (Word64)"           (prop_bits_to_string :: Word64 -> Bool)

      , testProperty "N bits to string"                  (bitsToStringN 4 (0x06 :: Word8) == "0110")
      ]
   , testGroup "Bit put/bit get"
      [ testProperty "Bit put/get Word8  - 8  bits"      (prop_reverse_word 8  :: Word8  -> ArbitraryBitOrder -> Bool)
      , testProperty "Bit put/get Word16 - 16 bits"      (prop_reverse_word 16 :: Word16 -> ArbitraryBitOrder -> Bool)
      , testProperty "Bit put/get Word32 - 32 bits"      (prop_reverse_word 32 :: Word32 -> ArbitraryBitOrder -> Bool)
      , testProperty "Bit put/get Word64 - 64 bits"      (prop_reverse_word 64 :: Word64 -> ArbitraryBitOrder -> Bool)
      , testProperty "Bit put/get Word8  - [1,8]  bits"  (prop_reverse_word_size :: Size8  -> Word8   -> ArbitraryBitOrder -> Bool)
      , testProperty "Bit put/get Word16 - [1,16] bits"  (prop_reverse_word_size :: Size16 -> Word16  -> ArbitraryBitOrder -> Bool)
      , testProperty "Bit put/get Word32 - [1,32] bits"  (prop_reverse_word_size :: Size32 -> Word32  -> ArbitraryBitOrder -> Bool)
      , testProperty "Bit put/get Word64 - [1,64] bits"  (prop_reverse_word_size :: Size64 -> Word64  -> ArbitraryBitOrder -> Bool)
      , testProperty "Monadic BitPut/BitGet, two parts of two Word64"
         (prop_split_word :: Size64 -> Size64 -> Word64 -> Word64 -> ArbitraryBitOrder -> Bool)
      , testProperty "Monadic BitPut/BitGet, bytestring with offset" prop_reverse_bs
      ]
   , testGroup "Variable length (LEB128)"
      [ testProperty "Put/Get reverse (Word8)"           (prop_uleb128_reverse :: Word8 -> Bool)
      , testProperty "Put/Get reverse (Word16)"          (prop_uleb128_reverse :: Word16 -> Bool)
      , testProperty "Put/Get reverse (Word32)"          (prop_uleb128_reverse :: Word32 -> Bool)
      , testProperty "Put/Get reverse (Word64)"          (prop_uleb128_reverse :: Word64 -> Bool)
      ]
   , testGroup "Reverse bits (Word8)"
      [ testProperty "Reverse bits in a Word8"  (reverseBits (0x28 :: Word8) == 0x14)
      , testProperty "Bijective: obvious"       (isBijective (reverseBitsObvious :: Word8 -> Word8))
      , testProperty "Bijective: 3Ops"          (isBijective reverseBits3Ops)
      , testProperty "Bijective: 4Ops"          (isBijective reverseBits4Ops)
      , testProperty "Bijective: lookup table"  (isBijective reverseBitsTable)
      , testProperty "Bijective: 7Ops"          (isBijective reverseBits7Ops)
      , testProperty "Bijective: 5LgN"          (isBijective (reverseBits5LgN :: Word8 -> Word8))
      , testProperty "Equivalent: obvious"      (isEquivalent (reverseBits :: Word8 -> Word8) reverseBitsObvious)
      , testProperty "Equivalent: 3Ops"         (isEquivalent (reverseBits :: Word8 -> Word8) reverseBits3Ops)
      , testProperty "Equivalent: 4Ops"         (isEquivalent (reverseBits :: Word8 -> Word8) reverseBits4Ops)
      , testProperty "Equivalent: lookup table" (isEquivalent (reverseBits :: Word8 -> Word8) reverseBitsTable)
      , testProperty "Equivalent: 7Ops"         (isEquivalent (reverseBits :: Word8 -> Word8) reverseBits7Ops)
      , testProperty "Equivalent: 5LgN"         (isEquivalent (reverseBits :: Word8 -> Word8) reverseBits5LgN)
      ]
   , testGroup "Reverse bits (Word16)"
      [ testProperty "Reverse bits in a Word16" (reverseBits (0x2817 :: Word16) == 0xe814)
      , testProperty "Bijective: obvious"       (isBijective (                reverseBitsObvious :: Word16 -> Word16))
      , testProperty "Bijective: 3Ops"          (isBijective (liftReverseBits reverseBits3Ops    :: Word16 -> Word16))
      , testProperty "Bijective: 4Ops"          (isBijective (liftReverseBits reverseBits4Ops    :: Word16 -> Word16))
      , testProperty "Bijective: lookup table"  (isBijective (liftReverseBits reverseBitsTable   :: Word16 -> Word16))
      , testProperty "Bijective: 7Ops"          (isBijective (liftReverseBits reverseBits7Ops    :: Word16 -> Word16))
      , testProperty "Bijective: 5LgN"          (isBijective (                reverseBits5LgN    :: Word16 -> Word16))
      , testProperty "Equivalent: obvious"      (isEquivalent (reverseBits :: Word16 -> Word16) reverseBitsObvious)
      , testProperty "Equivalent: 3Ops"         (isEquivalent (reverseBits :: Word16 -> Word16) (liftReverseBits reverseBits3Ops))
      , testProperty "Equivalent: 4Ops"         (isEquivalent (reverseBits :: Word16 -> Word16) (liftReverseBits reverseBits4Ops))
      , testProperty "Equivalent: lookup table" (isEquivalent (reverseBits :: Word16 -> Word16) (liftReverseBits reverseBitsTable))
      , testProperty "Equivalent: 7Ops"         (isEquivalent (reverseBits :: Word16 -> Word16) (liftReverseBits reverseBits7Ops))
      , testProperty "Equivalent: 5LgN"         (isEquivalent (reverseBits :: Word16 -> Word16) reverseBits5LgN)
      ]
   , testGroup "Reverse bits (Word32)"
      [ testProperty "Reverse bits in a Word32" (reverseBits (0x28173456 :: Word32) == 0x6a2ce814)
      , testProperty "Bijective: obvious"       (isBijective (                reverseBitsObvious :: Word32 -> Word32))
      , testProperty "Bijective: 3Ops"          (isBijective (liftReverseBits reverseBits3Ops    :: Word32 -> Word32))
      , testProperty "Bijective: 4Ops"          (isBijective (liftReverseBits reverseBits4Ops    :: Word32 -> Word32))
      , testProperty "Bijective: lookup table"  (isBijective (liftReverseBits reverseBitsTable   :: Word32 -> Word32))
      , testProperty "Bijective: 7Ops"          (isBijective (liftReverseBits reverseBits7Ops    :: Word32 -> Word32))
      , testProperty "Bijective: 5LgN"          (isBijective (                reverseBits5LgN    :: Word32 -> Word32))
      , testProperty "Equivalent: obvious"      (isEquivalent (reverseBits :: Word32 -> Word32) reverseBitsObvious)
      , testProperty "Equivalent: 3Ops"         (isEquivalent (reverseBits :: Word32 -> Word32) (liftReverseBits reverseBits3Ops))
      , testProperty "Equivalent: 4Ops"         (isEquivalent (reverseBits :: Word32 -> Word32) (liftReverseBits reverseBits4Ops))
      , testProperty "Equivalent: lookup table" (isEquivalent (reverseBits :: Word32 -> Word32) (liftReverseBits reverseBitsTable))
      , testProperty "Equivalent: 7Ops"         (isEquivalent (reverseBits :: Word32 -> Word32) (liftReverseBits reverseBits7Ops))
      , testProperty "Equivalent: 5LgN"         (isEquivalent (reverseBits :: Word32 -> Word32) reverseBits5LgN)
      ]
   , testGroup "Reverse bits (Word64)"
      [ testProperty "Reverse bits in a Word64" (reverseBits (0x2800017003450060 :: Word64) == 0x0600a2c00e800014)
      , testProperty "Bijective: obvious"       (isBijective (                reverseBitsObvious :: Word64 -> Word64))
      , testProperty "Bijective: 3Ops"          (isBijective (liftReverseBits reverseBits3Ops    :: Word64 -> Word64))
      , testProperty "Bijective: 4Ops"          (isBijective (liftReverseBits reverseBits4Ops    :: Word64 -> Word64))
      , testProperty "Bijective: lookup table"  (isBijective (liftReverseBits reverseBitsTable   :: Word64 -> Word64))
      , testProperty "Bijective: 7Ops"          (isBijective (liftReverseBits reverseBits7Ops    :: Word64 -> Word64))
      , testProperty "Bijective: 5LgN"          (isBijective (                reverseBits5LgN    :: Word64 -> Word64))
      , testProperty "Equivalent: obvious"      (isEquivalent (reverseBits :: Word64 -> Word64) reverseBitsObvious)
      , testProperty "Equivalent: 3Ops"         (isEquivalent (reverseBits :: Word64 -> Word64) (liftReverseBits reverseBits3Ops))
      , testProperty "Equivalent: 4Ops"         (isEquivalent (reverseBits :: Word64 -> Word64) (liftReverseBits reverseBits4Ops))
      , testProperty "Equivalent: lookup table" (isEquivalent (reverseBits :: Word64 -> Word64) (liftReverseBits reverseBitsTable))
      , testProperty "Equivalent: 7Ops"         (isEquivalent (reverseBits :: Word64 -> Word64) (liftReverseBits reverseBits7Ops))
      , testProperty "Equivalent: 5LgN"         (isEquivalent (reverseBits :: Word64 -> Word64) reverseBits5LgN)
      ]
   ]

newtype ArbitraryBitOrder = ArbitraryBitOrder BitOrder deriving (Show)

instance Arbitrary ArbitraryBitOrder where
   arbitrary = elements $ fmap ArbitraryBitOrder [BB,LB,BL,LL]
   shrink (ArbitraryBitOrder x) = case x of
      LL -> fmap ArbitraryBitOrder [BB,LB,BL]
      BL -> fmap ArbitraryBitOrder [BB,LB]
      LB -> fmap ArbitraryBitOrder [BB]
      BB -> fmap ArbitraryBitOrder []

class Size x where
   fromSize :: x -> Word

newtype Size8  = Size8  Word deriving (Show)
newtype Size16 = Size16 Word deriving (Show)
newtype Size32 = Size32 Word deriving (Show)
newtype Size64 = Size64 Word deriving (Show)

instance Size Size8  where fromSize (Size8  x) = x
instance Size Size16 where fromSize (Size16 x) = x
instance Size Size32 where fromSize (Size32 x) = x
instance Size Size64 where fromSize (Size64 x) = x

instance Arbitrary Size8  where arbitrary = fmap Size8  $ choose (1,8)
instance Arbitrary Size16 where arbitrary = fmap Size16 $ choose (1,16)
instance Arbitrary Size32 where arbitrary = fmap Size32 $ choose (1,32)
instance Arbitrary Size64 where arbitrary = fmap Size64 $ choose (1,64)


newtype BitString = BitString String deriving (Show)

instance Arbitrary BitString where
   arbitrary = fmap BitString $ vectorOf 64 (elements ['0','1'])

-- | Test that a word can be converted into a BitString and back
prop_bits_to_string :: Bits a => a -> Bool
prop_bits_to_string x = bitsFromString (bitsToString x) == x

-- | Test that words of the given length can be written and read back with
-- BitGet/BitPut. Test every bit ordering.
prop_reverse_word :: (Integral a, Bits a, ReversableBits a) => Word -> a -> ArbitraryBitOrder -> Bool
prop_reverse_word n w (ArbitraryBitOrder bo) = maskDyn n w == dec
   where
      enc = getBitPutBuffer  $ putBits n w $ newBitPutState bo
      dec = getBits n $ newBitGetState bo enc

-- | Test that a ByteString can be written and read back with
-- BitGet/BitPut. Test every bit ordering.
prop_reverse_bs :: Word64 -> Size64 -> ArbitraryBuffer -> ArbitraryBitOrder -> Bool
prop_reverse_bs w s (ArbitraryBuffer bs) (ArbitraryBitOrder bo) = runBitGet bo dec (runBitPut bo enc)
   where
      len = bufferSize bs
      enc = do
         putBitsM (fromSize s) w
         putBitsBufferM bs
      dec = do
         w2  <- getBitsM (fromSize s)
         bs' <- getBitsBSM (fromIntegral len)
         return (bs == bs' && w2 == maskDyn (fromSize s) w)

-- | Test that words with arbitrary (but still valid) lengths can be written and
-- read back with BitGet/BitPut. Test every bit ordering.
prop_reverse_word_size :: (Integral a, Bits a, ReversableBits a, Size s) => s -> a -> ArbitraryBitOrder -> Bool
prop_reverse_word_size n w bo = prop_reverse_word (fromSize n) w bo

-- | Write two parts of two words and read them back
prop_split_word :: (Num a, Integral a, Bits a, ReversableBits a,
                    Num b, Integral b, Bits b, ReversableBits b,
                    Size s1, Size s2) => s1 -> s2 -> a -> b -> ArbitraryBitOrder -> Bool
prop_split_word s1 s2 w1 w2 (ArbitraryBitOrder bo) = runBitGet bo dec (runBitPut bo enc)
   where
      enc = do
         putBitsM (fromSize s1) w1
         putBitsM (fromSize s2) w2
      dec = do
         v1 <- getBitsM (fromSize s1)
         v2 <- getBitsM (fromSize s2)
         return (v1 == maskDyn (fromSize s1) w1 && v2 == maskDyn (fromSize s2) w2)

-- | Test that ULEB128 decoder can read back what has been written with ULEB128
-- encoder
prop_uleb128_reverse :: (Integral a, Bits a) => a -> Bool
prop_uleb128_reverse w = w == runGetOrFail getULEB128 (runPut (putULEB128 w))
