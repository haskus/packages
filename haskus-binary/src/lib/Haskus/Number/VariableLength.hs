{-# LANGUAGE FlexibleContexts #-}

-- | Variable length encodings
--
-- * Unsigned Little Endian Base 128 (ULEB128)
--
-- The word is splitted in chunks of 7 bits, starting from least significant
-- bits. Each chunk is put in a Word8. The highest bit indicates if there is a
-- following byte (0 false, 1 true)
module Haskus.Number.VariableLength
   ( fromULEB128
   , toULEB128
   , getULEB128
   , putULEB128
   , getSLEB128
   , putSLEB128
   , getLEB128Buffer
   )
where

import Haskus.Number.Word
import Haskus.Number.Int
import Haskus.Binary.Get
import Haskus.Binary.Put
import Haskus.Binary.Bits
import Haskus.Binary.Bits.Put
import Haskus.Binary.Bits.Order
import Haskus.Binary.Buffer

-- | Convert a stream of ULEB 128 bytes into an Integral
--
-- >>> :seti -XBinaryLiterals
-- >>> import Control.Monad.Trans.State
-- >>> getNext = do { ~(x:xs) <- get; put xs; pure x }
-- >>> let x = evalState (fromULEB128 getNext) [0b10000001, 0b01111111] :: Word64
-- >>> x == 0b11111110000001
-- True
fromULEB128 :: (Bits a, Monad m, Integral a) => m Word8 -> m a
fromULEB128 getW8 = go 0 0
   where
      go acc n = do
         a <- getW8
         let
            w    = fromIntegral (a .&. 0x7f)
            acc' = w `shiftL` n .|. acc
         if not (testBit a 7)
            then return acc'
            else go acc' (n+7)

-- | Convert an Integral into a stream of ULEB128 bytes
--
-- >>> :seti -XBinaryLiterals
-- >>> :seti -XFlexibleContexts
-- >>> let f = toULEB128 (putStr . (++ " ") . bitsToString)
-- >>> f (0b1001001010101010 :: Word64)
-- 10101010 10100101 00000010
toULEB128 :: (Bits a, Monad m, Integral a) => (Word8 -> m ()) -> a -> m ()
toULEB128 putW8 = goFirst
   where
      goFirst 0 = putW8 0
      goFirst n = go n

      go 0 = pure ()
      go x = do
         let
            r = x `shiftR` 7
            w = fromIntegral (x .&. 0x7f)
            w' = if r == 0 then w else setBit w 7
         putW8 w'
         go r

-- | Get an unsigned word in Little Endian Base 128
getULEB128 :: (Integral a, Bits a) => Get a
getULEB128 = fromULEB128 getWord8

-- | Put an unsigned word in Little Endian Base 128
putULEB128 :: (Integral a, Bits a) => a -> Put
putULEB128 = toULEB128 putWord8


-- | Get a signed int in Little Endian Base 128
getSLEB128 :: (Integral a, Bits a) => Get a
getSLEB128 = do
   let toInt8 :: Word8 -> Int8
       toInt8 = fromIntegral
   a <- getWord8
   if not (testBit a 7)
      then return . fromIntegral . toInt8 $ (a .&. 0x7f) .|. ((a .&. 0x40) `shiftL` 1)
      else do
         b <- getSLEB128
         return $ (b `shiftL` 7) .|. (fromIntegral (a .&. 0x7f))

-- | Put a signed int in Little Endian Base 128
putSLEB128 :: (Integral a, Bits a) => a -> Put
putSLEB128 a = rec a
   where
      ext = if a >= 0 then 0 else complement 0
      rec x =  do
         let 
            r = x `shiftR` 7
            w = x .&. 0x7f
         if r /= ext
            then do
               putWord8 (fromIntegral w .|. 0x80)
               rec r
            else if (testBit w 6 && a < 0) || (not (testBit w 6) && a >= 0)
               then putWord8 (fromIntegral w)   -- no need for sign byte
               else do
                  putWord8 (fromIntegral w .|. 0x80)
                  putWord8 (fromIntegral ext .&. 0x7f)   -- sign byte


-- | Get a bytestring containing a decoded LEB128 string
getLEB128Buffer :: BitOrder -> Get Buffer
getLEB128Buffer bo = rec (newBitPutState bo)
   where
      rec state = do
         w      <- getWord8
         let state2 = putBits 7 w state
         case testBit w 7 of
            True  -> rec state2
            False -> return (getBitPutBuffer state2)

