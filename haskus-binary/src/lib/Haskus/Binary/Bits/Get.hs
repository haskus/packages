{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Bit getter
module Haskus.Binary.Bits.Get
   ( BitGetState(..)
   , newBitGetState
   , isEmpty
   , skipBits
   , skipBitsToAlignOnWord8
   , getBits
   , getBitsChecked
   , getBitsBuffer
   -- * Monadic
   , BitGet
   , BitGetT
   , runBitGet
   , runBitGetT
   , runBitGetPartialT
   , runBitGetPartial
   , resumeBitGetPartialT
   , resumeBitGetPartial
   , isEmptyM
   , skipBitsM
   , skipBitsToAlignOnWord8M
   , getBitsM
   , getBitsCheckedM
   , getBitBoolM
   , getBitsBSM
   , changeBitGetOrder
   , withBitGetOrder
   )
where

import System.IO.Unsafe (unsafePerformIO)
import Data.Functor.Identity
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr
import Control.Monad

import Haskus.Binary.Buffer
import Haskus.Binary.Bits.Order
import Haskus.Binary.Bits
import Haskus.Binary.Storable (poke)

-- | BitGet state
data BitGetState = BitGetState
   { bitGetStateInput      :: {-# UNPACK #-} !Buffer     -- ^ Input
   , bitGetStateBitOffset  :: {-# UNPACK #-} !Word       -- ^ Bit offset (0-7)
   , bitGetStateBitOrder   ::                !BitOrder   -- ^ Bit order
   } deriving (Show)

-- | Create a new BitGetState
newBitGetState :: BitOrder -> Buffer -> BitGetState
newBitGetState bo bs = BitGetState bs 0 bo

-- | Indicate that the source is empty
isEmpty :: BitGetState -> Bool
isEmpty (BitGetState bs o _) = o == 0 && isBufferEmpty bs

-- | Skip the given number of bits from the input
skipBits :: Word -> BitGetState -> BitGetState
skipBits o (BitGetState bs n bo) = BitGetState (bufferUnsafeDrop d bs) n' bo
   where
      !o' = n+o
      !d  = fromIntegral $ byteOffset o'
      !n' = bitOffset o'

-- | Skip the required number of bits to be aligned on 8-bits
skipBitsToAlignOnWord8 :: BitGetState -> BitGetState
skipBitsToAlignOnWord8 s = case bitGetStateBitOffset s of
   0 -> s
   n -> skipBits (8-n) s

-- | Read the given number of bits and put the result in a word
getBits :: (Integral a, Bits a) => Word -> BitGetState -> a
getBits nbits (BitGetState bs off bo) = rec zeroBits 0 bs off nbits
   where
      -- w   = current result
      -- n   = number of valid bits in w
      -- i   = input bytestring
      -- o   = bit offset in input bytestring
      -- r   = number of remaining bits to read
      rec w _ _ _ 0 = w
      rec w n i o r = rec nw (n+nb) (bufferTail i) o' (r-nb)
         where 
            -- current Word8
            c  = bufferHead i
            -- number of bits to take from the current Word8
            nb = min (8-o) r
            -- bits taken from the current Word8 and put in correct order in least-significant bits
            tc = fromIntegral $ getBitRange bo o nb c
            -- mix new bits with the current result
            nw = case bo of
                  BB -> (w `shiftL` fromIntegral nb) .|. tc
                  LB -> (w `shiftL` fromIntegral nb) .|. tc
                  BL -> (tc `shiftL` fromIntegral n) .|. w
                  LL -> (tc `shiftL` fromIntegral n) .|. w
            -- new offset ((o + nb) `mod` 8)
            o' = bitOffset (o + nb)

-- | Perform some checks before calling getBits
--
-- Check that the number of bits to read is not greater than the first parameter
getBitsChecked :: (Integral a, Bits a, ReversableBits a) => Word -> Word -> BitGetState -> a
{-# INLINABLE getBitsChecked #-}
getBitsChecked m n s
   | n > m     = error $ "Tried to read more than " ++ show m ++ " bits (" ++ show n ++")"
   | otherwise = getBits n s

-- | Read the given number of Word8 and return them in a Buffer
--
-- Examples:
-- @  BB: xxxABCDE FGHIJKLM NOPxxxxx -> ABCDEFGH IJKLMNOP @
-- @  LL: LMNOPxxx DEFGHIJK xxxxxABC -> ABCDEFGH IJKLMNOP @
-- @  BL: xxxPONML KJIHGFED CBAxxxxx -> ABCDEFGH IJKLMNOP @
-- @  LB: EDCBAxxx MLKJIHGF xxxxxPON -> ABCDEFGH IJKLMNOP @
getBitsBuffer :: Word -> BitGetState -> Buffer
getBitsBuffer n (BitGetState bs o bo) =
   if n == 0
      then emptyBuffer
      else
         let 
            bs'  = bufferUnsafeTake (n+1) bs
            bs'' = bufferUnsafeTake n     bs
            rev  = bufferMap reverseBits
         in case (o,bo) of
            (0,BB) ->                     bs''
            (0,LL) ->       bufferReverse bs''
            (0,LB) -> rev                 bs''
            (0,BL) -> rev $ bufferReverse bs''
            (_,LL) ->                     getBitsBuffer n (BitGetState (bufferReverse bs') (8-o)  BB)
            (_,BL) -> rev . bufferReverse $ getBitsBuffer n (BitGetState bs'               o     BB)
            (_,LB) -> rev . bufferReverse $ getBitsBuffer n (BitGetState bs'               o     LL)
            (_,BB) -> unsafePerformIO $ do
               let len = n+1
               ptr <- mallocBytes (fromIntegral len)
               let f r i = do
                     let
                        w  = bufferUnsafeIndex bs (len-i)
                        w' = (w `shiftL` fromIntegral o) .|. r
                        r' = w `shiftR` (8-fromIntegral o)
                     poke (castPtr ptr `plusPtr` fromIntegral (len-i)) w'
                     return r'
               foldM_ f 0 [1..len]
               bufferUnsafeInit <$> bufferPackPtr len ptr



-- | BitGet monad transformer
newtype BitGetT m a
  = BitGetT (BitGetState -> m (BitGetState, a))
  deriving (Functor)

instance Monad m => Applicative (BitGetT m) where
  pure a = BitGetT (\s -> pure (s,a))
  (BitGetT f) <*> (BitGetT a) =
    BitGetT \s -> do
      (s',f')  <- f s
      (s'',a') <- a s'
      pure (s'', f' a')

instance Monad m => Monad (BitGetT m) where
  BitGetT a >>= f = BitGetT \s -> do
    (s', a') <- a s
    case f a' of
      BitGetT r -> r s'

-- | BitGet monad
type BitGet a    = BitGetT Identity a

-- | Evaluate a BitGet monad
runBitGetT :: Monad m => BitOrder -> BitGetT m a -> Buffer -> m a
runBitGetT bo m bs = snd <$> runBitGetPartialT bo m bs


-- | Evaluate a BitGet monad
runBitGet :: BitOrder -> BitGet a -> Buffer -> a
runBitGet bo m bs = runIdentity (runBitGetT bo m bs)

-- | Evaluate a BitGet monad, return the remaining state
runBitGetPartialT :: Functor m => BitOrder -> BitGetT m a -> Buffer -> m (BitGetState,a)
runBitGetPartialT bo (BitGetT m) bs = m (newBitGetState bo bs)

-- | Evaluate a BitGet monad, return the remaining state
runBitGetPartial :: BitOrder -> BitGet a -> Buffer -> (BitGetState,a)
runBitGetPartial bo m bs = runIdentity (runBitGetPartialT bo m bs)

-- | Resume a BitGet evaluation
resumeBitGetPartialT :: BitGetT m a -> BitGetState -> m (BitGetState,a)
resumeBitGetPartialT (BitGetT m) s = m s

-- | Resume a BitGet evaluation
resumeBitGetPartial :: BitGet a -> BitGetState -> (BitGetState,a)
resumeBitGetPartial m s = runIdentity (resumeBitGetPartialT m s)

-- | Indicate if all bits have been read
isEmptyM :: Monad m => BitGetT m Bool
isEmptyM = BitGetT \s -> pure (s,isEmpty s)

-- | Skip the given number of bits from the input (monadic version)
skipBitsM :: Monad m => Word -> BitGetT m ()
skipBitsM n = BitGetT \s -> pure (skipBits n s, ())


-- | Skip the required number of bits to be aligned on 8-bits (monadic version)
skipBitsToAlignOnWord8M :: Monad m =>  BitGetT m ()
skipBitsToAlignOnWord8M = BitGetT \s -> pure (skipBitsToAlignOnWord8 s, ())

-- | Read the given number of bits and put the result in a word
getBitsM :: (Integral a, Bits a, Monad m) => Word -> BitGetT m a
getBitsM n = do
   v <- BitGetT \s -> pure (s, getBits n s)
   skipBitsM n
   return v

-- | Perform some checks before calling getBitsM
getBitsCheckedM :: (Integral a, Bits a, ReversableBits a, Monad m) => Word -> Word -> BitGetT m a
getBitsCheckedM m n = do
   v <- BitGetT \s -> pure (s, getBitsChecked m n s)
   skipBitsM n
   return v

-- | Get a bit and convert it into a Bool
getBitBoolM :: (Monad m) => BitGetT m Bool
getBitBoolM = do
   v <- getBitsM 1
   return ((v :: Word) == 1)

-- | Get the given number of Word8
getBitsBSM :: (Monad m) => Word -> BitGetT m Buffer
getBitsBSM n = do
   bs <- BitGetT \s -> pure (s, getBitsBuffer n s)
   skipBitsM (8*n)
   return bs

-- | Change the current bit ordering
--
-- Be careful to change the outer bit ordering (B* to L* or the inverse) only
-- on bytes boundaries! Otherwise, you will read the same bits more than once.
changeBitGetOrder :: Monad m => BitOrder -> BitGetT m ()
changeBitGetOrder bo = BitGetT \s -> pure (s { bitGetStateBitOrder = bo }, ())

-- | Change the bit ordering for the wrapped BitGet
--
-- Be careful, this function uses changeBitGetOrder internally.
withBitGetOrder :: Monad m => BitOrder -> BitGetT m a -> BitGetT m a
withBitGetOrder bo m = do
   bo' <- BitGetT \s -> pure (s, bitGetStateBitOrder s)
   changeBitGetOrder bo
   v <- m
   changeBitGetOrder bo'
   return v

