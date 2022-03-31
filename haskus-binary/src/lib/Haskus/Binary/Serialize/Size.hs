{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Haskus.Binary.Serialize.Size
   ( GetSize (..)
   , runGetSize
   )
where

import Haskus.Binary.Serialize.Put
import Haskus.Memory.Buffer
import System.IO.Unsafe
import GHC.Exts

newtype GetSize a
   = GetSize (Word# -> (# Word#, a #))

instance Functor GetSize where
  fmap f (GetSize g) = GetSize \w0 -> case g w0 of
                          (# w1, a #) -> (# w1, f a #)

instance Applicative GetSize where
  pure a = GetSize \w -> (# w, a #)
  GetSize f <*> GetSize a = GetSize \w0 -> case f w0 of
                              (# w1, f' #) -> case a w1 of
                                (# w2, a' #) -> (# w2, f' a' #)

instance Monad GetSize where
  GetSize m >>= f = GetSize \w0 -> case m w0 of
    (# w1, a #) -> case f a of
      GetSize f' -> f' w1

-- | Increment the current size
incSize :: Word -> GetSize ()
incSize (W# x) = GetSize \w -> (# w `plusWord#` x, () #)

-- | Get the total size
runGetSize :: GetSize a -> Word
runGetSize (GetSize s) = case s 0## of
  (# w, _a #) -> W# w

instance PutMonad GetSize where
   putWord8 _  = incSize 1
   putWord16 _ = incSize 2
   putWord32 _ = incSize 4
   putWord64 _ = incSize 8
   putBuffer b = incSize (unsafePerformIO (bufferSize b))
