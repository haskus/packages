{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

-- | A buffer in memory
module Haskus.Memory.Buffer where

import Haskus.Number.Word
import Haskus.Number.Int
import Haskus.Binary.Storable
import Haskus.Memory.Utils (memcpy#)
import Haskus.Utils.Monad

import Data.IORef
import System.IO.Unsafe
#if MIN_VERSION_GLASGOW_HASKELL (9,0,0,0)
import Unsafe.Coerce (unsafeCoerce#)
#endif

import GHC.STRef
import GHC.IORef
import GHC.Prim
import GHC.Base
import GHC.Exts (toList, IsList(..), Ptr (..))

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> :set -XScopedTypeVariables
-- >>> import Haskus.Binary.Bits
-- >>> import Haskus.Utils.Monad
-- >>> import Haskus.Number.Word

-- There are different kinds of buffers:
--  1. in managed heap: small and unpinned
--  2. in managed heap: pinned
--  3. out of the managed heap
--
-- GHC maintains a distinction between immutable and mutable in heap buffers
-- (respectively ByteArray# and MutableByteArray#) but they are represented by
-- the same heap objects and we can freely convert from one to the other.
--
-- Buffers and managed heap may be pinned (i.e. may have a fixed address). GHC
-- automatically pins large buffers. Buffers out of the managed heap are
-- represented by their address, hence the latter mustn't/can't change and they
-- behave as pinned buffers.
--
-- It is common to want to attach finalizers to buffers (e.g. to reclaim memory
-- for buffer out of the managed heap). We can't directly attach them to the
-- Addr# or to the ByteArray#. We must also be very careful to avoid attaching
-- them to the box (e.g. to "Ptr" in "Ptr Addr#") because GHC freely removes
-- boxes to produce faster code. The best option is to attach the finalizers to
-- an IORef which contains the finalizers themselves!
--
-- For performance, we often want buffer references to be unboxed. Hence the use
-- of unboxed sums/tuples.

type InBuffer# s   = MutableByteArray# s
type ExtBuffer#    = (# Addr#, Word# #)
type Finalizers# s = (# (# #) | MutVar# s [IO ()] #) -- Finalizers are optional
type Buffer# s     = (# (# InBuffer# s | ExtBuffer# #), Finalizers# s #)

data STBuffer s = Buffer (Buffer# s)
type Buffer     = STBuffer RealWorld

{-# COMPLETE InBuffer, OutBuffer #-}

pattern OutBuffer :: Addr# -> Word# -> Finalizers# s -> STBuffer s
pattern OutBuffer addr sz fin = Buffer (# (# | (# addr, sz #) #), fin #)

pattern InBuffer :: MutableByteArray# s -> Finalizers# s -> STBuffer s
pattern InBuffer ba fin = Buffer (# (# ba | #), fin #)

{-# COMPLETE NoFinalizers, Finalizers #-}

pattern NoFinalizers :: Finalizers# s
pattern NoFinalizers = (# (# #) | #)

pattern Finalizers :: MutVar# s [IO ()] -> Finalizers# s
pattern Finalizers fin = (# | fin #)
-----------------------------------------------------------------
-- Allocation
-----------------------------------------------------------------

-- | Allocate a buffer (unpinned if small)
--
-- >>> b <- newBuffer 1024
--
newBuffer :: Word -> IO Buffer
newBuffer (W# sz) = IO \s0 ->
  let !(# s1,ba  #) = newByteArray# (word2Int# sz) s0
  in (# s1, InBuffer ba NoFinalizers #)


-- | Allocate a buffer (pinned)
newPinnedBuffer :: Word -> IO Buffer
newPinnedBuffer (W# sz) = IO \s0 ->
  let !(# s1,ba  #) = newPinnedByteArray# (word2Int# sz) s0
  in (# s1, InBuffer ba NoFinalizers #)

-- | Allocate an aligned buffer (pinned)
newAlignedPinnedBuffer :: Word -> Word -> IO Buffer
newAlignedPinnedBuffer (W# sz) (W# al) = IO \s0 ->
  let !(# s1,ba  #) = newAlignedPinnedByteArray# (word2Int# sz) (word2Int# al) s0
  in (# s1, InBuffer ba NoFinalizers #)

-- | Attach an external buffer
attachExternalBuffer :: Addr# -> Word# -> Buffer
attachExternalBuffer addr sz = OutBuffer addr sz NoFinalizers

-- | Attach an external buffer
attachExternalBufferPtr :: Ptr a -> Word# -> Buffer
attachExternalBufferPtr (Ptr addr) sz = attachExternalBuffer addr sz

-- | Attach an external buffer with finalizers
attachFinalizedBuffer :: Addr# -> Word# -> IO Buffer
attachFinalizedBuffer addr sz = IO \s ->
  let !(# s', fin #) = newMutVar# [] s
  in (# s', OutBuffer addr sz (Finalizers fin) #)

-----------------------------------------------------------------
-- Finalizers
-----------------------------------------------------------------

-- | Insert a finalizer. Return True if there was no finalizer before
insertFinalizer :: Finalizers# RealWorld -> IO () -> IO Bool
insertFinalizer fin f = case fin of
  Finalizers rfs -> do
    atomicModifyIORef (IORef (STRef rfs)) $ \finalizers -> case finalizers of
      [] -> ([f] , True)
      fs -> (f:fs, False)
  NoFinalizers   -> error "insertFinalizer: can't insert finalizer (NoFinalizers)"

getFinalizers :: STBuffer s -> Finalizers# s
getFinalizers = \case
  InBuffer _ fin    -> fin
  OutBuffer _ _ fin -> fin

-- | Add a finalizer.
--
-- The latest added finalizers are executed first. Finalizers are not guaranteed
-- to run (e.g. if the program exits before the buffer is collected).
--
addFinalizer :: Buffer -> IO () -> IO ()
addFinalizer b f = do
   let !fin = getFinalizers b
   wasEmpty <- insertFinalizer fin f
   -- add the weak reference to the finalizer IORef (not to Addr#/byteArray#/...)
   when wasEmpty $ IO \s ->
    case mkWeak# fin b (unIO $ runFinalizers fin) s of
      (# s1, _wk #) -> (# s1, () #)

-- | Internal function used to execute finalizers
runFinalizers :: Finalizers# RealWorld -> IO ()
runFinalizers  = \case
  NoFinalizers   -> return ()
  Finalizers fin -> do
   -- atomically remove finalizers to avoid double execution
   fs <- atomicModifyIORef (IORef (STRef fin)) $ \fs -> ([], fs)
   sequence_ fs

-- | Indicate if a buffer is pinned
bufferIsPinned :: STBuffer s -> Bool
bufferIsPinned = \case
  OutBuffer {}  -> True
  InBuffer ba _ -> isTrue# (isMutableByteArrayPinned# ba)

-- | Touch a buffer to keep it alive
bufferTouch :: Buffer -> IO ()
bufferTouch b = IO \s -> case getFinalizers b of
  NoFinalizers   -> (# s, () #)
  Finalizers fin -> case touch# fin s of
                      s' -> (# s', () #)

withBuffer :: Buffer -> IO a -> IO a
withBuffer b f = do
  r <- f
  bufferTouch b
  pure r

-- | Get buffer size
bufferSize :: Buffer -> IO Word
bufferSize = \case
  OutBuffer _addr sz _fin -> pure (W# sz)
  InBuffer ba _fin        -> IO \s -> case getSizeofMutableByteArray# ba s of
   (# s', i #) -> (# s', W# (int2Word# i) #)

-- | Buffer address (careful with unpinned buffers!)
bufferAddr# :: Buffer -> Addr#
bufferAddr# = \case
  OutBuffer addr _ _ -> addr
  InBuffer ba _      -> byteArrayContents# (unsafeCoerce# ba)

-- | Get contents as a list of bytes
bufferToList :: Buffer -> IO [Word8]
bufferToList b = withBuffer b case b of
  OutBuffer addr sz _fin -> peekArray (W# sz) (Ptr addr)
  InBuffer {}
    | bufferIsPinned b -> do
        sz <- bufferSize b
        peekArray sz (Ptr (bufferAddr# b))

  InBuffer {} -> do
    sz <- bufferSize b
    let
       go i xs = do
          x <- bufferReadWord8 b i
          if i == 0
             then pure (x:xs)
             else go (i-1) (x:xs)
    go (sz-1) []

-- | Read a Word8, offset in bytes
--
-- We don't check that the offset is valid
bufferReadWord8 :: Buffer -> Word -> IO Word8
bufferReadWord8 b (W# off) = withBuffer b case b of
  InBuffer ba _fin -> IO \s -> case readWord8Array# ba (word2Int# off) s of
                        (# s2 , r #) -> (# s2 , W8# r #)

  OutBuffer addr _sz _fin -> IO \s -> case readWord8OffAddr# addr (word2Int# off) s of
                            (# s2 , r #) -> (# s2 , W8# r #)

-- | Read a Word16, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> let b = [0x12,0x34,0x56,0x78] :: Buffer
-- >>> x <- bufferReadWord16 b 0
-- >>> (x == 0x1234) || (x == 0x3412)
-- True
--
bufferReadWord16 :: Buffer -> Word -> IO Word16
bufferReadWord16 b (W# off) = withBuffer b case b of
  InBuffer ba _fin -> IO \s -> case readWord8ArrayAsWord16# ba (word2Int# off) s of
                        (# s2 , r #) -> (# s2 , W16# r #)

  OutBuffer addr _sz _fin -> IO \s -> case readWord16OffAddr# (addr `plusAddr#` word2Int# off) 0# s of
                            (# s2 , r #) -> (# s2 , W16# r #)


-- | Read a Word32, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> let b = [0x12,0x34,0x56,0x78] :: Buffer
-- >>> x <- bufferReadWord32 b 0
-- >>> (x == 0x12345678) || (x == 0x78563412)
-- True
--
bufferReadWord32 :: Buffer -> Word -> IO Word32
bufferReadWord32 b (W# off) = withBuffer b case b of
  InBuffer ba _fin -> IO \s -> case readWord8ArrayAsWord32# ba (word2Int# off) s of
                        (# s2 , r #) -> (# s2 , W32# r #)

  OutBuffer addr _sz _fin -> IO \s -> case readWord32OffAddr# (addr `plusAddr#` word2Int# off) 0# s of
                            (# s2 , r #) -> (# s2 , W32# r #)

-- | Read a Word64, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> let b = [0x12,0x34,0x56,0x78,0x9A,0xBC,0xDE,0xF0] :: Buffer
-- >>> x <- bufferReadWord64 b 0
-- >>> (x == 0x123456789ABCDEF0) || (x == 0xF0DEBC9A78563412)
-- True
--
bufferReadWord64 :: Buffer -> Word -> IO Word64
bufferReadWord64 b (W# off) = withBuffer b case b of
  InBuffer ba _fin -> IO \s -> case readWord8ArrayAsWord64# ba (word2Int# off) s of
                        (# s2 , r #) -> (# s2 , W64# r #)

  OutBuffer addr _sz _fin -> IO \s -> case readWord64OffAddr# (addr `plusAddr#` word2Int# off) 0# s of
                            (# s2 , r #) -> (# s2 , W64# r #)

-- | Do something with a buffer address
--
-- Note: don't write into immutable buffers as it would break referential
-- consistency
withBufferAddr# :: Buffer -> (Addr# -> IO a) -> IO a
withBufferAddr# b f = withBuffer b (f (bufferAddr# b))

-- | Write a Word8, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> b <- newBuffer 10
-- >>> bufferWriteWord8 b 1 123
-- >>> bufferReadWord8 b 1 
-- 123
--
bufferWriteWord8 :: Buffer -> Word -> Word8 -> IO ()
bufferWriteWord8 b (W# off) (W8# v) = withBuffer b case b of
  InBuffer ba _      -> IO \s -> case writeWord8Array# ba (word2Int# off) v s of s2 -> (# s2 , () #)
  OutBuffer addr _ _ -> IO \s -> case writeWord8OffAddr# addr (word2Int# off) v s of s2 -> (# s2 , () #)

-- | Write a Word16, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> b <- newBuffer 10
-- >>> let v = 1234 :: Word16
-- >>> bufferWriteWord16 b 1 v
-- >>> bufferReadWord16 b 1
-- 1234
--
-- >>> (x :: Word16) <- fromIntegral <$> bufferReadWord8 b 1
-- >>> (y :: Word16) <- fromIntegral <$> bufferReadWord8 b 2
-- >>> (((x `shiftL` 8) .|. y) == v)   ||   (((y `shiftL` 8) .|. x) == v)
-- True
--
bufferWriteWord16 :: Buffer -> Word -> Word16 -> IO ()
bufferWriteWord16 b (W# off) (W16# v) = withBuffer b case b of
  InBuffer ba _      -> IO \s -> case writeWord8ArrayAsWord16# ba (word2Int# off) v s of s2 -> (# s2 , () #)
  OutBuffer addr _ _ -> IO \s -> case writeWord16OffAddr# (addr `plusAddr#` word2Int# off) 0# v s of s2 -> (# s2 , () #)

-- | Write a Word32, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> b <- newBuffer 10
-- >>> let v = 1234 :: Word32
-- >>> bufferWriteWord32 b 1 v
-- >>> bufferReadWord32 b 1
-- 1234
--
bufferWriteWord32 :: Buffer -> Word -> Word32 -> IO ()
bufferWriteWord32 b (W# off) (W32# v) = withBuffer b case b of
  InBuffer ba _      -> IO \s -> case writeWord8ArrayAsWord32# ba (word2Int# off) v s of s2 -> (# s2 , () #)
  OutBuffer addr _ _ -> IO \s -> case writeWord32OffAddr# (addr `plusAddr#` word2Int# off) 0# v s of s2 -> (# s2 , () #)


-- | Write a Word64, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> b <- newBuffer 10
-- >>> let v = 1234 :: Word64
-- >>> bufferWriteWord64 b 1 v
-- >>> bufferReadWord64 b 1
-- 1234
--
bufferWriteWord64 :: Buffer -> Word -> Word64 -> IO ()
bufferWriteWord64 b (W# off) (W64# v) = withBuffer b case b of
  InBuffer ba _      -> IO \s -> case writeWord8ArrayAsWord64# ba (word2Int# off) v s of s2 -> (# s2 , () #)
  OutBuffer addr _ _ -> IO \s -> case writeWord64OffAddr# (addr `plusAddr#` word2Int# off) 0# v s of s2 -> (# s2 , () #)



-- | Support for OverloadedLists
--
-- >>> :set -XOverloadedLists
-- >>> let b = [25,26,27,28] :: Buffer
--
instance IsList Buffer where
   type Item Buffer = Word8
   toList b         = unsafePerformIO (bufferToList b)
   fromList xs      = unsafePerformIO do
      let sz = fromIntegral (length xs)
      b <- newBuffer sz
      forM_ ([0..] `zip` xs) \(i,x) -> do
         bufferWriteWord8 b i x
      pure b

   fromListN sz xs   = unsafePerformIO do
      b <- newBuffer (fromIntegral sz)
      forM_ ([0..] `zip` xs) \(i,x) -> do
         bufferWriteWord8 b i x
      pure b

-- | Copy a buffer into another from/to the given offsets
--
-- We don't check buffer limits.
--
-- >>> let b = [0,1,2,3,4,5,6,7,8] :: Buffer
-- >>> b2 <- newBuffer 8
-- >>> bufferCopy b 4 b2 0 4
-- >>> bufferCopy b 0 b2 4 4
-- >>> forM [0..7] (bufferReadWord8 b2)
-- [4,5,6,7,0,1,2,3]
--
bufferCopy
  :: Buffer  -- ^ Source buffer
  -> Word    -- ^ Offset in source buffer
  -> Buffer  -- ^ Target buffer
  -> Word    -- ^ Offset in target buffer
  -> Word    -- ^ Number of Word8 to copy
  -> IO ()
bufferCopy src (W# soff) dst (W# doff) (W# cnt) = withBuffer src $ withBuffer dst case (src,dst) of
  (InBuffer sba _, InBuffer dba _) -> IO \s ->
      case copyMutableByteArray# sba (word2Int# soff) dba (word2Int# doff) (word2Int# cnt) s of
         s2 -> (# s2, () #)
  (InBuffer sba _, OutBuffer addr _ _) -> IO \s ->
      case copyMutableByteArrayToAddr# sba (word2Int# soff) (addr `plusAddr#` word2Int# doff) (word2Int# cnt) s of
         s2 -> (# s2, () #)
  (OutBuffer addr _ _, InBuffer dba _) -> IO \s ->
      case copyAddrToByteArray# (addr `plusAddr#` word2Int# soff) dba (word2Int# doff) (word2Int# cnt) s of
         s2 -> (# s2, () #)
  (OutBuffer addr1 _ _, OutBuffer addr2 _ _) ->
    memcpy# (addr1 `plusAddr#` word2Int# soff) (addr2 `plusAddr#` word2Int# doff) (word2Int# cnt)
