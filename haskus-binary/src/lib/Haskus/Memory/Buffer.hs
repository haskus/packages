{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}

module Haskus.Memory.Buffer
   ( Buffer (..)
   , TypedBuffer (..)
   , Finalizers
   , module Control.Monad.Primitive
   , newBuffer
   , newPinnedBuffer
   , newAlignedPinnedBuffer
   , Pinning (..)
   , Management (..)
   , Mutability (..)
   , withBufferAddr#
   , bufferSize
   -- * Finalizers
   , addFinalizer
   , makeFinalizable
   , touchBuffer
   )
where

import qualified Data.Primitive.ByteArray as BA
import qualified Data.Primitive.Types     as BA
import Control.Monad.Primitive
import Control.Monad
import Data.IORef

import GHC.Prim
import GHC.Weak

-- | Is the buffer pinned into memory?
data Pinning
   = Pinned
   | NotPinned
   deriving (Show,Eq)

-- | Is the buffer automatically garbage collected?
data Management
   = Managed    -- ^ GCed
   | Finalized  -- ^ Finalizers run just before GCing
   | NotManaged -- ^ Not managed (e.g., pointer to external heap)
   deriving (Show,Eq)

-- | Is the buffer mutable or not
data Mutability
   = Mutable
   | Immutable
   deriving (Show,Eq)

data Buffer s (mut :: Mutability) (pin :: Pinning) (gc :: Management) where
   Buffer    :: BA.ByteArray                        -> Buffer s 'Immutable 'NotPinned 'Managed
   BufferP   :: BA.ByteArray                        -> Buffer s 'Immutable 'Pinned    'Managed
   BufferM   :: BA.MutableByteArray s               -> Buffer s 'Mutable   'NotPinned 'Managed
   BufferMP  :: BA.MutableByteArray s               -> Buffer s 'Mutable   'Pinned    'Managed
   BufferE   :: Addr# -> Word                       -> Buffer s 'Mutable   'Pinned    'NotManaged
   BufferF   :: BA.ByteArray          -> Finalizers -> Buffer s 'Immutable 'NotPinned 'Finalized
   BufferPF  :: BA.ByteArray          -> Finalizers -> Buffer s 'Immutable 'Pinned    'Finalized
   BufferMF  :: BA.MutableByteArray s -> Finalizers -> Buffer s 'Mutable   'NotPinned 'Finalized
   BufferMPF :: BA.MutableByteArray s -> Finalizers -> Buffer s 'Mutable   'Pinned    'Finalized
   BufferEF  :: Addr# -> Word         -> Finalizers -> Buffer s 'Mutable   'Pinned    'Finalized

-- | A buffer with an additional phantom type indicating its binary format
newtype TypedBuffer (t :: k) s mut pin gc = TypedBuffer (Buffer s mut pin gc)

-----------------------------------------------------------------
-- Allocation
-----------------------------------------------------------------

-- | Allocate a buffer (mutable, unpinned)
newBuffer :: PrimMonad m => Word -> m (Buffer (PrimState m) 'Mutable 'NotPinned 'Managed)
newBuffer sz = BufferM <$> BA.newByteArray (fromIntegral sz)

-- | Allocate a buffer (mutable, pinned)
newPinnedBuffer :: PrimMonad m => Word -> m (Buffer (PrimState m) 'Mutable 'Pinned 'Managed)
newPinnedBuffer sz = BufferMP <$> BA.newPinnedByteArray (fromIntegral sz)

-- | Allocate an aligned buffer (mutable, pinned)
newAlignedPinnedBuffer :: PrimMonad m => Word -> Word -> m (Buffer (PrimState m) 'Mutable 'Pinned 'Managed)
newAlignedPinnedBuffer sz al = BufferMP <$> BA.newAlignedPinnedByteArray (fromIntegral sz) (fromIntegral al)

-----------------------------------------------------------------
-- Finalizers
-----------------------------------------------------------------

newtype Finalizers = Finalizers (IORef [IO ()])

-- | Insert a finalizer. Return True if there was no finalizer before
insertFinalizer :: Finalizers -> IO () -> IO Bool
insertFinalizer (Finalizers rfs) f = do
  atomicModifyIORef rfs $ \finalizers -> case finalizers of
    [] -> ([f] , True)
    fs -> (f:fs, False)

-- | Add a finalizer.
addFinalizer :: Buffer s mut pin 'Finalized -> IO () -> IO ()
addFinalizer b f = case b of
   BufferEF _addr _sz fin@(Finalizers rfs) -> do
      wasEmpty <- insertFinalizer fin f
      -- add the weak reference to the finalizer IORef (not to Addr#)
      when wasEmpty $ void $ mkWeakIORef rfs (runFinalizers fin)

   BufferF ba fin -> do
      wasEmpty <- insertFinalizer fin f
      -- add the weak reference to the ByteArray
      when wasEmpty $ void $ mkWeak ba () (Just (runFinalizers fin))

   BufferPF ba fin -> do
      wasEmpty <- insertFinalizer fin f
      -- add the weak reference to the ByteArray
      when wasEmpty $ void $ mkWeak ba () (Just (runFinalizers fin))

   BufferMF ba fin -> do
      wasEmpty <- insertFinalizer fin f
      -- add the weak reference to the MutableByteArray
      when wasEmpty $ void $ mkWeak ba () (Just (runFinalizers fin))

   BufferMPF ba fin -> do
      wasEmpty <- insertFinalizer fin f
      -- add the weak reference to the MutableByteArray
      when wasEmpty $ void $ mkWeak ba () (Just (runFinalizers fin))

-- | Internal function used to execute finalizers
runFinalizers :: Finalizers -> IO ()
runFinalizers (Finalizers rfs) = do
   -- atomically remove finalizers to avoid double execution
   fs <- atomicModifyIORef rfs $ \fs -> ([], fs)
   sequence_ fs

-- | Create empty Finalizers
newFinalizers :: IO Finalizers
newFinalizers = Finalizers <$> newIORef []

-- | Touch a buffer
touchBuffer :: PrimMonad m => Buffer s mut pin gc -> m ()
touchBuffer (Buffer    ba                        ) = touch ba
touchBuffer (BufferP   ba                        ) = touch ba
touchBuffer (BufferM   ba                        ) = touch ba
touchBuffer (BufferMP  ba                        ) = touch ba
touchBuffer (BufferF   ba         _fin           ) = touch ba
touchBuffer (BufferPF  ba         _fin           ) = touch ba
touchBuffer (BufferMF  ba         _fin           ) = touch ba
touchBuffer (BufferMPF ba         _fin           ) = touch ba
touchBuffer (BufferE   _addr _sz                 ) = return ()
touchBuffer (BufferEF  _addr _sz (Finalizers fin)) = touch fin

-- | Make a buffer finalizable
--
-- The new buffer liveness is used to trigger finalizers.
--
makeFinalizable :: Buffer s mut pin f -> IO (Buffer s mut pin 'Finalized)
makeFinalizable (BufferE addr sz) = BufferEF  addr sz <$> newFinalizers
makeFinalizable (Buffer  ba  )    = BufferF   ba      <$> newFinalizers
makeFinalizable (BufferP ba  )    = BufferPF  ba      <$> newFinalizers
makeFinalizable (BufferM ba  )    = BufferMF  ba      <$> newFinalizers
makeFinalizable (BufferMP ba )    = BufferMPF ba      <$> newFinalizers
makeFinalizable x@(BufferF {})    = return x
makeFinalizable x@(BufferEF {})   = return x
makeFinalizable x@(BufferPF {})   = return x
makeFinalizable x@(BufferMF {})   = return x
makeFinalizable x@(BufferMPF {})  = return x


-- | Do something with a buffer address
--
-- Note: don't write into immutable buffer as it would break referential
-- consistency
withBufferAddr# :: Buffer s mut 'Pinned gc -> (Addr# -> IO a) -> IO a
withBufferAddr# b@(BufferP ba) f = do
   let !(BA.Addr addr) = BA.byteArrayContents ba
   r <- f addr
   touch b
   return r
withBufferAddr# b@(BufferMP ba) f = do
   let !(BA.Addr addr) = BA.mutableByteArrayContents ba
   r <- f addr
   touch b
   return r
withBufferAddr# b@(BufferPF ba _fin) f = do
   let !(BA.Addr addr) = BA.byteArrayContents ba
   r <- f addr
   touch b
   return r
withBufferAddr# b@(BufferMPF ba _fin) f = do
   let !(BA.Addr addr) = BA.mutableByteArrayContents ba
   r <- f addr
   touch b
   return r
withBufferAddr# (BufferE addr _sz)         f = f (addr)
withBufferAddr# b@(BufferEF addr _sz _fin) f = do
   r <- f addr
   touch b
   return r


-- | Get buffer size
bufferSize :: PrimMonad m => Buffer (PrimState m) mut pin gc -> m Word
bufferSize = \case
   BufferM ba             -> fromIntegral <$> BA.getSizeofMutableByteArray ba
   BufferMP ba            -> fromIntegral <$> BA.getSizeofMutableByteArray ba
   BufferMF  ba _fin      -> fromIntegral <$> BA.getSizeofMutableByteArray ba
   BufferMPF ba _fin      -> fromIntegral <$> BA.getSizeofMutableByteArray ba
   BufferE  _addr sz      -> return sz
   BufferEF _addr sz _fin -> return sz
   Buffer  ba             -> return $ fromIntegral $ BA.sizeofByteArray ba
   BufferP ba             -> return $ fromIntegral $ BA.sizeofByteArray ba
   BufferF   ba _fin      -> return $ fromIntegral $ BA.sizeofByteArray ba
   BufferPF  ba _fin      -> return $ fromIntegral $ BA.sizeofByteArray ba
