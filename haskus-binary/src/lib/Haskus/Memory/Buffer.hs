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
   , withBufferPtr
   , bufferSize
   , bufferToList
   -- * Finalizers
   , addFinalizer
   , makeFinalizable
   , touchBuffer
   )
where

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Ptr
import Haskus.Utils.Monad

import qualified Data.Primitive.ByteArray as BA
import qualified Data.Primitive.Types     as BA
import Control.Monad.Primitive
import Data.IORef
import Unsafe.Coerce

import GHC.Prim
import GHC.Weak
import GHC.Exts (toList)

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

data Buffer (mut :: Mutability) (pin :: Pinning) (gc :: Management) where
   Buffer    :: BA.ByteArray                                -> Buffer 'Immutable 'NotPinned 'Managed
   BufferP   :: BA.ByteArray                                -> Buffer 'Immutable 'Pinned    'Managed
   BufferM   :: BA.MutableByteArray RealWorld               -> Buffer 'Mutable   'NotPinned 'Managed
   BufferMP  :: BA.MutableByteArray RealWorld               -> Buffer 'Mutable   'Pinned    'Managed
   BufferE   :: Addr# -> Word                               -> Buffer 'Mutable   'Pinned    'NotManaged
   BufferF   :: BA.ByteArray                  -> Finalizers -> Buffer 'Immutable 'NotPinned 'Finalized
   BufferPF  :: BA.ByteArray                  -> Finalizers -> Buffer 'Immutable 'Pinned    'Finalized
   BufferMF  :: BA.MutableByteArray RealWorld -> Finalizers -> Buffer 'Mutable   'NotPinned 'Finalized
   BufferMPF :: BA.MutableByteArray RealWorld -> Finalizers -> Buffer 'Mutable   'Pinned    'Finalized
   BufferEF  :: Addr# -> Word                 -> Finalizers -> Buffer 'Mutable   'Pinned    'Finalized

-- | A buffer with an additional phantom type indicating its binary format
newtype TypedBuffer (t :: k) mut pin gc = TypedBuffer (Buffer mut pin gc)

-----------------------------------------------------------------
-- Allocation
-----------------------------------------------------------------

-- | Allocate a buffer (mutable, unpinned)
newBuffer :: MonadIO m => Word -> m (Buffer 'Mutable 'NotPinned 'Managed)
newBuffer sz = BufferM <$> liftIO (BA.newByteArray (fromIntegral sz))

-- | Allocate a buffer (mutable, pinned)
newPinnedBuffer :: MonadIO m => Word -> m (Buffer 'Mutable 'Pinned 'Managed)
newPinnedBuffer sz = BufferMP <$> liftIO (BA.newPinnedByteArray (fromIntegral sz))

-- | Allocate an aligned buffer (mutable, pinned)
newAlignedPinnedBuffer :: MonadIO m => Word -> Word -> m (Buffer 'Mutable 'Pinned 'Managed)
newAlignedPinnedBuffer sz al = BufferMP <$> liftIO (BA.newAlignedPinnedByteArray (fromIntegral sz) (fromIntegral al))

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
addFinalizer :: Buffer mut pin 'Finalized -> IO () -> IO ()
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
touchBuffer :: MonadIO m => Buffer mut pin gc -> m ()
touchBuffer (Buffer    ba                        ) = liftIO $ touch ba
touchBuffer (BufferP   ba                        ) = liftIO $ touch ba
touchBuffer (BufferM   ba                        ) = liftIO $ touch ba
touchBuffer (BufferMP  ba                        ) = liftIO $ touch ba
touchBuffer (BufferF   ba         _fin           ) = liftIO $ touch ba
touchBuffer (BufferPF  ba         _fin           ) = liftIO $ touch ba
touchBuffer (BufferMF  ba         _fin           ) = liftIO $ touch ba
touchBuffer (BufferMPF ba         _fin           ) = liftIO $ touch ba
touchBuffer (BufferE   _addr _sz                 ) = return ()
touchBuffer (BufferEF  _addr _sz (Finalizers fin)) = liftIO $ touch fin

-- | Make a buffer finalizable
--
-- The new buffer liveness is used to trigger finalizers.
--
makeFinalizable :: Buffer mut pin f -> IO (Buffer mut pin 'Finalized)
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
withBufferAddr# :: Buffer mut 'Pinned gc -> (Addr# -> IO a) -> IO a
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

-- | Do something with a buffer pointer
--
-- Note: don't write into immutable buffer as it would break referential
-- consistency
withBufferPtr :: Buffer mut 'Pinned gc -> (Ptr b -> IO a) -> IO a
withBufferPtr b f = withBufferAddr# b g
   where
      g addr = f (Ptr addr)


-- | Get buffer size
bufferSize :: MonadIO m => Buffer mut pin gc -> m Word
bufferSize = \case
   BufferM ba             -> fromIntegral <$> liftIO (BA.getSizeofMutableByteArray ba)
   BufferMP ba            -> fromIntegral <$> liftIO (BA.getSizeofMutableByteArray ba)
   BufferMF  ba _fin      -> fromIntegral <$> liftIO (BA.getSizeofMutableByteArray ba)
   BufferMPF ba _fin      -> fromIntegral <$> liftIO (BA.getSizeofMutableByteArray ba)
   BufferE  _addr sz      -> return sz
   BufferEF _addr sz _fin -> return sz
   Buffer  ba             -> return $ fromIntegral $ BA.sizeofByteArray ba
   BufferP ba             -> return $ fromIntegral $ BA.sizeofByteArray ba
   BufferF   ba _fin      -> return $ fromIntegral $ BA.sizeofByteArray ba
   BufferPF  ba _fin      -> return $ fromIntegral $ BA.sizeofByteArray ba


-- | Get contents as a list of bytes
bufferToList :: MonadIO m => Buffer mut pin gc -> m [Word8]
bufferToList = \case
   Buffer  ba             -> return (toList ba)
   BufferP ba             -> return (toList ba)
   BufferF   ba _fin      -> return (toList ba)
   BufferPF  ba _fin      -> return (toList ba)
   BufferM ba             -> return (toList (unsafeCoerce ba :: BA.ByteArray))
   BufferMP ba            -> return (toList (unsafeCoerce ba :: BA.ByteArray))
   BufferMF  ba _fin      -> return (toList (unsafeCoerce ba :: BA.ByteArray))
   BufferMPF ba _fin      -> return (toList (unsafeCoerce ba :: BA.ByteArray))
   BufferE  addr sz       -> peekArray sz (Ptr addr)
   BufferEF addr sz _fin  -> peekArray sz (Ptr addr)

