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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | A buffer in memory
module Haskus.Data.Buffer
   ( Buffer (..)
   , TypedBuffer (..)
   -- * Buffers taxonomy
   , Pinning (..)
   , Finalization (..)
   , Mutability (..)
   , BufferI
   , BufferP
   , BufferM
   , BufferMP
   , BufferME
   , BufferE
   , BufferF
   , BufferPF
   , BufferMF
   , BufferMPF
   , BufferMEF
   , BufferEF
   -- * Operations
   , newBuffer
   , newPinnedBuffer
   , newAlignedPinnedBuffer
   , withBufferAddr#
   , withBufferPtr
   , bufferSizeIO
   , BufferSize (..)
   , bufferToListIO
   , BufferToList (..)
   , bufferReadWord8IO
   , bufferReadWord8
   , bufferWriteWord8IO
   , bufferReadWord16IO
   , bufferReadWord16
   , bufferWriteWord16IO
   , bufferReadWord32IO
   , bufferReadWord32
   , bufferWriteWord32IO
   , bufferReadWord64IO
   , bufferReadWord64
   , bufferWriteWord64IO
   , copyBuffer
   -- * Finalizers
   , Finalizers
   , addFinalizer
   , makeFinalizable
   , touchBuffer
   -- * Reexport
   , module Control.Monad.Primitive
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
import GHC.Exts (toList, IsList(..))
import GHC.Types (IO(..))

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> :set -XScopedTypeVariables
-- >>> import Haskus.Format.Binary.Bits

-- | Is the buffer pinned into memory?
data Pinning
   = Pinned    -- ^ The buffer has a fixed associated memory address
   | NotPinned -- ^ The buffer contents can be freely moved to another address
   deriving (Show,Eq)

-- | Is the buffer automatically garbage collected?
data Finalization
   = Collected    -- ^ Automatically collected by the garbage-collector
   | Finalized    -- ^ Finalizers are run just before GCing
   | NotFinalized -- ^ Not managed at all
   deriving (Show,Eq)

-- | Allocation heap
data Heap
   = Internal -- ^ GHC heap
   | External -- ^ External heap

-- | Is the buffer mutable or not?
data Mutability
   = Mutable   -- ^ Memory cells are mutable
   | Immutable -- ^ Memory cells are immutable
   deriving (Show,Eq)

data Buffer (mut :: Mutability) (pin :: Pinning) (fin :: Finalization) (heap :: Heap) where
   Buffer    :: BA.ByteArray                                -> Buffer 'Immutable 'NotPinned 'Collected    'Internal
   BufferP   :: BA.ByteArray                                -> Buffer 'Immutable 'Pinned    'Collected    'Internal
   BufferM   :: BA.MutableByteArray RealWorld               -> Buffer 'Mutable   'NotPinned 'Collected    'Internal
   BufferMP  :: BA.MutableByteArray RealWorld               -> Buffer 'Mutable   'Pinned    'Collected    'Internal
   BufferME  :: Addr# -> Word                               -> Buffer 'Mutable   'Pinned    'NotFinalized 'External
   BufferE   :: Addr# -> Word                               -> Buffer 'Immutable 'Pinned    'NotFinalized 'External
   BufferF   :: BA.ByteArray                  -> Finalizers -> Buffer 'Immutable 'NotPinned 'Finalized    'Internal
   BufferPF  :: BA.ByteArray                  -> Finalizers -> Buffer 'Immutable 'Pinned    'Finalized    'Internal
   BufferMF  :: BA.MutableByteArray RealWorld -> Finalizers -> Buffer 'Mutable   'NotPinned 'Finalized    'Internal
   BufferMPF :: BA.MutableByteArray RealWorld -> Finalizers -> Buffer 'Mutable   'Pinned    'Finalized    'Internal
   BufferMEF :: Addr# -> Word                 -> Finalizers -> Buffer 'Mutable   'Pinned    'Finalized    'External
   BufferEF  :: Addr# -> Word                 -> Finalizers -> Buffer 'Immutable 'Pinned    'Finalized    'External

type BufferI   = Buffer 'Immutable 'NotPinned 'Collected    'Internal
type BufferP   = Buffer 'Immutable 'Pinned    'Collected    'Internal
type BufferM   = Buffer 'Mutable   'NotPinned 'Collected    'Internal
type BufferMP  = Buffer 'Mutable   'Pinned    'Collected    'Internal
type BufferME  = Buffer 'Mutable   'Pinned    'NotFinalized 'External
type BufferE   = Buffer 'Immutable 'Pinned    'NotFinalized 'External
type BufferF   = Buffer 'Immutable 'NotPinned 'Finalized    'Internal
type BufferPF  = Buffer 'Immutable 'Pinned    'Finalized    'Internal
type BufferMF  = Buffer 'Mutable   'NotPinned 'Finalized    'Internal
type BufferMPF = Buffer 'Mutable   'Pinned    'Finalized    'Internal
type BufferMEF = Buffer 'Mutable   'Pinned    'Finalized    'External
type BufferEF  = Buffer 'Immutable 'Pinned    'Finalized    'External

-- | A buffer with an additional phantom type indicating its binary format
newtype TypedBuffer (t :: k) mut pin fin heap = TypedBuffer (Buffer mut pin fin heap)

-----------------------------------------------------------------
-- Allocation
-----------------------------------------------------------------

-- | Allocate a buffer (mutable, unpinned)
--
-- >>> b <- newBuffer 1024
--
newBuffer :: MonadIO m => Word -> m BufferM
newBuffer sz = BufferM <$> liftIO (BA.newByteArray (fromIntegral sz))

-- | Allocate a buffer (mutable, pinned)
newPinnedBuffer :: MonadIO m => Word -> m BufferMP
newPinnedBuffer sz = BufferMP <$> liftIO (BA.newPinnedByteArray (fromIntegral sz))

-- | Allocate an aligned buffer (mutable, pinned)
newAlignedPinnedBuffer :: MonadIO m => Word -> Word -> m BufferMP
newAlignedPinnedBuffer sz al = BufferMP <$> liftIO (BA.newAlignedPinnedByteArray (fromIntegral sz) (fromIntegral al))

-----------------------------------------------------------------
-- Finalizers
-----------------------------------------------------------------

newtype Finalizers = Finalizers (IORef [IO ()])

-- | Insert a finalizer. Return True if there was no finalizer before
insertFinalizer :: MonadIO m => Finalizers -> IO () -> m Bool
insertFinalizer (Finalizers rfs) f = do
  liftIO $ atomicModifyIORef rfs $ \finalizers -> case finalizers of
    [] -> ([f] , True)
    fs -> (f:fs, False)

-- | Add a finalizer.
--
-- The latest added finalizers are executed first. Finalizers are not guaranteed
-- to run (e.g. if the program exits before the buffer is collected).
--
addFinalizer :: MonadIO m => Buffer mut pin 'Finalized heap -> IO () -> m ()
addFinalizer b f = case b of
   BufferMEF _addr _sz fin@(Finalizers rfs) -> do
      wasEmpty <- insertFinalizer fin f
      -- add the weak reference to the finalizer IORef (not to Addr#)
      when wasEmpty $ void $ liftIO $ mkWeakIORef rfs (runFinalizers fin)

   BufferEF _addr _sz fin@(Finalizers rfs) -> do
      wasEmpty <- insertFinalizer fin f
      -- add the weak reference to the finalizer IORef (not to Addr#)
      when wasEmpty $ void $ liftIO $ mkWeakIORef rfs (runFinalizers fin)

   BufferF ba fin -> do
      wasEmpty <- insertFinalizer fin f
      -- add the weak reference to the ByteArray
      when wasEmpty $ void $ liftIO $ mkWeak ba () (Just (runFinalizers fin))

   BufferPF ba fin -> do
      wasEmpty <- insertFinalizer fin f
      -- add the weak reference to the ByteArray
      when wasEmpty $ void $ liftIO $ mkWeak ba () (Just (runFinalizers fin))

   BufferMF ba fin -> do
      wasEmpty <- insertFinalizer fin f
      -- add the weak reference to the MutableByteArray
      when wasEmpty $ void $ liftIO $ mkWeak ba () (Just (runFinalizers fin))

   BufferMPF ba fin -> do
      wasEmpty <- insertFinalizer fin f
      -- add the weak reference to the MutableByteArray
      when wasEmpty $ void $ liftIO $ mkWeak ba () (Just (runFinalizers fin))

-- | Internal function used to execute finalizers
runFinalizers :: Finalizers -> IO ()
runFinalizers (Finalizers rfs) = do
   -- atomically remove finalizers to avoid double execution
   fs <- atomicModifyIORef rfs $ \fs -> ([], fs)
   sequence_ fs

-- | Create empty Finalizers
newFinalizers :: MonadIO m => m Finalizers
newFinalizers = Finalizers <$> liftIO (newIORef [])

-- | Touch a buffer
touchBuffer :: MonadIO m => Buffer mut pin fin heap -> m ()
touchBuffer (Buffer    ba                        ) = liftIO $ touch ba
touchBuffer (BufferP   ba                        ) = liftIO $ touch ba
touchBuffer (BufferM   ba                        ) = liftIO $ touch ba
touchBuffer (BufferMP  ba                        ) = liftIO $ touch ba
touchBuffer (BufferF   ba         _fin           ) = liftIO $ touch ba
touchBuffer (BufferPF  ba         _fin           ) = liftIO $ touch ba
touchBuffer (BufferMF  ba         _fin           ) = liftIO $ touch ba
touchBuffer (BufferMPF ba         _fin           ) = liftIO $ touch ba
touchBuffer (BufferME  _addr _sz                 ) = return ()
touchBuffer (BufferE   _addr _sz                 ) = return ()
touchBuffer (BufferMEF _addr _sz (Finalizers fin)) = liftIO $ touch fin
touchBuffer (BufferEF  _addr _sz (Finalizers fin)) = liftIO $ touch fin

-- | Make a buffer finalizable
--
-- The new buffer liveness is used to trigger finalizers.
--
makeFinalizable :: MonadIO m => Buffer mut pin f heap -> m (Buffer mut pin 'Finalized heap)
makeFinalizable (BufferME addr sz) = BufferMEF addr sz <$> newFinalizers
makeFinalizable (BufferE  addr sz) = BufferEF  addr sz <$> newFinalizers
makeFinalizable (Buffer   ba  )    = BufferF   ba      <$> newFinalizers
makeFinalizable (BufferP  ba  )    = BufferPF  ba      <$> newFinalizers
makeFinalizable (BufferM  ba  )    = BufferMF  ba      <$> newFinalizers
makeFinalizable (BufferMP ba  )    = BufferMPF ba      <$> newFinalizers
makeFinalizable x@(BufferF {})     = return x
makeFinalizable x@(BufferMEF{})    = return x
makeFinalizable x@(BufferEF{})     = return x
makeFinalizable x@(BufferPF {})    = return x
makeFinalizable x@(BufferMF {})    = return x
makeFinalizable x@(BufferMPF {})   = return x


-- | Do something with a buffer address
--
-- Note: don't write into immutable buffer as it would break referential
-- consistency
withBufferAddr# :: Buffer mut 'Pinned fin heap -> (Addr# -> IO a) -> IO a
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
withBufferAddr# (BufferME addr _sz)         f = f (addr)
withBufferAddr# (BufferE  addr _sz)         f = f (addr)
withBufferAddr# b@(BufferMEF addr _sz _fin) f = do
   r <- f addr
   touch b
   return r
withBufferAddr# b@(BufferEF addr _sz _fin)  f = do
   r <- f addr
   touch b
   return r

-- | Do something with a buffer pointer
--
-- Note: don't write into immutable buffer as it would break referential
-- consistency
withBufferPtr :: Buffer mut 'Pinned fin heap -> (Ptr b -> IO a) -> IO a
withBufferPtr b f = withBufferAddr# b g
   where
      g addr = f (Ptr addr)


-- | Get buffer size
bufferSizeIO :: MonadIO m => Buffer mut pin fin heap -> m Word
bufferSizeIO = \case
   BufferM ba              -> fromIntegral <$> liftIO (BA.getSizeofMutableByteArray ba)
   BufferMP ba             -> fromIntegral <$> liftIO (BA.getSizeofMutableByteArray ba)
   BufferMF  ba _fin       -> fromIntegral <$> liftIO (BA.getSizeofMutableByteArray ba)
   BufferMPF ba _fin       -> fromIntegral <$> liftIO (BA.getSizeofMutableByteArray ba)
   BufferME  _addr sz      -> return sz
   BufferMEF _addr sz _fin -> return sz
   BufferE   _addr sz      -> return sz
   BufferEF  _addr sz _fin -> return sz
   Buffer  ba              -> return $ fromIntegral $ BA.sizeofByteArray ba
   BufferP ba              -> return $ fromIntegral $ BA.sizeofByteArray ba
   BufferF   ba _fin       -> return $ fromIntegral $ BA.sizeofByteArray ba
   BufferPF  ba _fin       -> return $ fromIntegral $ BA.sizeofByteArray ba

class BufferSize a where
   -- |  Get buffer size
   bufferSize :: a -> Word

instance BufferSize BufferI where
   bufferSize (Buffer ba)  = fromIntegral $ BA.sizeofByteArray ba
instance BufferSize BufferP where
   bufferSize (BufferP ba) = fromIntegral $ BA.sizeofByteArray ba
instance BufferSize BufferF where
   bufferSize (BufferF ba _fin)  = fromIntegral $ BA.sizeofByteArray ba
instance BufferSize BufferPF where
   bufferSize (BufferPF ba _fin) = fromIntegral $ BA.sizeofByteArray ba
instance BufferSize BufferME where
   bufferSize (BufferME _addr sz) = sz
instance BufferSize BufferMEF where
   bufferSize (BufferMEF _addr sz _fin) = sz
instance BufferSize BufferE where
   bufferSize (BufferE _addr sz) = sz
instance BufferSize BufferEF where
   bufferSize (BufferEF _addr sz _fin) = sz

-- | Get contents as a list of bytes
bufferToListIO :: MonadIO m => Buffer mut pin fin heap -> m [Word8]
bufferToListIO = \case
   Buffer  ba             -> return (toList ba)
   BufferP ba             -> return (toList ba)
   BufferF   ba _fin      -> return (toList ba)
   BufferPF  ba _fin      -> return (toList ba)
   BufferM ba             -> return (toList (unsafeCoerce ba :: BA.ByteArray))
   BufferMP ba            -> return (toList (unsafeCoerce ba :: BA.ByteArray))
   BufferMF  ba _fin      -> return (toList (unsafeCoerce ba :: BA.ByteArray))
   BufferMPF ba _fin      -> return (toList (unsafeCoerce ba :: BA.ByteArray))
   BufferME addr sz       -> peekArray sz (Ptr addr)
   BufferMEF addr sz _fin -> peekArray sz (Ptr addr)
   BufferE  addr sz       -> peekArray sz (Ptr addr)
   BufferEF  addr sz _fin -> peekArray sz (Ptr addr)

class BufferToList a where
   -- | Get contents as a list of bytes
   bufferToList :: a -> [Word8]

instance BufferToList BufferI where
   bufferToList (Buffer ba) = toList ba
instance BufferToList BufferP where
   bufferToList (BufferP ba) = toList ba
instance BufferToList BufferF where
   bufferToList (BufferF ba _fin) = toList ba
instance BufferToList BufferPF where
   bufferToList (BufferPF ba _fin) = toList ba

-- | Support for OverloadedLists
--
-- >>> :set -XOverloadedLists
-- >>> let b = [25,26,27,28] :: BufferI
--
instance IsList BufferI where
   type Item BufferI  = Word8
   toList (Buffer ba) = toList ba
   fromList xs        = Buffer (fromList xs)
   fromListN sz xs    = Buffer (fromListN sz xs)


-- | Read a Word8, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> let b = [25,26,27,28] :: BufferI
-- >>> bufferReadWord8IO b 2 
-- 27
--
bufferReadWord8IO :: MonadIO m => Buffer mut pin fin heap -> Word -> m Word8
bufferReadWord8IO b (fromIntegral -> !(I# off)) = case b of
   BufferM (BA.MutableByteArray ba)          -> liftIO $ IO $ \s -> case readWord8Array# ba off s of (# s2 , r #) -> (# s2 , W8# r #)
   BufferMP (BA.MutableByteArray ba)         -> liftIO $ IO $ \s -> case readWord8Array# ba off s of (# s2 , r #) -> (# s2 , W8# r #)
   BufferMF  (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case readWord8Array# ba off s of (# s2 , r #) -> (# s2 , W8# r #)
   BufferMPF (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case readWord8Array# ba off s of (# s2 , r #) -> (# s2 , W8# r #)
   BufferME  addr _sz                        -> liftIO $ IO $ \s -> case readWord8OffAddr# addr off s of (# s2 , r #) -> (# s2 , W8# r #)
   BufferMEF addr _sz _fin                   -> liftIO $ IO $ \s -> case readWord8OffAddr# addr off s of (# s2 , r #) -> (# s2 , W8# r #)
   BufferE   addr _sz                        -> liftIO $ IO $ \s -> case readWord8OffAddr# addr off s of (# s2 , r #) -> (# s2 , W8# r #)
   BufferEF  addr _sz _fin                   -> liftIO $ IO $ \s -> case readWord8OffAddr# addr off s of (# s2 , r #) -> (# s2 , W8# r #)
   Buffer  (BA.ByteArray ba)                 -> return (W8# (indexWord8Array# ba off))
   BufferP (BA.ByteArray ba)                 -> return (W8# (indexWord8Array# ba off))
   BufferF   (BA.ByteArray ba) _fin          -> return (W8# (indexWord8Array# ba off))
   BufferPF  (BA.ByteArray ba) _fin          -> return (W8# (indexWord8Array# ba off))

-- | Read a Word8 in an immutable buffer, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> let b = [25,26,27,28] :: BufferI
-- >>> putStrLn $ "Word8 at offset 2 is " ++ show (bufferReadWord8 b 2)
-- Word8 at offset 2 is 27
--
bufferReadWord8 :: Buffer 'Immutable pin fin heap -> Word -> Word8
bufferReadWord8 b (fromIntegral -> !(I# off)) = case b of
   Buffer  (BA.ByteArray ba)                 -> W8# (indexWord8Array# ba off)
   BufferP (BA.ByteArray ba)                 -> W8# (indexWord8Array# ba off)
   BufferF   (BA.ByteArray ba) _fin          -> W8# (indexWord8Array# ba off)
   BufferPF  (BA.ByteArray ba) _fin          -> W8# (indexWord8Array# ba off)
   BufferE  addr _sz                         -> W8# (indexWord8OffAddr# (addr `plusAddr#` off) 0#)
   BufferEF addr _sz _fin                    -> W8# (indexWord8OffAddr# (addr `plusAddr#` off) 0#)

-- | Write a Word8, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> b <- newBuffer 10
-- >>> bufferWriteWord8IO b 1 123
-- >>> bufferReadWord8IO b 1 
-- 123
--
bufferWriteWord8IO :: MonadIO m => Buffer 'Mutable pin fin heap -> Word -> Word8 -> m ()
bufferWriteWord8IO b (fromIntegral -> !(I# off)) (W8# v) = case b of
   BufferM (BA.MutableByteArray ba)          -> liftIO $ IO $ \s -> case writeWord8Array# ba off v s of s2 -> (# s2 , () #)
   BufferMP (BA.MutableByteArray ba)         -> liftIO $ IO $ \s -> case writeWord8Array# ba off v s of s2 -> (# s2 , () #)
   BufferMF  (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case writeWord8Array# ba off v s of s2 -> (# s2 , () #)
   BufferMPF (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case writeWord8Array# ba off v s of s2 -> (# s2 , () #)
   BufferME  addr _sz                        -> liftIO $ IO $ \s -> case writeWord8OffAddr# addr off v s of s2 -> (# s2 , () #)
   BufferMEF addr _sz _fin                   -> liftIO $ IO $ \s -> case writeWord8OffAddr# addr off v s of s2 -> (# s2 , () #)





-- | Read a Word16, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> let b = [0x12,0x34,0x56,0x78] :: BufferI
-- >>> x <- bufferReadWord16IO b 0
-- >>> (x == 0x1234) || (x == 0x3412)
-- True
--
bufferReadWord16IO :: MonadIO m => Buffer mut pin fin heap -> Word -> m Word16
bufferReadWord16IO b (fromIntegral -> !(I# off)) = case b of
   BufferM (BA.MutableByteArray ba)          -> liftIO $ IO $ \s -> case readWord8ArrayAsWord16# ba off s of (# s2 , r #) -> (# s2 , W16# r #)
   BufferMP (BA.MutableByteArray ba)         -> liftIO $ IO $ \s -> case readWord8ArrayAsWord16# ba off s of (# s2 , r #) -> (# s2 , W16# r #)
   BufferMF  (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case readWord8ArrayAsWord16# ba off s of (# s2 , r #) -> (# s2 , W16# r #)
   BufferMPF (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case readWord8ArrayAsWord16# ba off s of (# s2 , r #) -> (# s2 , W16# r #)
   BufferME  addr _sz                        -> liftIO $ IO $ \s -> case readWord16OffAddr# (addr `plusAddr#` off) 0# s of (# s2 , r #) -> (# s2 , W16# r #)
   BufferMEF addr _sz _fin                   -> liftIO $ IO $ \s -> case readWord16OffAddr# (addr `plusAddr#` off) 0# s of (# s2 , r #) -> (# s2 , W16# r #)
   BufferE   addr _sz                        -> liftIO $ IO $ \s -> case readWord16OffAddr# (addr `plusAddr#` off) 0# s of (# s2 , r #) -> (# s2 , W16# r #)
   BufferEF  addr _sz _fin                   -> liftIO $ IO $ \s -> case readWord16OffAddr# (addr `plusAddr#` off) 0# s of (# s2 , r #) -> (# s2 , W16# r #)
   Buffer  (BA.ByteArray ba)                 -> return (W16# (indexWord8ArrayAsWord16# ba off))
   BufferP (BA.ByteArray ba)                 -> return (W16# (indexWord8ArrayAsWord16# ba off))
   BufferF   (BA.ByteArray ba) _fin          -> return (W16# (indexWord8ArrayAsWord16# ba off))
   BufferPF  (BA.ByteArray ba) _fin          -> return (W16# (indexWord8ArrayAsWord16# ba off))

-- | Read a Word16 in an immutable buffer, offset in bytes
--
-- We don't check that the offset is valid
bufferReadWord16 :: Buffer 'Immutable pin fin heap -> Word -> Word16
bufferReadWord16 b (fromIntegral -> !(I# off)) = case b of
   Buffer  (BA.ByteArray ba)                 -> W16# (indexWord8ArrayAsWord16# ba off)
   BufferP (BA.ByteArray ba)                 -> W16# (indexWord8ArrayAsWord16# ba off)
   BufferF   (BA.ByteArray ba) _fin          -> W16# (indexWord8ArrayAsWord16# ba off)
   BufferPF  (BA.ByteArray ba) _fin          -> W16# (indexWord8ArrayAsWord16# ba off)
   BufferE  addr _sz                         -> W16# (indexWord16OffAddr# (addr `plusAddr#` off) 0#)
   BufferEF addr _sz _fin                    -> W16# (indexWord16OffAddr# (addr `plusAddr#` off) 0#)

-- | Write a Word16, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> b <- newBuffer 10
-- >>> let v = 1234 :: Word16
-- >>> bufferWriteWord16IO b 1 v
-- >>> bufferReadWord16IO b 1
-- 1234
--
-- >>> (x :: Word16) <- fromIntegral <$> bufferReadWord8IO b 1
-- >>> (y :: Word16) <- fromIntegral <$> bufferReadWord8IO b 2
-- >>> (((x `shiftL` 8) .|. y) == v)   ||   (((y `shiftL` 8) .|. x) == v)
-- True
--
bufferWriteWord16IO :: MonadIO m => Buffer 'Mutable pin fin heap -> Word -> Word16 -> m ()
bufferWriteWord16IO b (fromIntegral -> !(I# off)) (W16# v) = case b of
   BufferM (BA.MutableByteArray ba)          -> liftIO $ IO $ \s -> case writeWord8ArrayAsWord16# ba off v s of s2 -> (# s2 , () #)
   BufferMP (BA.MutableByteArray ba)         -> liftIO $ IO $ \s -> case writeWord8ArrayAsWord16# ba off v s of s2 -> (# s2 , () #)
   BufferMF  (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case writeWord8ArrayAsWord16# ba off v s of s2 -> (# s2 , () #)
   BufferMPF (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case writeWord8ArrayAsWord16# ba off v s of s2 -> (# s2 , () #)
   BufferME  addr _sz                        -> liftIO $ IO $ \s -> case writeWord16OffAddr# (addr `plusAddr#` off) 0# v s of s2 -> (# s2 , () #)
   BufferMEF addr _sz _fin                   -> liftIO $ IO $ \s -> case writeWord16OffAddr# (addr `plusAddr#` off) 0# v s of s2 -> (# s2 , () #)



-- | Read a Word32, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> let b = [0x12,0x34,0x56,0x78] :: BufferI
-- >>> x <- bufferReadWord32IO b 0
-- >>> (x == 0x12345678) || (x == 0x78563412)
-- True
--
bufferReadWord32IO :: MonadIO m => Buffer mut pin fin heap -> Word -> m Word32
bufferReadWord32IO b (fromIntegral -> !(I# off)) = case b of
   BufferM    (BA.MutableByteArray ba)        -> liftIO $ IO $ \s -> case readWord8ArrayAsWord32# ba off s of (# s2 , r #) -> (# s2 , W32# r #)
   BufferMP   (BA.MutableByteArray ba)        -> liftIO $ IO $ \s -> case readWord8ArrayAsWord32# ba off s of (# s2 , r #) -> (# s2 , W32# r #)
   BufferMF   (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case readWord8ArrayAsWord32# ba off s of (# s2 , r #) -> (# s2 , W32# r #)
   BufferMPF  (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case readWord8ArrayAsWord32# ba off s of (# s2 , r #) -> (# s2 , W32# r #)
   BufferME   addr _sz                        -> liftIO $ IO $ \s -> case readWord32OffAddr# (addr `plusAddr#` off) 0# s of (# s2 , r #) -> (# s2 , W32# r #)
   BufferMEF  addr _sz _fin                   -> liftIO $ IO $ \s -> case readWord32OffAddr# (addr `plusAddr#` off) 0# s of (# s2 , r #) -> (# s2 , W32# r #)
   BufferE    addr _sz                        -> liftIO $ IO $ \s -> case readWord32OffAddr# (addr `plusAddr#` off) 0# s of (# s2 , r #) -> (# s2 , W32# r #)
   BufferEF   addr _sz _fin                   -> liftIO $ IO $ \s -> case readWord32OffAddr# (addr `plusAddr#` off) 0# s of (# s2 , r #) -> (# s2 , W32# r #)
   Buffer     (BA.ByteArray ba)               -> return (W32# (indexWord8ArrayAsWord32# ba off))
   BufferP    (BA.ByteArray ba)               -> return (W32# (indexWord8ArrayAsWord32# ba off))
   BufferF    (BA.ByteArray ba) _fin          -> return (W32# (indexWord8ArrayAsWord32# ba off))
   BufferPF   (BA.ByteArray ba) _fin          -> return (W32# (indexWord8ArrayAsWord32# ba off))

-- | Read a Word32 in an immutable buffer, offset in bytes
--
-- We don't check that the offset is valid
bufferReadWord32 :: Buffer 'Immutable pin fin heap -> Word -> Word32
bufferReadWord32 b (fromIntegral -> !(I# off)) = case b of
   Buffer  (BA.ByteArray ba)                 -> W32# (indexWord8ArrayAsWord32# ba off)
   BufferP (BA.ByteArray ba)                 -> W32# (indexWord8ArrayAsWord32# ba off)
   BufferF   (BA.ByteArray ba) _fin          -> W32# (indexWord8ArrayAsWord32# ba off)
   BufferPF  (BA.ByteArray ba) _fin          -> W32# (indexWord8ArrayAsWord32# ba off)
   BufferE  addr _sz                         -> W32# (indexWord32OffAddr# (addr `plusAddr#` off) 0#)
   BufferEF addr _sz _fin                    -> W32# (indexWord32OffAddr# (addr `plusAddr#` off) 0#)

-- | Write a Word32, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> b <- newBuffer 10
-- >>> let v = 1234 :: Word32
-- >>> bufferWriteWord32IO b 1 v
-- >>> bufferReadWord32IO b 1
-- 1234
--
bufferWriteWord32IO :: MonadIO m => Buffer 'Mutable pin fin heap -> Word -> Word32 -> m ()
bufferWriteWord32IO b (fromIntegral -> !(I# off)) (W32# v) = case b of
   BufferM (BA.MutableByteArray ba)          -> liftIO $ IO $ \s -> case writeWord8ArrayAsWord32# ba off v s of s2 -> (# s2 , () #)
   BufferMP (BA.MutableByteArray ba)         -> liftIO $ IO $ \s -> case writeWord8ArrayAsWord32# ba off v s of s2 -> (# s2 , () #)
   BufferMF  (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case writeWord8ArrayAsWord32# ba off v s of s2 -> (# s2 , () #)
   BufferMPF (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case writeWord8ArrayAsWord32# ba off v s of s2 -> (# s2 , () #)
   BufferME  addr _sz                        -> liftIO $ IO $ \s -> case writeWord32OffAddr# (addr `plusAddr#` off) 0# v s of s2 -> (# s2 , () #)
   BufferMEF addr _sz _fin                   -> liftIO $ IO $ \s -> case writeWord32OffAddr# (addr `plusAddr#` off) 0# v s of s2 -> (# s2 , () #)


-- | Read a Word64, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> let b = [0x12,0x34,0x56,0x78,0x9A,0xBC,0xDE,0xF0] :: BufferI
-- >>> x <- bufferReadWord64IO b 0
-- >>> (x == 0x123456789ABCDEF0) || (x == 0xF0DEBC9A78563412)
-- True
--
bufferReadWord64IO :: MonadIO m => Buffer mut pin fin heap -> Word -> m Word64
bufferReadWord64IO b (fromIntegral -> !(I# off)) = case b of
   BufferM (BA.MutableByteArray ba)          -> liftIO $ IO $ \s -> case readWord8ArrayAsWord64# ba off s of (# s2 , r #) -> (# s2 , W64# r #)
   BufferMP (BA.MutableByteArray ba)         -> liftIO $ IO $ \s -> case readWord8ArrayAsWord64# ba off s of (# s2 , r #) -> (# s2 , W64# r #)
   BufferMF  (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case readWord8ArrayAsWord64# ba off s of (# s2 , r #) -> (# s2 , W64# r #)
   BufferMPF (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case readWord8ArrayAsWord64# ba off s of (# s2 , r #) -> (# s2 , W64# r #)
   BufferME  addr _sz                        -> liftIO $ IO $ \s -> case readWord64OffAddr# (addr `plusAddr#` off) 0# s of (# s2 , r #) -> (# s2 , W64# r #)
   BufferMEF addr _sz _fin                   -> liftIO $ IO $ \s -> case readWord64OffAddr# (addr `plusAddr#` off) 0# s of (# s2 , r #) -> (# s2 , W64# r #)
   BufferE  addr _sz                         -> liftIO $ IO $ \s -> case readWord64OffAddr# (addr `plusAddr#` off) 0# s of (# s2 , r #) -> (# s2 , W64# r #)
   BufferEF addr _sz _fin                    -> liftIO $ IO $ \s -> case readWord64OffAddr# (addr `plusAddr#` off) 0# s of (# s2 , r #) -> (# s2 , W64# r #)
   Buffer  (BA.ByteArray ba)                 -> return (W64# (indexWord8ArrayAsWord64# ba off))
   BufferP (BA.ByteArray ba)                 -> return (W64# (indexWord8ArrayAsWord64# ba off))
   BufferF   (BA.ByteArray ba) _fin          -> return (W64# (indexWord8ArrayAsWord64# ba off))
   BufferPF  (BA.ByteArray ba) _fin          -> return (W64# (indexWord8ArrayAsWord64# ba off))

-- | Read a Word64 in an immutable buffer, offset in bytes
--
-- We don't check that the offset is valid
bufferReadWord64 :: Buffer 'Immutable pin fin heap -> Word -> Word64
bufferReadWord64 b (fromIntegral -> !(I# off)) = case b of
   Buffer    (BA.ByteArray ba)               -> W64# (indexWord8ArrayAsWord64# ba off)
   BufferP   (BA.ByteArray ba)               -> W64# (indexWord8ArrayAsWord64# ba off)
   BufferF   (BA.ByteArray ba) _fin          -> W64# (indexWord8ArrayAsWord64# ba off)
   BufferPF  (BA.ByteArray ba) _fin          -> W64# (indexWord8ArrayAsWord64# ba off)
   BufferE  addr _sz                         -> W64# (indexWord64OffAddr# (addr `plusAddr#` off) 0#)
   BufferEF addr _sz _fin                    -> W64# (indexWord64OffAddr# (addr `plusAddr#` off) 0#)

-- | Write a Word64, offset in bytes
--
-- We don't check that the offset is valid
--
-- >>> b <- newBuffer 10
-- >>> let v = 1234 :: Word64
-- >>> bufferWriteWord64IO b 1 v
-- >>> bufferReadWord64IO b 1
-- 1234
--
bufferWriteWord64IO :: MonadIO m => Buffer 'Mutable pin fin heap -> Word -> Word64 -> m ()
bufferWriteWord64IO b (fromIntegral -> !(I# off)) (W64# v) = case b of
   BufferM   (BA.MutableByteArray ba)        -> liftIO $ IO $ \s -> case writeWord8ArrayAsWord64# ba off v s of s2 -> (# s2 , () #)
   BufferMP  (BA.MutableByteArray ba)        -> liftIO $ IO $ \s -> case writeWord8ArrayAsWord64# ba off v s of s2 -> (# s2 , () #)
   BufferMF  (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case writeWord8ArrayAsWord64# ba off v s of s2 -> (# s2 , () #)
   BufferMPF (BA.MutableByteArray ba) _fin   -> liftIO $ IO $ \s -> case writeWord8ArrayAsWord64# ba off v s of s2 -> (# s2 , () #)
   BufferME  addr _sz                        -> liftIO $ IO $ \s -> case writeWord64OffAddr# (addr `plusAddr#` off) 0# v s of s2 -> (# s2 , () #)
   BufferMEF addr _sz _fin                   -> liftIO $ IO $ \s -> case writeWord64OffAddr# (addr `plusAddr#` off) 0# v s of s2 -> (# s2 , () #)


-- | Copy a buffer into another from/to the given offsets
--
-- We don't check buffer limits.
--
-- >>> let b = [0,1,2,3,4,5,6,7,8] :: BufferI
-- >>> b2 <- newBuffer 8
-- >>> copyBuffer b 4 b2 0 4
-- >>> copyBuffer b 0 b2 4 4
-- >>> forM [0..7] (bufferReadWord8IO b2)
-- [4,5,6,7,0,1,2,3]
--
copyBuffer :: forall m mut pin0 fin0 heap0 pin1 fin1 heap1.
   ( MonadIO m
   ) => Buffer mut pin0 fin0 heap0 -> Word -> Buffer 'Mutable pin1 fin1 heap1 -> Word -> Word -> m ()
copyBuffer sb (fromIntegral -> I# soff) db (fromIntegral -> I# doff) (fromIntegral -> I# cnt) = buf2buf
   where
      buf2buf = case db of
         BufferM   mba         -> toMba mba
         BufferMP  mba         -> toMba mba
         BufferMF  mba      _f -> toMba mba
         BufferMPF mba      _f -> toMba mba
         BufferME  addr _sz    -> toAddr addr
         BufferMEF addr _sz _f -> toAddr addr

      toMba :: BA.MutableByteArray RealWorld -> m ()
      toMba mba = case sb of
         Buffer    ba          -> baToMba ba mba
         BufferP   ba          -> baToMba ba mba
         BufferM   mba2        -> mbaToMba mba2 mba
         BufferMP  mba2        -> mbaToMba mba2 mba
         BufferME  addr _sz    -> addrToMba addr mba
         BufferE   addr _sz    -> addrToMba addr mba
         BufferF   ba       _f -> baToMba ba mba
         BufferPF  ba       _f -> baToMba ba mba
         BufferMF  mba2     _f -> mbaToMba mba2 mba
         BufferMPF mba2     _f -> mbaToMba mba2 mba
         BufferMEF addr _sz _f -> addrToMba addr mba
         BufferEF  addr _sz _f -> addrToMba addr mba

      toAddr :: Addr# -> m ()
      toAddr addr = case sb of
         Buffer    ba           -> baToAddr ba addr
         BufferP   ba           -> baToAddr ba addr
         BufferM   mba          -> mbaToAddr mba addr
         BufferMP  mba          -> mbaToAddr mba addr
         BufferME  addr2 _sz    -> addrToAddr addr2 addr
         BufferE   addr2 _sz    -> addrToAddr addr2 addr
         BufferF   ba        _f -> baToAddr ba addr
         BufferPF  ba        _f -> baToAddr ba addr
         BufferMF  mba       _f -> mbaToAddr mba addr
         BufferMPF mba       _f -> mbaToAddr mba addr
         BufferMEF addr2 _sz _f -> addrToAddr addr2 addr
         BufferEF  addr2 _sz _f -> addrToAddr addr2 addr

      mbaToMba :: BA.MutableByteArray RealWorld -> BA.MutableByteArray RealWorld -> m ()
      mbaToMba   (BA.MutableByteArray mba1) (BA.MutableByteArray mba2) =
         liftIO $ IO $ \s ->
            case copyMutableByteArray# mba1 soff mba2 doff cnt s of
               s2 -> (# s2, () #)

      baToMba :: BA.ByteArray -> BA.MutableByteArray RealWorld -> m ()
      baToMba (BA.ByteArray ba) (BA.MutableByteArray mba) =
         liftIO $ IO $ \s ->
            case copyByteArray# ba soff mba doff cnt s of
               s2 -> (# s2, () #)

      addrToMba :: Addr# -> BA.MutableByteArray RealWorld -> m ()
      addrToMba addr (BA.MutableByteArray mba) =
         liftIO $ IO $ \s ->
            case copyAddrToByteArray# (addr `plusAddr#` soff) mba doff cnt s of
               s2 -> (# s2, () #)

      baToAddr :: BA.ByteArray -> Addr# -> m ()
      baToAddr (BA.ByteArray ba) addr =
         liftIO $ IO $ \s ->
            case copyByteArrayToAddr# ba soff (addr `plusAddr#` doff) cnt s of
               s2 -> (# s2, () #)


      mbaToAddr :: BA.MutableByteArray RealWorld -> Addr# -> m ()
      mbaToAddr (BA.MutableByteArray mba) addr =
         liftIO $ IO $ \s ->
            case copyMutableByteArrayToAddr# mba soff (addr `plusAddr#` doff) cnt s of
               s2 -> (# s2, () #)

      addrToAddr :: Addr# -> Addr# -> m ()
      addrToAddr addr1 addr2 =
         liftIO $ memcpy (addr1 `plusAddr#` soff)
                         (addr2 `plusAddr#` doff)
                         cnt
        
foreign import ccall unsafe "string.h" memcpy  :: Addr# -> Addr# -> Int# -> IO ()

