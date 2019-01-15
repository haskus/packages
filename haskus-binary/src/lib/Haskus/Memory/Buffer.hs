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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | A buffer in memory
module Haskus.Memory.Buffer
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
   -- * GHC allocator
   , newBuffer
   , newPinnedBuffer
   , newAlignedPinnedBuffer
   -- * Operations
   , Freezable (..)
   , Thawable (..)
   , bufferIsDynamicallyPinned
   , bufferDynamicallyPinned
   , withBufferAddr#
   , withBufferPtr
   , unsafeWithBufferAddr#
   , unsafeWithBufferPtr
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
   Buffer    :: {-# UNPACK #-} !BA.ByteArray                                                  -> BufferI
   BufferP   :: {-# UNPACK #-} !BA.ByteArray                                                  -> BufferP
   BufferM   :: {-# UNPACK #-} !(BA.MutableByteArray RealWorld)                               -> BufferM
   BufferMP  :: {-# UNPACK #-} !(BA.MutableByteArray RealWorld)                               -> BufferMP
   BufferME  :: Addr# -> {-# UNPACK #-} !Word                                                 -> BufferME
   BufferE   :: Addr# -> {-# UNPACK #-} !Word                                                 -> BufferE
   BufferF   :: {-# UNPACK #-} !BA.ByteArray                    -> {-# UNPACK #-} !Finalizers -> BufferF
   BufferPF  :: {-# UNPACK #-} !BA.ByteArray                    -> {-# UNPACK #-} !Finalizers -> BufferPF
   BufferMF  :: {-# UNPACK #-} !(BA.MutableByteArray RealWorld) -> {-# UNPACK #-} !Finalizers -> BufferMF
   BufferMPF :: {-# UNPACK #-} !(BA.MutableByteArray RealWorld) -> {-# UNPACK #-} !Finalizers -> BufferMPF
   BufferMEF :: Addr# -> {-# UNPACK #-} !Word                   -> {-# UNPACK #-} !Finalizers -> BufferMEF
   BufferEF  :: Addr# -> {-# UNPACK #-} !Word                   -> {-# UNPACK #-} !Finalizers -> BufferEF

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
{-# INLINE newBuffer #-}
newBuffer sz = BufferM <$> liftIO (BA.newByteArray (fromIntegral sz))

-- | Allocate a buffer (mutable, pinned)
newPinnedBuffer :: MonadIO m => Word -> m BufferMP
{-# INLINE newPinnedBuffer #-}
newPinnedBuffer sz = BufferMP <$> liftIO (BA.newPinnedByteArray (fromIntegral sz))

-- | Allocate an aligned buffer (mutable, pinned)
newAlignedPinnedBuffer :: MonadIO m => Word -> Word -> m BufferMP
{-# INLINE newAlignedPinnedBuffer #-}
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
{-# INLINABLE touchBuffer #-}
{-# SPECIALIZE INLINE touchBuffer :: MonadIO m => BufferI  -> m () #-}
{-# SPECIALIZE INLINE touchBuffer :: MonadIO m => BufferP  -> m () #-}
{-# SPECIALIZE INLINE touchBuffer :: MonadIO m => BufferM  -> m () #-}
{-# SPECIALIZE INLINE touchBuffer :: MonadIO m => BufferMP -> m () #-}
{-# SPECIALIZE INLINE touchBuffer :: MonadIO m => BufferME -> m () #-}
{-# SPECIALIZE INLINE touchBuffer :: MonadIO m => BufferE  -> m () #-}
{-# SPECIALIZE INLINE touchBuffer :: MonadIO m => BufferF  -> m () #-}
{-# SPECIALIZE INLINE touchBuffer :: MonadIO m => BufferPF -> m () #-}
{-# SPECIALIZE INLINE touchBuffer :: MonadIO m => BufferMF -> m () #-}
{-# SPECIALIZE INLINE touchBuffer :: MonadIO m => BufferMPF-> m () #-}
{-# SPECIALIZE INLINE touchBuffer :: MonadIO m => BufferMEF-> m () #-}
{-# SPECIALIZE INLINE touchBuffer :: MonadIO m => BufferEF -> m () #-}
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
{-# INLINABLE makeFinalizable #-}
{-# SPECIALIZE INLINE makeFinalizable :: MonadIO m => BufferI  -> m BufferF #-}
{-# SPECIALIZE INLINE makeFinalizable :: MonadIO m => BufferP  -> m BufferPF #-}
{-# SPECIALIZE INLINE makeFinalizable :: MonadIO m => BufferM  -> m BufferMF #-}
{-# SPECIALIZE INLINE makeFinalizable :: MonadIO m => BufferMP -> m BufferMPF #-}
{-# SPECIALIZE INLINE makeFinalizable :: MonadIO m => BufferME -> m BufferMEF #-}
{-# SPECIALIZE INLINE makeFinalizable :: MonadIO m => BufferE  -> m BufferEF #-}
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

-----------------------------------------------------------------
-- Operations
-----------------------------------------------------------------

-- | Buffer that can be frozen (converted from mutable to immutable)
class Freezable a b | a -> b where
   -- | Convert a mutable buffer to an immutable one without copying. The
   -- buffer should not be modified after the conversion.
   unsafeBufferFreeze :: MonadIO m => a -> m b

instance Freezable (Buffer 'Mutable   pin 'Collected heap)
                   (Buffer 'Immutable pin 'Collected heap)
   where
      {-# INLINE unsafeBufferFreeze #-}
      unsafeBufferFreeze = \case
         BufferM mba  -> Buffer  <$> liftIO (BA.unsafeFreezeByteArray mba)
         BufferMP mba -> BufferP <$> liftIO (BA.unsafeFreezeByteArray mba)

instance Freezable (Buffer 'Mutable   pin fin 'External)
                   (Buffer 'Immutable pin fin 'External)
   where
      {-# INLINE unsafeBufferFreeze #-}
      unsafeBufferFreeze = \case
         BufferME  addr sz     -> return (BufferE addr sz)
         -- works because finalizers are attached to the IORef "fin"
         BufferMEF addr sz fin -> return (BufferEF addr sz fin)


-- | Buffer that can be thawed (converted from immutable to mutable)
class Thawable a b | a -> b where
   -- | Convert an immutable buffer to a mutable one without copying. The
   -- original buffer should not be used after the conversion.
   unsafeBufferThaw :: MonadIO m => a -> m b

instance Thawable (Buffer 'Immutable pin 'Collected heap)
                  (Buffer 'Mutable   pin 'Collected heap)
   where
      {-# INLINE unsafeBufferThaw #-}
      unsafeBufferThaw = \case
         Buffer mba  -> BufferM  <$> liftIO (BA.unsafeThawByteArray mba)
         BufferP mba -> BufferMP <$> liftIO (BA.unsafeThawByteArray mba)

instance Thawable (Buffer 'Immutable pin 'NotFinalized heap)
                  (Buffer 'Mutable   pin 'NotFinalized heap)
   where
      {-# INLINE unsafeBufferThaw #-}
      unsafeBufferThaw = \case
         BufferE addr sz -> return (BufferME addr sz)



-- | Some buffers managed by GHC can be pinned as an optimization. This function
-- reports this.
bufferIsDynamicallyPinned :: Buffer mut pin fin heap -> Bool
bufferIsDynamicallyPinned = \case
   BufferP  {}       -> True
   BufferMP {}       -> True
   BufferME {}       -> True
   BufferPF {}       -> True
   BufferE  {}       -> True
   BufferMEF{}       -> True
   BufferEF {}       -> True
   BufferMPF{}       -> True
   Buffer   ba       -> BA.isByteArrayPinned ba
   BufferM  mba      -> BA.isMutableByteArrayPinned mba
   BufferF  ba  _fin -> BA.isByteArrayPinned ba
   BufferMF mba _fin -> BA.isMutableByteArrayPinned mba

-- | Transform type-level NotPinned buffers into type-level Pinned if the buffer
-- is dynamically pinned (see `bufferIsDynamicallyPinned`).
bufferDynamicallyPinned
   :: Buffer mut pin fin heap
   -> Either (Buffer mut 'NotPinned fin heap) (Buffer mut 'Pinned fin heap)
bufferDynamicallyPinned b = case b of
   BufferP  {}      -> Right b
   BufferMP {}      -> Right b
   BufferME {}      -> Right b
   BufferPF {}      -> Right b
   BufferE  {}      -> Right b
   BufferMEF{}      -> Right b
   BufferEF {}      -> Right b
   BufferMPF{}      -> Right b
   Buffer   ba      -> if BA.isByteArrayPinned ba
                        then Right (BufferP ba)
                        else Left b
   BufferM  mba     -> if BA.isMutableByteArrayPinned mba
                        then Right (BufferMP mba)
                        else Left b
   BufferF  ba  fin -> if BA.isByteArrayPinned ba
                        then Right (BufferPF ba fin)
                        else Left b
   BufferMF mba fin -> if BA.isMutableByteArrayPinned mba
                        then Right (BufferMPF mba fin)
                        else Left b



-- | Do something with a buffer address
--
-- Note: don't write into immutable buffer as it would break referential
-- consistency
unsafeWithBufferAddr# :: MonadIO m => Buffer mut 'Pinned fin heap -> (Addr# -> m a) -> m a
{-# INLINABLE unsafeWithBufferAddr# #-}
{-# SPECIALIZE INLINE unsafeWithBufferAddr# :: MonadIO m => BufferP  -> (Addr# -> m a) -> m a #-}
{-# SPECIALIZE INLINE unsafeWithBufferAddr# :: MonadIO m => BufferMP -> (Addr# -> m a) -> m a #-}
{-# SPECIALIZE INLINE unsafeWithBufferAddr# :: MonadIO m => BufferME -> (Addr# -> m a) -> m a #-}
{-# SPECIALIZE INLINE unsafeWithBufferAddr# :: MonadIO m => BufferE  -> (Addr# -> m a) -> m a #-}
{-# SPECIALIZE INLINE unsafeWithBufferAddr# :: MonadIO m => BufferPF -> (Addr# -> m a) -> m a #-}
{-# SPECIALIZE INLINE unsafeWithBufferAddr# :: MonadIO m => BufferMPF-> (Addr# -> m a) -> m a #-}
{-# SPECIALIZE INLINE unsafeWithBufferAddr# :: MonadIO m => BufferMEF-> (Addr# -> m a) -> m a #-}
{-# SPECIALIZE INLINE unsafeWithBufferAddr# :: MonadIO m => BufferEF -> (Addr# -> m a) -> m a #-}
unsafeWithBufferAddr# b@(BufferP ba) f = do
   let !(BA.Addr addr) = BA.byteArrayContents ba
   r <- f addr
   touchBuffer b
   return r
unsafeWithBufferAddr# b@(BufferMP ba) f = do
   let !(BA.Addr addr) = BA.mutableByteArrayContents ba
   r <- f addr
   touchBuffer b
   return r
unsafeWithBufferAddr# b@(BufferPF ba _fin) f = do
   let !(BA.Addr addr) = BA.byteArrayContents ba
   r <- f addr
   touchBuffer b
   return r
unsafeWithBufferAddr# b@(BufferMPF ba _fin) f = do
   let !(BA.Addr addr) = BA.mutableByteArrayContents ba
   r <- f addr
   touchBuffer b
   return r
unsafeWithBufferAddr# (BufferME addr _sz)         f = f (addr)
unsafeWithBufferAddr# (BufferE  addr _sz)         f = f (addr)
unsafeWithBufferAddr# b@(BufferMEF addr _sz _fin) f = do
   r <- f addr
   touchBuffer b
   return r
unsafeWithBufferAddr# b@(BufferEF addr _sz _fin)  f = do
   r <- f addr
   touchBuffer b
   return r

-- | Do something with a buffer pointer
--
-- Note: don't write into immutable buffer as it would break referential
-- consistency
unsafeWithBufferPtr :: MonadIO m => Buffer mut 'Pinned fin heap -> (Ptr b -> m a) -> m a
{-# INLINABLE unsafeWithBufferPtr #-}
{-# SPECIALIZE INLINE unsafeWithBufferPtr :: MonadIO m => BufferP  -> (Ptr b -> m a) -> m a #-}
{-# SPECIALIZE INLINE unsafeWithBufferPtr :: MonadIO m => BufferMP -> (Ptr b -> m a) -> m a #-}
{-# SPECIALIZE INLINE unsafeWithBufferPtr :: MonadIO m => BufferME -> (Ptr b -> m a) -> m a #-}
{-# SPECIALIZE INLINE unsafeWithBufferPtr :: MonadIO m => BufferE  -> (Ptr b -> m a) -> m a #-}
{-# SPECIALIZE INLINE unsafeWithBufferPtr :: MonadIO m => BufferPF -> (Ptr b -> m a) -> m a #-}
{-# SPECIALIZE INLINE unsafeWithBufferPtr :: MonadIO m => BufferMPF-> (Ptr b -> m a) -> m a #-}
{-# SPECIALIZE INLINE unsafeWithBufferPtr :: MonadIO m => BufferMEF-> (Ptr b -> m a) -> m a #-}
{-# SPECIALIZE INLINE unsafeWithBufferPtr :: MonadIO m => BufferEF -> (Ptr b -> m a) -> m a #-}
unsafeWithBufferPtr b f = unsafeWithBufferAddr# b g
   where
      g addr = f (Ptr addr)

-- | Do something with a buffer address
withBufferAddr# :: MonadIO m => Buffer 'Mutable 'Pinned fin heap -> (Addr# -> m a) -> m a
{-# INLINABLE withBufferAddr# #-}
{-# SPECIALIZE INLINE withBufferAddr# :: MonadIO m => BufferMP -> (Addr# -> m a) -> m a #-}
{-# SPECIALIZE INLINE withBufferAddr# :: MonadIO m => BufferME -> (Addr# -> m a) -> m a #-}
{-# SPECIALIZE INLINE withBufferAddr# :: MonadIO m => BufferMPF-> (Addr# -> m a) -> m a #-}
{-# SPECIALIZE INLINE withBufferAddr# :: MonadIO m => BufferMEF-> (Addr# -> m a) -> m a #-}
withBufferAddr# = unsafeWithBufferAddr#

-- | Do something with a buffer pointer
withBufferPtr :: MonadIO m => Buffer 'Mutable 'Pinned fin heap -> (Ptr b -> m a) -> m a
{-# INLINABLE withBufferPtr #-}
{-# SPECIALIZE INLINE withBufferPtr :: MonadIO m => BufferMP -> (Ptr b -> m a) -> m a #-}
{-# SPECIALIZE INLINE withBufferPtr :: MonadIO m => BufferME -> (Ptr b -> m a) -> m a #-}
{-# SPECIALIZE INLINE withBufferPtr :: MonadIO m => BufferMPF-> (Ptr b -> m a) -> m a #-}
{-# SPECIALIZE INLINE withBufferPtr :: MonadIO m => BufferMEF-> (Ptr b -> m a) -> m a #-}
withBufferPtr = unsafeWithBufferPtr

-- | Get buffer size
bufferSizeIO :: MonadIO m => Buffer mut pin fin heap -> m Word
{-# INLINABLE bufferSizeIO #-}
{-# SPECIALIZE INLINE bufferSizeIO :: MonadIO m => BufferI  -> m Word #-}
{-# SPECIALIZE INLINE bufferSizeIO :: MonadIO m => BufferP  -> m Word #-}
{-# SPECIALIZE INLINE bufferSizeIO :: MonadIO m => BufferM  -> m Word #-}
{-# SPECIALIZE INLINE bufferSizeIO :: MonadIO m => BufferMP -> m Word #-}
{-# SPECIALIZE INLINE bufferSizeIO :: MonadIO m => BufferME -> m Word #-}
{-# SPECIALIZE INLINE bufferSizeIO :: MonadIO m => BufferE  -> m Word #-}
{-# SPECIALIZE INLINE bufferSizeIO :: MonadIO m => BufferF  -> m Word #-}
{-# SPECIALIZE INLINE bufferSizeIO :: MonadIO m => BufferPF -> m Word #-}
{-# SPECIALIZE INLINE bufferSizeIO :: MonadIO m => BufferMF -> m Word #-}
{-# SPECIALIZE INLINE bufferSizeIO :: MonadIO m => BufferMPF-> m Word #-}
{-# SPECIALIZE INLINE bufferSizeIO :: MonadIO m => BufferMEF-> m Word #-}
{-# SPECIALIZE INLINE bufferSizeIO :: MonadIO m => BufferEF -> m Word #-}
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
   {-# INLINE bufferSize #-}
   bufferSize (Buffer ba)  = fromIntegral $ BA.sizeofByteArray ba
instance BufferSize BufferP where
   {-# INLINE bufferSize #-}
   bufferSize (BufferP ba) = fromIntegral $ BA.sizeofByteArray ba
instance BufferSize BufferF where
   {-# INLINE bufferSize #-}
   bufferSize (BufferF ba _fin)  = fromIntegral $ BA.sizeofByteArray ba
instance BufferSize BufferPF where
   {-# INLINE bufferSize #-}
   bufferSize (BufferPF ba _fin) = fromIntegral $ BA.sizeofByteArray ba
instance BufferSize BufferME where
   {-# INLINE bufferSize #-}
   bufferSize (BufferME _addr sz) = sz
instance BufferSize BufferMEF where
   {-# INLINE bufferSize #-}
   bufferSize (BufferMEF _addr sz _fin) = sz
instance BufferSize BufferE where
   {-# INLINE bufferSize #-}
   bufferSize (BufferE _addr sz) = sz
instance BufferSize BufferEF where
   {-# INLINE bufferSize #-}
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
{-# INLINABLE bufferReadWord8IO #-}
{-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferI  -> Word -> m Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferP  -> Word -> m Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferM  -> Word -> m Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferMP -> Word -> m Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferME -> Word -> m Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferE  -> Word -> m Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferF  -> Word -> m Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferPF -> Word -> m Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferMF -> Word -> m Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferMPF-> Word -> m Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferMEF-> Word -> m Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferEF -> Word -> m Word8 #-}
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
{-# INLINABLE bufferReadWord8 #-}
{-# SPECIALIZE INLINE bufferReadWord8 :: BufferI  -> Word -> Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8 :: BufferP  -> Word -> Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8 :: BufferE  -> Word -> Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8 :: BufferF  -> Word -> Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8 :: BufferPF -> Word -> Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8 :: BufferEF -> Word -> Word8 #-}
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
{-# INLINABLE bufferWriteWord8IO #-}
{-# SPECIALIZE INLINE bufferWriteWord8IO :: MonadIO m => BufferM  -> Word -> Word8 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord8IO :: MonadIO m => BufferMP -> Word -> Word8 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord8IO :: MonadIO m => BufferME -> Word -> Word8 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord8IO :: MonadIO m => BufferMF -> Word -> Word8 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord8IO :: MonadIO m => BufferMPF-> Word -> Word8 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord8IO :: MonadIO m => BufferMEF-> Word -> Word8 -> m ()#-}
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
{-# INLINABLE bufferReadWord16IO #-}
{-# SPECIALIZE INLINE bufferReadWord16IO :: MonadIO m => BufferI  -> Word -> m Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16IO :: MonadIO m => BufferP  -> Word -> m Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16IO :: MonadIO m => BufferM  -> Word -> m Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16IO :: MonadIO m => BufferMP -> Word -> m Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16IO :: MonadIO m => BufferME -> Word -> m Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16IO :: MonadIO m => BufferE  -> Word -> m Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16IO :: MonadIO m => BufferF  -> Word -> m Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16IO :: MonadIO m => BufferPF -> Word -> m Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16IO :: MonadIO m => BufferMF -> Word -> m Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16IO :: MonadIO m => BufferMPF-> Word -> m Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16IO :: MonadIO m => BufferMEF-> Word -> m Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16IO :: MonadIO m => BufferEF -> Word -> m Word16 #-}
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
{-# INLINABLE bufferReadWord16 #-}
{-# SPECIALIZE INLINE bufferReadWord16 :: BufferI  -> Word -> Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16 :: BufferP  -> Word -> Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16 :: BufferE  -> Word -> Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16 :: BufferF  -> Word -> Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16 :: BufferPF -> Word -> Word16 #-}
{-# SPECIALIZE INLINE bufferReadWord16 :: BufferEF -> Word -> Word16 #-}
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
{-# INLINABLE bufferWriteWord16IO #-}
{-# SPECIALIZE INLINE bufferWriteWord16IO :: MonadIO m => BufferM  -> Word -> Word16 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord16IO :: MonadIO m => BufferMP -> Word -> Word16 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord16IO :: MonadIO m => BufferME -> Word -> Word16 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord16IO :: MonadIO m => BufferMF -> Word -> Word16 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord16IO :: MonadIO m => BufferMPF-> Word -> Word16 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord16IO :: MonadIO m => BufferMEF-> Word -> Word16 -> m ()#-}
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
{-# INLINABLE bufferReadWord32IO #-}
{-# SPECIALIZE INLINE bufferReadWord32IO :: MonadIO m => BufferI  -> Word -> m Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32IO :: MonadIO m => BufferP  -> Word -> m Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32IO :: MonadIO m => BufferM  -> Word -> m Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32IO :: MonadIO m => BufferMP -> Word -> m Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32IO :: MonadIO m => BufferME -> Word -> m Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32IO :: MonadIO m => BufferE  -> Word -> m Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32IO :: MonadIO m => BufferF  -> Word -> m Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32IO :: MonadIO m => BufferPF -> Word -> m Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32IO :: MonadIO m => BufferMF -> Word -> m Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32IO :: MonadIO m => BufferMPF-> Word -> m Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32IO :: MonadIO m => BufferMEF-> Word -> m Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32IO :: MonadIO m => BufferEF -> Word -> m Word32 #-}
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
{-# INLINABLE bufferReadWord32 #-}
{-# SPECIALIZE INLINE bufferReadWord32 :: BufferI  -> Word -> Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32 :: BufferP  -> Word -> Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32 :: BufferE  -> Word -> Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32 :: BufferF  -> Word -> Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32 :: BufferPF -> Word -> Word32 #-}
{-# SPECIALIZE INLINE bufferReadWord32 :: BufferEF -> Word -> Word32 #-}
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
{-# INLINABLE bufferWriteWord32IO #-}
{-# SPECIALIZE INLINE bufferWriteWord32IO :: MonadIO m => BufferM  -> Word -> Word32 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord32IO :: MonadIO m => BufferMP -> Word -> Word32 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord32IO :: MonadIO m => BufferME -> Word -> Word32 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord32IO :: MonadIO m => BufferMF -> Word -> Word32 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord32IO :: MonadIO m => BufferMPF-> Word -> Word32 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord32IO :: MonadIO m => BufferMEF-> Word -> Word32 -> m ()#-}
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
{-# INLINABLE bufferReadWord64IO #-}
{-# SPECIALIZE INLINE bufferReadWord64IO :: MonadIO m => BufferI  -> Word -> m Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64IO :: MonadIO m => BufferP  -> Word -> m Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64IO :: MonadIO m => BufferM  -> Word -> m Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64IO :: MonadIO m => BufferMP -> Word -> m Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64IO :: MonadIO m => BufferME -> Word -> m Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64IO :: MonadIO m => BufferE  -> Word -> m Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64IO :: MonadIO m => BufferF  -> Word -> m Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64IO :: MonadIO m => BufferPF -> Word -> m Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64IO :: MonadIO m => BufferMF -> Word -> m Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64IO :: MonadIO m => BufferMPF-> Word -> m Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64IO :: MonadIO m => BufferMEF-> Word -> m Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64IO :: MonadIO m => BufferEF -> Word -> m Word64 #-}
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
{-# INLINABLE bufferReadWord64 #-}
{-# SPECIALIZE INLINE bufferReadWord64 :: BufferI  -> Word -> Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64 :: BufferP  -> Word -> Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64 :: BufferE  -> Word -> Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64 :: BufferF  -> Word -> Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64 :: BufferPF -> Word -> Word64 #-}
{-# SPECIALIZE INLINE bufferReadWord64 :: BufferEF -> Word -> Word64 #-}
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
{-# INLINABLE bufferWriteWord64IO #-}
{-# SPECIALIZE INLINE bufferWriteWord64IO :: MonadIO m => BufferM  -> Word -> Word64 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord64IO :: MonadIO m => BufferMP -> Word -> Word64 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord64IO :: MonadIO m => BufferME -> Word -> Word64 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord64IO :: MonadIO m => BufferMF -> Word -> Word64 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord64IO :: MonadIO m => BufferMPF-> Word -> Word64 -> m ()#-}
{-# SPECIALIZE INLINE bufferWriteWord64IO :: MonadIO m => BufferMEF-> Word -> Word64 -> m ()#-}
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
   MonadIO m
   => Buffer mut pin0 fin0 heap0        -- ^ Source buffer
   -> Word                              -- ^ Offset in source buffer
   -> Buffer 'Mutable pin1 fin1 heap1   -- ^ Target buffer
   -> Word                              -- ^ Offset in target buffer
   -> Word                              -- ^ Number of Word8 to copy
   -> m ()
{-# INLINABLE copyBuffer #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferI   -> Word -> BufferM   -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferI   -> Word -> BufferMP  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferI   -> Word -> BufferME  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferI   -> Word -> BufferMF  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferI   -> Word -> BufferMPF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferI   -> Word -> BufferMEF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferP   -> Word -> BufferM   -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferP   -> Word -> BufferMP  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferP   -> Word -> BufferME  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferP   -> Word -> BufferMF  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferP   -> Word -> BufferMPF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferP   -> Word -> BufferMEF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferM   -> Word -> BufferM   -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferM   -> Word -> BufferMP  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferM   -> Word -> BufferME  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferM   -> Word -> BufferMF  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferM   -> Word -> BufferMPF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferM   -> Word -> BufferMEF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMP  -> Word -> BufferM   -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMP  -> Word -> BufferMP  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMP  -> Word -> BufferME  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMP  -> Word -> BufferMF  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMP  -> Word -> BufferMPF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMP  -> Word -> BufferMEF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferME  -> Word -> BufferM   -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferME  -> Word -> BufferMP  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferME  -> Word -> BufferME  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferME  -> Word -> BufferMF  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferME  -> Word -> BufferMPF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferME  -> Word -> BufferMEF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferE   -> Word -> BufferM   -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferE   -> Word -> BufferMP  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferE   -> Word -> BufferME  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferE   -> Word -> BufferMF  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferE   -> Word -> BufferMPF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferE   -> Word -> BufferMEF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferF   -> Word -> BufferM   -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferF   -> Word -> BufferMP  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferF   -> Word -> BufferME  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferF   -> Word -> BufferMF  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferF   -> Word -> BufferMPF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferF   -> Word -> BufferMEF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferPF  -> Word -> BufferM   -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferPF  -> Word -> BufferMP  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferPF  -> Word -> BufferME  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferPF  -> Word -> BufferMF  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferPF  -> Word -> BufferMPF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferPF  -> Word -> BufferMEF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMF  -> Word -> BufferM   -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMF  -> Word -> BufferMP  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMF  -> Word -> BufferME  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMF  -> Word -> BufferMF  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMF  -> Word -> BufferMPF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMF  -> Word -> BufferMEF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMPF -> Word -> BufferM   -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMPF -> Word -> BufferMP  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMPF -> Word -> BufferME  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMPF -> Word -> BufferMF  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMPF -> Word -> BufferMPF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMPF -> Word -> BufferMEF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMEF -> Word -> BufferM   -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMEF -> Word -> BufferMP  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMEF -> Word -> BufferME  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMEF -> Word -> BufferMF  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMEF -> Word -> BufferMPF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferMEF -> Word -> BufferMEF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferEF  -> Word -> BufferM   -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferEF  -> Word -> BufferMP  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferEF  -> Word -> BufferME  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferEF  -> Word -> BufferMF  -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferEF  -> Word -> BufferMPF -> Word -> Word -> m () #-}
{-# SPECIALIZE INLINE copyBuffer :: MonadIO m => BufferEF  -> Word -> BufferMEF -> Word -> Word -> m () #-}
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

