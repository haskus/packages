{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}

-- | Malloc memory allocator
module Haskus.Memory.Allocator.Malloc
   ( newBuffer
   , newFinalizedBuffer
   , makeFinalized
   , freeBuffer
   )
where

import GHC.Exts
import Foreign.Ptr (nullPtr)
import Haskus.Utils.Monad
import Haskus.Memory.Buffer
   ( Buffer(..), BufferME, BufferMEF
   , makeFinalizable,addFinalizer
   )

foreign import ccall unsafe "malloc"  malloc_ :: Word -> IO (Ptr ())
foreign import ccall unsafe "free"    free    :: Addr# -> IO ()

-- | Allocate a new Buffer using system ``malloc``
newBuffer :: MonadIO m => Word -> m (Maybe BufferME)
{-# INLINABLE newBuffer #-}
newBuffer sz = do
   p <- liftIO (malloc_ sz)
   case p == nullPtr of
      True  -> return Nothing
      False -> case p of
         Ptr addr -> return (Just (BufferME addr sz))

-- | Allocate a new finalized buffer using system ``malloc`` and finalized with
-- ``free``.
newFinalizedBuffer :: MonadIO m => Word -> m (Maybe BufferMEF)
{-# INLINABLE newFinalizedBuffer #-}
newFinalizedBuffer sz = do
   mb  <- newBuffer sz
   forM mb makeFinalized
   
-- | Make a buffer finalized with ``free``
makeFinalized :: MonadIO m => BufferME -> m BufferMEF
{-# INLINABLE makeFinalized #-}
makeFinalized b = do
   fb <- makeFinalizable b
   case fb of
      BufferMEF addr _sz _f -> addFinalizer fb (free addr)
   return fb


-- | Free a malloc-ed Buffer
freeBuffer :: MonadIO m => BufferME -> m ()
{-# INLINABLE freeBuffer #-}
freeBuffer (BufferME addr _sz) = liftIO (free addr)
