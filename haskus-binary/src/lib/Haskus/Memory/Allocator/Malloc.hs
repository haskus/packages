{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | Malloc memory allocator
module Haskus.Memory.Allocator.Malloc
   ( newBuffer
   , newFinalizedBuffer
   , freeBuffer
   )
where

import GHC.Exts
import Foreign.Ptr (nullPtr)
import Haskus.Utils.Monad
import qualified Haskus.Memory.Buffer as B
import Haskus.Memory.Buffer (Buffer)

foreign import ccall unsafe "malloc"  malloc_ :: Word -> IO (Ptr ())
foreign import ccall unsafe "free"    free    :: Addr# -> IO ()

-- | Allocate a new Buffer using system ``malloc``
newBuffer :: Word -> IO (Maybe Buffer)
newBuffer sz@(W# sz#) = do
   p <- malloc_ sz
   case p == nullPtr of
      True  -> return Nothing
      False -> case p of
         Ptr addr -> pure (Just (B.attachExternalBuffer addr sz#))

-- | Allocate a new finalized buffer using system ``malloc`` and finalized with
-- ``free``.
newFinalizedBuffer :: Word -> IO (Maybe Buffer)
newFinalizedBuffer sz@(W# sz#) = do
   p <- malloc_ sz
   case p == nullPtr of
      True  -> return Nothing
      False -> case p of
         Ptr addr -> do
          b  <- B.attachFinalizedBuffer addr sz#
          B.addFinalizer b (free addr)
          pure (Just b)
   
-- | Free a malloc-ed Buffer
freeBuffer :: Buffer -> IO ()
{-# INLINABLE freeBuffer #-}
freeBuffer = \case
  B.InBuffer {}        -> error "freeBuffer: unexpected managed buffer"
  B.OutBuffer addr _ _ -> liftIO (free addr)
