{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- | A view (e.g. a slice) of a buffer
module Haskus.Memory.View
   ( View (..)
   , ViewSource (..)
   , ViewPattern (..)
   , viewReadWord8
   , newBufferView
   , newBufferWeakView
   , withValidView
   )
where

import Data.IORef
import System.Mem.Weak
import Control.Concurrent

import Haskus.Utils.Monad
import Haskus.Format.Binary.Word
import Haskus.Memory.Buffer

-- | The source of a view
--
-- Weak views are used so that the underlying buffer can be freed by the GC.
-- When it happens and if the view is still alive the contents of the buffer
-- used by the view is copied into a fresh (smaller) buffer.
--
-- Weak views can also be used as sources: in this case, when the source
-- view is GCed, the current view is updated to point to the source of the
-- source.
--
data ViewSource
   = forall mut pin fin heap. SourceBuffer (Buffer mut pin fin heap)
      -- ^ The source is a buffer. The view keeps the buffer alive
   | forall mut pin fin heap. SourceWeakBuffer (Weak (Buffer mut pin fin heap))
      -- ^ The source is a weak buffer. If the buffer is collected, its contents
      -- is copied in to a new buffer and the view is updated to use it.
   | SourceWeakView   (Weak View)
      -- ^ The source is a weak view. If the source view is collected, the
      -- current view is updated to use whatever the source view uses as a
      -- source (another view or a buffer).
      -- This mechanism makes buffer contents cascade into smaller views while
      -- preserving some sharing.

-- | A view on a buffer
newtype View = View (IORef (ViewSource,ViewPattern))

-- | A view pattern
data ViewPattern
   = PatternFull
   | Pattern1D
      { pattern1DOffset :: {-# UNPACK #-} !Word
      , pattern1DSize   :: {-# UNPACK #-} !Word
      }
   | Pattern2D
      { pattern2DOffset :: {-# UNPACK #-} !Word -- ^ Offset of the first line
      , pattern2DWidth  :: {-# UNPACK #-} !Word -- ^ Width (line size)
      , pattern2DHeight :: {-# UNPACK #-} !Word -- ^ Height (number of lines)
      , pattern2DStride :: {-# UNPACK #-} !Word -- ^ Stride (space between two lines)
      }

-- | Compute an actual offset when used with the given pattern
patternOffset :: ViewPattern -> Word -> Word
patternOffset pat off = case pat of
   PatternFull                -> off
   Pattern1D off2 _sz         -> off2+off
   Pattern2D off2 w _h stride -> let (y,x) = off `quotRem` w in off2+y*(w+stride)+x

-- | Compute the effective size occupied by a pattern
patternSize :: ViewPattern -> Word
patternSize = \case
   PatternFull                -> error "Don't call patternSize on PatternFull"
   Pattern1D _off sz          -> sz
   Pattern2D _off w h _stride -> w * h

-- | Read a Word8 from a view
viewReadWord8 :: MonadIO m => View -> Word -> m Word8
viewReadWord8 v@(View ref) off = do
   (src,pat) <- liftIO (readIORef ref)

   let waitForSource = do
         -- the source is gone for now. Some thread must be copying back
         -- to life so we give it some space to run with `yield` and then
         -- we retry
         liftIO yield
         viewReadWord8 v off

   case src of
      SourceBuffer b      -> bufferReadWord8IO b (patternOffset pat off)
      SourceWeakBuffer wb -> liftIO (deRefWeak wb) >>= \case
         Nothing -> waitForSource
         Just b  -> bufferReadWord8IO b (patternOffset pat off)
      SourceWeakView wv   -> liftIO (deRefWeak wv) >>= \case
         Nothing -> waitForSource
         Just v2 -> viewReadWord8 v2 (patternOffset pat off)


withValidView :: MonadIO m => View -> (forall mut pin fin heap. Buffer mut pin fin heap -> ViewPattern -> m a) -> (View -> ViewPattern -> m a) -> m a
withValidView (View ref) fb fv = go
   where
      go = do
         (src,pat) <- liftIO (readIORef ref)

         let waitForSource = do
               -- the source is gone for now. Some thread must be copying back
               -- to life so we give it some space to run with `yield` and then
               -- we retry
               liftIO yield
               go

         case src of
            SourceBuffer b      -> fb b pat
            SourceWeakBuffer wb -> liftIO (deRefWeak wb) >>= \case
               Nothing -> waitForSource
               Just b  -> fb b pat
            SourceWeakView wv   -> liftIO (deRefWeak wv) >>= \case
               Nothing -> waitForSource
               Just v2 -> fv v2 pat


-- | Create a view on a buffer
newBufferView :: MonadIO m => Buffer mut pin fin heap -> ViewPattern -> m View
newBufferView b pat = View <$> liftIO (newIORef (SourceBuffer b,pat))

-- | Create a weak view on a buffer
--
-- The buffer is weakly referenced and can be GCed. When it happens, its
-- contents is stored into a new buffer.
--
-- You should only use this for views that are much smaller than the original
-- buffer so that the copying cost is balanced by the memory occupation
-- difference.
--
newBufferWeakView :: MonadIO m => Buffer 'Immutable pin fin heap -> ViewPattern -> m View
newBufferWeakView b pat = do
   ref <- liftIO $ newIORef (SourceBuffer b,pat)

   wViewRef <- liftIO $ mkWeakIORef ref (return ())

   let finalizer = deRefWeak wViewRef >>= \case
         Nothing      -> return () -- the view is dead
         Just viewRef -> do
            bsz <- bufferSizeIO b
            newSrc <- case pat of
               -- this is stupid (the view covers the whole buffer) but let's resurrect b
               PatternFull                          -> return (SourceBuffer b)
               Pattern1D 0 psz   | psz == bsz       -> return (SourceBuffer b)
               Pattern2D 0 w h 0 | w*h == bsz       -> return (SourceBuffer b)
               Pattern2D _ w h _ | w == 0 || h == 0 -> error "Invalid Pattern2D: width or height set to 0"

               _ -> do
                  let !sz = patternSize pat
                  -- we allocate a new buffer and copy the contents in it
                  b' <- newBuffer sz
                  case pat of
                     PatternFull               -> error "Unreachable code"
                     Pattern1D poff psz        -> copyBuffer b poff b' 0 psz
                     Pattern2D poff w h stride -> forM_ [0..h-1] $ \r ->
                        copyBuffer b (poff + r*(w+stride)) b' (r*w) w
                  return (SourceBuffer b')

            -- update the view IORef
            writeIORef viewRef (newSrc,PatternFull)

   wb  <- liftIO (mkWeakPtr b (Just finalizer))
   liftIO (writeIORef ref (SourceWeakBuffer wb,pat))
   return (View ref)
