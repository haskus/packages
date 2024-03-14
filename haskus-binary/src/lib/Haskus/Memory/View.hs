{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}



-- | A view (e.g. a slice) of a buffer
--
-- Suppose we have a big buffer B.
--
-- We can have buffer views on B, say vb1 and vb2.
--
--   B <----- vb1
--   ^------- vb2
--
-- These views don't duplicate B's contents and they keep B alive.
-- If the views are much smaller than B, it may not be what we want: a lot of
-- space is wasted and we would better duplicate B's data required by the views
-- and free B.
--
-- To support this, we can use "weak buffer views", say wvb1 and wvb2.
--
--   B <~~~~~ wvb1
--   ^~~~~~~~ wvb2
--
-- If/when B is collected, new buffers are created from it for the views:
--
--   B1 <----- wvb1
--   B2 <----- wvb2
--
-- We can also create "weak view views", say wvv1 and wvv2:
--
--   B <~~~~~ wvb1 <~~~~~ wvv1
--              ^~~~~~~~~ wvv2
--
-- If/when B is collected before wvb1, the sharing is kept while the required
-- contents of B is duplicated:
--
--   B' <---- wvb1 <~~~~~ wvv1
--              ^~~~~~~~~ wvv2
--
-- When wvb1 is collected, we can be in one of the following state depending if
-- B has been collected already or not:
--
--   B <~~~~~~~~~~~~~~~~~ wvv1
--   ^~~~~~~~~~~~~~~~~~~~ wvv2
--
--              B' <~~~~~ wvv1
--              ^~~~~~~~~ wvv2
--
module Haskus.Memory.View
   ( View (..)
   , ViewSource (..)
   , ViewPattern (..)
   , viewReadWord8
   , newBufferView
   , newBufferWeakView
   , newViewWeakView
   , copyBufferWithPattern
   , viewToBuffer
   , showViewState
   , patternSize
   , unsafePatternSize
   )
where

import Data.IORef
import System.Mem.Weak
import Control.Concurrent

import Haskus.Utils.Monad
import Haskus.Number.Word
import Haskus.Memory.Buffer

-- $setup
-- >>> import Haskus.Memory.Buffer

-- | The source of a view
--
-- Weak views are used so that the underlying buffer can be freed by the GC.
-- When it happens and if the view is still alive the contents of the buffer
-- used by the view is copied into a fresh (usually smaller) buffer.
--
-- Weak views can also be used as sources: in this case, when the source
-- view is GCed, the current view is updated to point to the source of the
-- source.
--
data ViewSource
   = SourceBuffer Buffer
      -- ^ The source is a buffer. The view keeps the buffer alive
   | SourceWeakBuffer (Weak Buffer)
      -- ^ The source is a weak buffer. If the buffer is collected, its contents
      -- is copied in to a new buffer and the view is updated to use it.
   | SourceWeakView (Weak ViewIORef)
      -- ^ The source is a weak view. If the source view is collected, the
      -- current view is updated to use whatever the source view uses as a
      -- source (another view or a buffer).
      -- This mechanism makes buffer contents cascade into smaller views while
      -- preserving some sharing.

-- | A view on a buffer
newtype View = View ViewIORef

type ViewIORef = IORef (ViewSource,ViewPattern)

-- | A view pattern
data ViewPattern
   = PatternFull  -- ^ The whole buffer
   | Pattern1D    -- ^ 1D slice
      { pattern1DOffset :: {-# UNPACK #-} !Word -- ^ Offset of the first cell
      , pattern1DSize   :: {-# UNPACK #-} !Word -- ^ Number of cells
      }
   | Pattern2D    -- ^ 2D slice
      { pattern2DOffset :: {-# UNPACK #-} !Word -- ^ Offset of the first line
      , pattern2DWidth  :: {-# UNPACK #-} !Word -- ^ Width (line size)
      , pattern2DHeight :: {-# UNPACK #-} !Word -- ^ Height (number of lines)
      , pattern2DStride :: {-# UNPACK #-} !Word -- ^ Stride (space between two lines)
      }
   | PatternOn ViewPattern ViewPattern -- ^ Composed pattern
   deriving (Show)

-- | Compute an actual offset when used with the given pattern
patternOffset :: ViewPattern -> Word -> Word
patternOffset pat off = case pat of
   PatternFull                -> off
   Pattern1D off2 _sz         -> off2+off
   Pattern2D off2 w _h stride -> let (y,x) = off `quotRem` w in off2+y*(w+stride)+x
   PatternOn p1 p2            -> patternOffset p2 (patternOffset p1 off)

-- | Compute the effective size occupied by a pattern
unsafePatternSize :: ViewPattern -> Word
unsafePatternSize = \case
   PatternFull                -> error "Don't call unsafePatternSize on PatternFull"
   Pattern1D _off sz          -> sz
   Pattern2D _off w h _stride -> w * h
   PatternOn p1 _p2           -> unsafePatternSize p1

-- | Compute the effective size occupied by a pattern
patternSize :: ViewPattern -> Word -> Word
patternSize v bsz = case v of
   PatternFull                -> bsz
   Pattern1D _off sz          -> sz
   Pattern2D _off w h _stride -> w * h
   PatternOn p1 p2            -> patternSize p1 (patternSize p2 bsz)

-- | Combine two patterns
--
-- Remove trivial patterns combinations
patternApplyOn :: ViewPattern -> ViewPattern -> ViewPattern
patternApplyOn p1 p2 = case (p1, p2) of
   (PatternFull,p)                     -> p
   (p,PatternFull)                     -> p
   (Pattern1D o1 s1, Pattern1D o2 _s2) -> Pattern1D (o1+o2) s1
   _                                   -> PatternOn p1 p2

-- | Read a Word8 from a view
viewReadWord8 :: View -> Word -> IO Word8
viewReadWord8 view off =
   withValidView view
      (\b pat -> bufferReadWord8 b (patternOffset pat off))
      (\b pat -> bufferReadWord8 b (patternOffset pat off))
      (\v pat -> viewReadWord8   v (patternOffset pat off))


-- | Wait for a view to be valid then use one of the 3 passed functions on it
-- depending on its source type (Buffer, WeakBuffer, WeakView).
withValidView
   :: MonadIO m
   => View
   -> (Buffer -> ViewPattern -> m a)
   -> (Buffer -> ViewPattern -> m a)
   -> (View -> ViewPattern -> m a)
   -> m a
withValidView (View ref) fb fwb fwv = go True
   where
      go _firstRun = do
         (src,pat) <- liftIO (readIORef ref)

         let waitForSource = do
               -- the source is gone for now. Some thread must be copying back
               -- to life so we give it some space to run with `yield` and then
               -- we retry
               liftIO yield
               -- TODO: We execute the spin-lock in a thread to avoid locking
               -- the finalizer thread
               -- if firstRun
               --    then forkIO (go False)
               --    else go False
               go False

         case src of
            SourceBuffer b      -> fb b pat
            SourceWeakBuffer wb -> liftIO (deRefWeak wb) >>= \case
               Nothing -> waitForSource
               Just b  -> fwb b pat
            SourceWeakView wv   -> liftIO (deRefWeak wv) >>= \case
               Nothing -> waitForSource
               Just v2 -> fwv (View v2) pat


-- | Create a view on a buffer
newBufferView :: MonadIO m => Buffer -> ViewPattern -> m View
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
newBufferWeakView :: MonadIO m => Buffer -> ViewPattern -> m View
newBufferWeakView b pat = do
   -- temporarily create a View that non-weakly references the buffer
   v <- View <$> (liftIO $ newIORef (SourceBuffer b,pat))

   -- assign the weak buffer source to the view
   assignBufferWeakView v b pat

   return v

assignBufferWeakView
   :: MonadIO m
   => View
   -> Buffer
   -> ViewPattern
   -> m ()
assignBufferWeakView (View ref) b pat = do
   -- create a weak reference to the view
   wViewRef <- liftIO $ mkWeakIORef ref (return ())
   -- associate a finalizer to the buffer that will copy the duplicate the
   -- buffer when it is collected
   let finalizer = bufferWeakViewFinalier b pat wViewRef
   wb  <- liftIO (mkWeakPtr b (Just finalizer))
   -- update the view to reference the weak buffer
   liftIO (writeIORef ref (SourceWeakBuffer wb,pat))


bufferWeakViewFinalier
   :: Buffer         -- ^ Source buffer
   -> ViewPattern    -- ^ View pattern
   -> Weak ViewIORef -- ^ Weak IORef of the view
   -> IO ()
bufferWeakViewFinalier b pat wViewRef = deRefWeak wViewRef >>= \case
   Nothing      -> return () -- the view is dead
   Just viewRef -> do
      bsz <- bufferSize b
      newSrc <- case pat of
         -- this is stupid (the view covers the whole buffer) but let's resurrect b
         PatternFull                          -> return (SourceBuffer b)
         Pattern1D 0 psz   | psz == bsz       -> return (SourceBuffer b)
         Pattern2D 0 w h 0 | w*h == bsz       -> return (SourceBuffer b)
         Pattern2D _ w h _ | w == 0 || h == 0 -> error "Invalid Pattern2D: width or height set to 0"

         _ -> do
            -- we allocate a new buffer and copy the contents in it
            b'  <- copyBufferWithPattern b pat
            return (SourceBuffer b')

      -- update the view IORef
      writeIORef viewRef (newSrc,PatternFull)


-- | Create a weak view on a view
newViewWeakView :: MonadIO m => View -> ViewPattern -> m View
newViewWeakView src@(View srcRef) pat = do
   -- create a new view. For now it only combines the two patterns
   -- and uses the same source.
   v <- liftIO $ do
         (srcSrc,srcPat) <- readIORef srcRef
         View <$> newIORef (srcSrc, pat `patternApplyOn` srcPat)
   -- assign it the weak view source
   assignViewWeakView v src pat
   return v

assignViewWeakView :: MonadIO m => View -> View -> ViewPattern -> m ()
assignViewWeakView (View ref) (View srcRef) pat = do
   -- create a weak reference on the current view (its IORef in fact)
   weakView <- liftIO $ mkWeakIORef ref (return ())

   -- create a finalizer for srcRef. We can reference srcRef directly but not
   -- the current view which must be accessed through its weak reference
   -- "weakView"
   let finalizer = viewWeakViewFinalizer weakView srcRef pat

   -- the finalizer is attached to the IORef of the source view
   wSrcRef  <- liftIO $ mkWeakIORef srcRef finalizer

   -- we update the view
   liftIO (writeIORef ref (SourceWeakView wSrcRef,pat))

   -- we don't want the finalizer to run before we write the IORef
   -- FIXME: liftIO (touch srcRef)

viewWeakViewFinalizer :: Weak ViewIORef -> ViewIORef -> ViewPattern -> IO ()
viewWeakViewFinalizer weakView srcRef pat = deRefWeak weakView >>= \case
   Nothing      -> return () -- the view is dead
   Just viewRef -> do
      let v = View viewRef
      -- wait for the source to be valid and then handle it
      withValidView (View srcRef)
         (\srcB srcPat  -> do
            let newPat = pat `patternApplyOn` srcPat
            assignBufferWeakView v srcB newPat
         )
         (\srcWB srcPat -> do
            let newPat = pat `patternApplyOn` srcPat
            assignBufferWeakView v srcWB newPat
         )
         (\srcV  srcPat -> do
            let newPat = pat `patternApplyOn` srcPat
            assignViewWeakView v srcV newPat
         )

-- | Allocate a new buffer initialized with the contents of the source buffer
-- according to the given pattern
copyBufferWithPattern :: Buffer -> ViewPattern -> IO Buffer
copyBufferWithPattern b pat = do
   bsz <- bufferSize b
   let !sz = patternSize pat bsz
   b' <- newBuffer sz
   case pat of
      PatternFull               -> error "Unreachable code"
      Pattern1D poff psz        -> bufferCopy b poff b' 0 psz
      Pattern2D poff w h stride -> forM_ [0..h-1] $ \r ->
         bufferCopy b (poff + r*(w+stride)) b' (r*w) w
      PatternOn _p1 _p2         -> forM_ [0..sz-1] $ \off -> do
         -- Not very efficient to copy byte by byte...
         v <- bufferReadWord8 b (patternOffset pat off)
         bufferWriteWord8 b' off v
   return b'


-- | Convert a view into an actual buffer
viewToBuffer :: View -> IO Buffer
viewToBuffer = go PatternFull
   where
      go :: ViewPattern -> View -> IO Buffer
      go pat v = withValidView v
         (\b pat2 -> copyBufferWithPattern b (pat `patternApplyOn` pat2))
         (\b pat2 -> copyBufferWithPattern b (pat `patternApplyOn` pat2))
         (\v2 pat2 -> go (pat `patternApplyOn` pat2) v2)

-- | Display the state of a View
--
-- >>> :set -XOverloadedLists
-- >>> import System.Mem
-- >>> v <- newBufferWeakView ([10,11,12,13,14,15,16,17] :: Buffer) (Pattern1D 2 4)
-- >>> v2 <- newViewWeakView v (Pattern1D 1 1)
--
-- > putStr =<< showViewState v2
-- View source: weak view
-- Source size: 4
-- View pattern: Pattern1D {pattern1DOffset = 1, pattern1DSize = 1}
-- Wasted space: 75%
-- Source:
--    View source: weak buffer
--    Source size: 8
--    View pattern: Pattern1D {pattern1DOffset = 2, pattern1DSize = 4}
--    Wasted space: 50%
--
-- > performGC
-- > putStr =<< showViewState v2
-- View source: weak view
-- Source size: 4
-- View pattern: Pattern1D {pattern1DOffset = 1, pattern1DSize = 1}
-- Wasted space: 75%
-- Source:
--    View source: buffer
--    Source size: 4
--    View pattern: PatternFull
--    Wasted space: 0%
--
showViewState :: View -> IO String
showViewState = fmap fst . go

   where
      go v = withValidView v
         (\b pat -> do
            sz <- bufferSize b
            let psz = patternSize pat sz
            return (unlines
               [ "View source: buffer"
               , "Source size: " ++ show sz
               , "View pattern: " ++ show pat
               , "Wasted space: " ++ show (100 - ((psz * 100) `div` sz)) ++ "%"
               ], psz)
         )
         (\b pat -> do
            sz <- bufferSize b
            let psz = patternSize pat sz
            return (unlines
               [ "View source: weak buffer"
               , "Source size: " ++ show sz
               , "View pattern: " ++ show pat
               , "Wasted space: " ++ show (100 - ((psz * 100) `div` sz)) ++ "%"
               ], psz)
         )
         (\v2 pat -> do
            (r,sz) <- go v2
            let psz = patternSize pat sz
            return (unlines $
               [ "View source: weak view"
               , "Source size: " ++ show sz
               , "View pattern: " ++ show pat
               , "Wasted space: " ++ show (100 - ((psz * 100) `div` sz)) ++ "%"
               , "Source:"
               ] ++ fmap ("   " ++) (lines r), psz)
         )
