{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

-- | Serializer into a mutable buffer
--
-- >>> let w = do putWord8 0x01 ; putWord32BE 0x23456789 ; putWord32BE 0xAABBCCDD
-- >>> b <- newBuffer 10
-- >>> void $ runBufferPut b 0 overflowBufferFail w
-- >>> xs <- forM [0..4] (bufferReadWord8IO b)
-- >>> xs == [0x01,0x23,0x45,0x67,0x89]
-- True
--
-- >>> b <- newBuffer 2 -- small buffer
-- >>> (_,b',_) <- runBufferPut b 0 overflowBufferDouble w
-- >>> xs <- forM [0..4] (bufferReadWord8IO b')
-- >>> xs == [0x01,0x23,0x45,0x67,0x89]
-- True
-- >>> bufferSizeIO b'
-- 16
--
module Haskus.Binary.Serialize.Buffer
   ( BufferPutT (..)
   , BufferPut
   , getPutOffset
   , getPutBuffer
   , setPutOffset
   , runBufferPut
   , liftBufferPut
   -- * Buffer overflow
   , OverflowStrategy (..)
   , BufferOverflow (..)
   , getOverflowStrategy
   , overflowBufferFail
   , overflowBufferDouble
   , overflowBufferDoublePinned
   , overflowBufferAdd
   , overflowBufferAddPinned
   )
where

import Haskus.Binary.Serialize.Put
import Haskus.Memory.Buffer
import Haskus.Utils.Monad
import Haskus.Utils.Flow

import Data.Functor.Identity
import Control.Monad.Trans.State.Strict as S
import Control.Monad.Fail as F
import Control.Monad.Fix

-- | Action to perform when the buffer isn't large enough to contain the
-- required data (extend the buffer, flush the data, etc.)
--
-- The returned buffer and offset replace the current ones.
newtype OverflowStrategy m b = OverflowStrategy (BufferOverflow b -> m (b,Word))

-- | Buffer overflow strategy: fails when there isn't enough space left
overflowBufferFail :: MonadFail m => OverflowStrategy m b
overflowBufferFail = OverflowStrategy \ex -> do
   F.fail $ "Not enough space in the target buffer (requiring "
          ++ show (overflowRequired ex) ++ " bytes)"

-- | Buffer extend strategy: double the buffer size each time and copy the
-- original contents in it
overflowBufferDouble :: MonadIO m => OverflowStrategy m BufferM
overflowBufferDouble = OverflowStrategy \ex -> do
   sz <- bufferSizeIO (overflowBuffer ex)
   let off = overflowOffset   ex
       req = overflowRequired ex
       b   = overflowBuffer   ex
       makeSzs i = i*i : makeSzs (i*i) -- infinite list of doubling sizes
       newSz = head <| filter (> req+off) (makeSzs sz)
   newB <- newBuffer newSz
   copyBuffer b 0 newB 0 off
   pure (newB,off)

-- | Buffer extend strategy: double the buffer size each time and copy the
-- original contents in it
overflowBufferDoublePinned :: MonadIO m => Maybe Word -> OverflowStrategy m BufferMP
overflowBufferDoublePinned malignment = OverflowStrategy \ex -> do
   sz <- bufferSizeIO (overflowBuffer ex)
   let off = overflowOffset   ex
       req = overflowRequired ex
       b   = overflowBuffer   ex
       makeSzs i = i*i : makeSzs (i*i) -- infinite list of doubling sizes
       newSz = head <| filter (> req+off) (makeSzs sz)
   newB <- case malignment of
      Nothing -> newPinnedBuffer newSz
      Just al -> newAlignedPinnedBuffer newSz al
   copyBuffer b 0 newB 0 off
   pure (newB,off)

-- | Buffer extend strategy: add the given size each time and copy the
-- original contents in it
overflowBufferAdd :: MonadIO m => Word -> OverflowStrategy m BufferM
overflowBufferAdd addSz = OverflowStrategy \ex -> do
   sz <- bufferSizeIO (overflowBuffer ex)
   let off = overflowOffset   ex
       req = overflowRequired ex
       b   = overflowBuffer   ex
       makeSzs i = i+addSz : makeSzs (i+addSz) -- infinite list of added sizes
       newSz = head <| filter (> req+off) (makeSzs sz)
   newB <- newBuffer newSz
   copyBuffer b 0 newB 0 off
   pure (newB,off)

-- | Buffer extend strategy: add the given size each time and copy the
-- original contents in it
overflowBufferAddPinned :: MonadIO m => Maybe Word -> Word -> OverflowStrategy m BufferMP
overflowBufferAddPinned malignment addSz = OverflowStrategy \ex -> do
   sz <- bufferSizeIO (overflowBuffer ex)
   let off = overflowOffset   ex
       req = overflowRequired ex
       b   = overflowBuffer   ex
       makeSzs i = i+addSz : makeSzs (i+addSz) -- infinite list of added sizes
       newSz = head <| filter (> req+off) (makeSzs sz)
   newB <- case malignment of
      Nothing -> newPinnedBuffer newSz
      Just al -> newAlignedPinnedBuffer newSz al
   copyBuffer b 0 newB 0 off
   pure (newB,off)



-- | Buffer extension information
data BufferOverflow b = BufferOverflow
   { overflowBuffer   :: b     -- ^ Current buffer
   , overflowOffset   :: Word  -- ^ Current offset in buffer
   , overflowRequired :: Word  -- ^ Required size in bytes (don't take into account leftover bytes in the current buffer)
   }

-- | BufferPutT state
data BufferPutState m b = BufferPutState
   { bufferPutBuffer :: b                    -- ^ Buffer used for writing
   , bufferPutOffset :: Word                 -- ^ Current offset
   , bufferPutStrat  :: OverflowStrategy m b -- ^ Extension stretegy
   } 

-- | A Put monad than fails when there is not enough space in the target buffer
newtype BufferPutT b m a
   = BufferPutT (StateT (BufferPutState m b) m a) 
   deriving newtype (Functor, Applicative, Monad, MonadFail, MonadFix, MonadIO)

type BufferPut b a = BufferPutT b Identity a

-- | Lift into BufferPutT
liftBufferPut :: Monad m => m a -> BufferPutT b m a
liftBufferPut act = BufferPutT (lift act)

-- | Run a buffer put
runBufferPut :: Monad m => b -> Word -> OverflowStrategy m b -> BufferPutT b m a -> m (a,b,Word)
runBufferPut b off strat (BufferPutT s) = do
   (a,s') <- runStateT s (BufferPutState b off strat)
   return (a,bufferPutBuffer s',bufferPutOffset s')

-- | Get current offset
getPutOffset :: Monad m => BufferPutT b m Word
getPutOffset = BufferPutT (bufferPutOffset <$> S.get)

-- | Get buffer
getPutBuffer :: Monad m => BufferPutT b m b
getPutBuffer = BufferPutT (bufferPutBuffer <$> S.get)

-- | Set buffer
setPutBuffer :: Monad m => b -> BufferPutT b m ()
setPutBuffer v = BufferPutT do
   S.modify \s -> s { bufferPutBuffer = v }


-- | Get current offset
setPutOffset :: Monad m => Word -> BufferPutT b m ()
setPutOffset v = BufferPutT do
   S.modify \s -> s { bufferPutOffset = v }

-- | Get extend strategy
getOverflowStrategy :: Monad m => BufferPutT b m (OverflowStrategy m b)
getOverflowStrategy = BufferPutT (bufferPutStrat <$> S.get)


-- | Helper to put something
putSomething
   :: MonadIO m
   => Word
   -> (Buffer Mutable pin fin heap -> Word -> t -> m ())
   -> t
   -> BufferPutT (Buffer Mutable pin fin heap) m ()
{-# INLINABLE putSomething #-}
putSomething sz act v = putSomeThings sz $ Just \b off -> act b off v

-- | Helper to put some things
putSomeThings
   :: MonadIO m
   => Word
   -> Maybe (Buffer Mutable pin fin heap -> Word -> m ())
   -> BufferPutT (Buffer Mutable pin fin heap) m ()
{-# INLINABLE putSomeThings #-}
putSomeThings sz mact = do
   off <- getPutOffset
   b   <- getPutBuffer
   bs  <- liftIO (bufferSizeIO b)
   let !newOff = off+sz

   if (newOff > bs)
      then do -- we need to extend/flush the buffer
         OverflowStrategy strat <- getOverflowStrategy
         (upB,upOff) <- liftBufferPut <| strat <| BufferOverflow
                              { overflowBuffer   = b
                              , overflowOffset   = off
                              , overflowRequired = sz
                              }
         setPutBuffer upB
         setPutOffset upOff
         putSomeThings sz mact

      else case mact of
            Nothing  -> return () -- we only preallocate
            Just act -> do        -- we write something for real
               liftBufferPut (act b off)
               setPutOffset newOff
   

instance
   ( MonadIO m
   , MonadFail m
   ) => PutMonad (BufferPutT (Buffer 'Mutable pin gc heap) m)
   where
      putWord8  = putSomething 1 bufferWriteWord8IO
      putWord16 = putSomething 2 bufferWriteWord16IO
      putWord32 = putSomething 4 bufferWriteWord32IO
      putWord64 = putSomething 8 bufferWriteWord64IO

      putWord8s xs = putSomeThings (fromIntegral (length xs)) $ Just \b off -> do
         forM_ ([off,(off+1)..] `zip` xs) $ \(boff,v) -> do
            bufferWriteWord8IO b boff v

      putWord16s xs = putSomeThings (2*fromIntegral (length xs)) $ Just \b off -> do
         forM_ ([off,(off+2)..] `zip` xs) $ \(boff,v) -> do
            bufferWriteWord16IO b boff v

      putWord32s xs = putSomeThings (4*fromIntegral (length xs)) $ Just \b off -> do
         forM_ ([off,(off+4)..] `zip` xs) $ \(boff,v) -> do
            bufferWriteWord32IO b boff v

      putWord64s xs = putSomeThings (8*fromIntegral (length xs)) $ Just \b off -> do
         forM_ ([off,(off+8)..] `zip` xs) $ \(boff,v) -> do
            bufferWriteWord64IO b boff v

      preAllocateAtLeast l = putSomeThings l Nothing

      putBuffer x = do
         sz <- liftIO (bufferSizeIO x)
         putSomeThings sz $ Just \b off -> copyBuffer x 0 b off sz
