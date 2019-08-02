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
-- >>> void $ runBufferPut b 0 extendBufferFail w
-- >>> xs <- forM [0..4] (bufferReadWord8IO b)
-- >>> xs == [0x01,0x23,0x45,0x67,0x89]
-- True
--
-- >>> b <- newBuffer 2 -- small buffer
-- >>> (_,b',_) <- runBufferPut b 0 extendBufferDouble w
-- >>> xs <- forM [0..4] (bufferReadWord8IO b')
-- >>> xs == [0x01,0x23,0x45,0x67,0x89]
-- True
-- >>> bufferSizeIO b'
-- 16
--
module Haskus.Format.Binary.Serialize.Buffer
   ( BufferPutT (..)
   , BufferPut
   , getPutOffset
   , getPutBuffer
   , setPutOffset
   , runBufferPut
   , liftBufferPut
   -- * Buffer extension
   , ExtendStrategy (..)
   , BufferExtend (..)
   , getExtendStrategy
   , extendBufferFail
   , extendBufferDouble
   , extendBufferDoublePinned
   , extendBufferAdd
   , extendBufferAddPinned
   )
where

import Haskus.Format.Binary.Serialize
import Haskus.Memory.Buffer
import Haskus.Utils.Monad
import Haskus.Utils.Flow

import Data.Functor.Identity
import Control.Monad.Trans.State as S
import Control.Monad.Fail as F
import Control.Monad.Fix

-- | Return a new buffer able to store the required data.
--
-- We don't check buffer size so you have to ensure that the returned buffer is
-- large enough.
newtype ExtendStrategy m b = ExtendStrategy (BufferExtend b -> m b)

-- | Buffer extend strategy: fails when there isn't enough space left
extendBufferFail :: MonadFail m => ExtendStrategy m b
extendBufferFail = ExtendStrategy \ex -> do
   F.fail $ "Not enough space in the target buffer (requiring "
          ++ show (extendRequired ex) ++ " bytes)"

-- | Buffer extend strategy: double the buffer size each time
extendBufferDouble :: MonadIO m => ExtendStrategy m BufferM
extendBufferDouble = ExtendStrategy \ex -> do
   sz <- bufferSizeIO (extendBuffer ex)
   let off = extendOffset   ex
       req = extendRequired ex
       makeSzs i = i*i : makeSzs (i*i) -- infinite list of doubling sizes
       newSz = head <| filter (> req+off) (makeSzs sz)
   newBuffer newSz

-- | Buffer extend strategy: double the buffer size each time
extendBufferDoublePinned :: MonadIO m => Maybe Word -> ExtendStrategy m BufferMP
extendBufferDoublePinned malignment = ExtendStrategy \ex -> do
   sz <- bufferSizeIO (extendBuffer ex)
   let off = extendOffset   ex
       req = extendRequired ex
       makeSzs i = i*i : makeSzs (i*i) -- infinite list of doubling sizes
       newSz = head <| filter (> req+off) (makeSzs sz)
   case malignment of
      Nothing -> newPinnedBuffer newSz
      Just al -> newAlignedPinnedBuffer newSz al

-- | Buffer extend strategy: add the given size each time
extendBufferAdd :: MonadIO m => Word -> ExtendStrategy m BufferM
extendBufferAdd addSz = ExtendStrategy \ex -> do
   sz <- bufferSizeIO (extendBuffer ex)
   let off = extendOffset   ex
       req = extendRequired ex
       makeSzs i = i+addSz : makeSzs (i+addSz) -- infinite list of added sizes
       newSz = head <| filter (> req+off) (makeSzs sz)
   newBuffer newSz

-- | Buffer extend strategy: add the given size each time
extendBufferAddPinned :: MonadIO m => Maybe Word -> Word -> ExtendStrategy m BufferMP
extendBufferAddPinned malignment addSz = ExtendStrategy \ex -> do
   sz <- bufferSizeIO (extendBuffer ex)
   let off = extendOffset   ex
       req = extendRequired ex
       makeSzs i = i+addSz : makeSzs (i+addSz) -- infinite list of added sizes
       newSz = head <| filter (> req+off) (makeSzs sz)
   case malignment of
      Nothing -> newPinnedBuffer newSz
      Just al -> newAlignedPinnedBuffer newSz al



-- | Buffer extension information
data BufferExtend b = BufferExtend
   { extendBuffer   :: b     -- ^ Current buffer
   , extendOffset   :: Word  -- ^ Current offset in buffer
   , extendRequired :: Word  -- ^ Required size in bytes (don't take into account leftover bytes in the current buffer)
   }

-- | BufferPutT state
data BufferPutState m b = BufferPutState
   { bufferPutBuffer :: b                  -- ^ Buffer used for writing
   , bufferPutOffset :: Word               -- ^ Current offset
   , bufferPutStrat  :: ExtendStrategy m b -- ^ Extension stretegy
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
runBufferPut :: Monad m => b -> Word -> ExtendStrategy m b -> BufferPutT b m a -> m (a,b,Word)
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
getExtendStrategy :: Monad m => BufferPutT b m (ExtendStrategy m b)
getExtendStrategy = BufferPutT (bufferPutStrat <$> S.get)


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

   when (newOff > bs) do -- we need to extend the buffer
      ExtendStrategy strat <- getExtendStrategy
      newB <- liftBufferPut <| strat <| BufferExtend
                  { extendBuffer   = b
                  , extendOffset   = off
                  , extendRequired = sz
                  }
      copyBuffer b 0 newB 0 off
      setPutBuffer newB

   b' <- getPutBuffer -- get the newer put buffer if any
   case mact of
      Nothing  -> return () -- we only preallocate
      Just act -> do        -- we write something for real
         liftBufferPut (act b' off)
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
