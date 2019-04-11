{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

-- | Serializer into a mutable buffer
--
-- >>> let w = do putWord8 0x01 ; putWord32BE 0x23456789
-- >>> b <- newBuffer 10
-- >>> void $ runBufferPut b 0 w
-- >>> xs <- forM [0..4] (bufferReadWord8IO b)
-- >>> xs == [0x01,0x23,0x45,0x67,0x89]
-- True
--
module Haskus.Format.Binary.Serialize.Buffer
   ( BufferPutT (..)
   , BufferPut
   , getPutOffset
   , getPutBuffer
   , setPutOffset
   , runBufferPut
   )
where

import Haskus.Format.Binary.Serialize
import Haskus.Memory.Buffer
import Haskus.Utils.Monad

import Data.Functor.Identity
import Control.Monad.Trans.State as S
import Control.Monad.Fail as F
import Control.Monad.Fix

data BufferPutState b = BufferPutState
   { bufferPutBuffer :: b    -- ^ Buffer used for writing
   , bufferPutOffset :: Word -- ^ Current offset
   } 

-- | A Put monad than fails when there is not enough space in the target buffer
newtype BufferPutT b m a
   = BufferPutT (StateT (BufferPutState b) m a) 
   deriving newtype (Functor, Applicative, Monad, MonadFail, MonadFix, MonadIO, MonadTrans)

type BufferPut b a    = BufferPutT b Identity a

-- | Run a buffer put
runBufferPut :: Monad m => b -> Word -> BufferPutT b m a -> m (a,Word)
runBufferPut b off (BufferPutT s) = do
   (a,s') <- runStateT s (BufferPutState b off)
   return (a,bufferPutOffset s')

-- | Get current offset
getPutOffset :: Monad m => BufferPutT b m Word
getPutOffset = BufferPutT (bufferPutOffset <$> S.get)

-- | Get buffer
getPutBuffer :: Monad m => BufferPutT b m b
getPutBuffer = BufferPutT (bufferPutBuffer <$> S.get)

-- | Get current offset
setPutOffset :: Monad m => Word -> BufferPutT b m ()
setPutOffset v = BufferPutT $ do
   S.modify (\s -> s { bufferPutOffset = v })


-- | Called when there is not enough space left in the buffer
bufferPutNotEnoughSpace :: (MonadFail m, MonadIO m) => Word -> BufferPutT b m ()
bufferPutNotEnoughSpace reqSize = do
   F.fail $ "Not enough space in the target buffer (requiring "
          ++ show reqSize ++ " bytes)"
   
-- | Helper to put something
putSomething
   :: (MonadIO m, MonadFail m)
   => Word
   -> (Buffer mut pin fin heap -> Word -> t -> m ())
   -> t
   -> BufferPutT (Buffer mut pin fin heap) m ()
{-# INLINABLE putSomething #-}
putSomething sz act v = do
   off <- getPutOffset
   b   <- getPutBuffer
   bs  <- liftIO (bufferSizeIO b)
   let !newOff = off+sz
   when (newOff > bs) $ bufferPutNotEnoughSpace sz
   lift (act b off v)
   setPutOffset newOff

-- | Helper to put some things
putSomeThings
   :: (MonadIO m, MonadFail m)
   => Word
   -> (Buffer mut pin fin heap -> Word -> m ())
   -> BufferPutT (Buffer mut pin fin heap) m ()
{-# INLINABLE putSomeThings #-}
putSomeThings sz act = do
   off <- getPutOffset
   b   <- getPutBuffer
   bs  <- liftIO (bufferSizeIO b)
   let !newOff = off+sz
   when (newOff > bs) $ bufferPutNotEnoughSpace sz
   lift (act b off)
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

      putWord8s xs = putSomeThings (fromIntegral (length xs)) $ \b off -> do
         forM_ ([off,(off+1)..] `zip` xs) $ \(boff,v) -> do
            bufferWriteWord8IO b boff v

      putWord16s xs = putSomeThings (2*fromIntegral (length xs)) $ \b off -> do
         forM_ ([off,(off+2)..] `zip` xs) $ \(boff,v) -> do
            bufferWriteWord16IO b boff v

      putWord32s xs = putSomeThings (4*fromIntegral (length xs)) $ \b off -> do
         forM_ ([off,(off+4)..] `zip` xs) $ \(boff,v) -> do
            bufferWriteWord32IO b boff v

      putWord64s xs = putSomeThings (8*fromIntegral (length xs)) $ \b off -> do
         forM_ ([off,(off+8)..] `zip` xs) $ \(boff,v) -> do
            bufferWriteWord64IO b boff v

      preAllocateAtLeast l = do
         off <- getPutOffset
         b   <- getPutBuffer
         bs  <- liftIO (bufferSizeIO b)
         when (l+off > bs) $ bufferPutNotEnoughSpace l

      putBuffer x = do
         sz <- liftIO (bufferSizeIO x)
         putSomeThings sz (\b off -> copyBuffer x 0 b off sz)
