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
import Haskus.Data.Buffer
import Haskus.Utils.Monad

import Data.Functor.Identity
import Control.Monad.Trans.State as S
import Control.Monad.Fail as F
import Control.Monad.Fix

data BufferPutState b = BufferPutState
   { bufferPutBuffer :: b    -- ^ Buffer used for writing
   , bufferPutOffset :: Word -- ^ Current offset
   } 

newtype BufferPutT m b a
   = BufferPutT (StateT (BufferPutState b) m a) 
   deriving newtype (Functor, Applicative, Monad, MonadFail, MonadFix, MonadIO)

type BufferPut b a    = BufferPutT Identity b a

-- | Run a buffer put
runBufferPut :: Monad m => b -> Word -> BufferPutT m b a -> m (a,Word)
runBufferPut b off (BufferPutT s) = do
   (a,s') <- runStateT s (BufferPutState b off)
   return (a,bufferPutOffset s')

-- | Get current offset
getPutOffset :: Monad m => BufferPutT m b Word
getPutOffset = BufferPutT (bufferPutOffset <$> S.get)

-- | Get buffer
getPutBuffer :: Monad m => BufferPutT m b b
getPutBuffer = BufferPutT (bufferPutBuffer <$> S.get)

-- | Get current offset
setPutOffset :: Monad m => Word -> BufferPutT m b ()
setPutOffset v = BufferPutT $ do
   S.modify (\s -> s { bufferPutOffset = v })


instance
   ( MonadIO m
   , MonadFail m
   ) => PutMonad (BufferPutT m (Buffer 'Mutable pin gc heap))
   where
      putWord8 v = do
         off <- getPutOffset
         b   <- getPutBuffer
         bs  <- liftIO (bufferSizeIO b)
         let !l = off+1
         when (l > bs) $
            F.fail $ "Not enough space in the target buffer ("
                   ++ show l ++ " > " ++ show bs ++ ")"

         bufferWriteWord8IO b off v
         setPutOffset l

      putWord16 v = do
         off <- getPutOffset
         b   <- getPutBuffer
         bs  <- liftIO (bufferSizeIO b)
         let !l = off+2
         when (l > bs) $
            F.fail $ "Not enough space in the target buffer ("
                   ++ show l ++ " > " ++ show bs ++ ")"

         bufferWriteWord16IO b off v
         setPutOffset l

      putWord32 v = do
         off <- getPutOffset
         b   <- getPutBuffer
         bs  <- liftIO (bufferSizeIO b)
         let !l = off+4
         when (l > bs) $
            F.fail $ "Not enough space in the target buffer ("
                   ++ show l ++ " > " ++ show bs ++ ")"

         bufferWriteWord32IO b off v
         setPutOffset l

      putWord64 v = do
         off <- getPutOffset
         b   <- getPutBuffer
         bs  <- liftIO (bufferSizeIO b)
         let !l = off+4
         when (l > bs) $
            F.fail $ "Not enough space in the target buffer ("
                   ++ show l ++ " > " ++ show bs ++ ")"

         bufferWriteWord64IO b off v
         setPutOffset l

      putWord8s xs = do
         off <- getPutOffset
         b   <- getPutBuffer
         bs  <- liftIO (bufferSizeIO b)
         let !l = fromIntegral (length xs)
         when (l+off > bs) $
            F.fail $ "Not enough space in the target buffer ("
                   ++ show (l+off) ++ " > " ++ show bs ++ ")"

         forM_ ([off,(off+1)..] `zip` xs) $ \(boff,v) -> do
            bufferWriteWord8IO b boff v

         setPutOffset (off+l)

      putWord16s xs = do
         off <- getPutOffset
         b   <- getPutBuffer
         bs  <- liftIO (bufferSizeIO b)
         let !l = 2*fromIntegral (length xs)
         when (l+off > bs) $
            F.fail $ "Not enough space in the target buffer ("
                   ++ show (l+off) ++ " > " ++ show bs ++ ")"

         forM_ ([off,(off+2)..] `zip` xs) $ \(boff,v) -> do
            bufferWriteWord16IO b boff v

         setPutOffset (off+l)

      putWord32s xs = do
         off <- getPutOffset
         b   <- getPutBuffer
         bs  <- liftIO (bufferSizeIO b)
         let !l = 4*fromIntegral (length xs)
         when (l+off > bs) $
            F.fail $ "Not enough space in the target buffer ("
                   ++ show (l+off) ++ " > " ++ show bs ++ ")"

         forM_ ([off,(off+4)..] `zip` xs) $ \(boff,v) -> do
            bufferWriteWord32IO b boff v

         setPutOffset (off+l)

      putWord64s xs = do
         off <- getPutOffset
         b   <- getPutBuffer
         bs  <- liftIO (bufferSizeIO b)
         let !l = 8*fromIntegral (length xs)
         when (l+off > bs) $
            F.fail $ "Not enough space in the target buffer ("
                   ++ show (l+off) ++ " > " ++ show bs ++ ")"

         forM_ ([off,(off+8)..] `zip` xs) $ \(boff,v) -> do
            bufferWriteWord64IO b boff v

         setPutOffset (off+l)

      preAllocateAtLeast l = do
         off <- getPutOffset
         b   <- getPutBuffer
         bs  <- liftIO (bufferSizeIO b)
         when (l+off > bs) $
            F.fail $ "Not enough space in the target buffer ("
                   ++ show (l+off) ++ " > " ++ show bs ++ ")"

      putBuffer x = do
         off <- getPutOffset
         b   <- getPutBuffer
         bs  <- liftIO (bufferSizeIO b)
         l   <- liftIO (bufferSizeIO x)
         when (l+off > bs) $
            F.fail $ "Not enough space in the target buffer ("
                   ++ show (l+off) ++ " > " ++ show bs ++ ")"

         copyBuffer x 0 b off l
         setPutOffset (off+l)
