{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

module Haskus.Binary.Serialize.Size
   ( GetSize (..)
   , runGetSize
   )
where

import Haskus.Binary.Serialize.Put
import Haskus.Memory.Buffer
import Control.Monad.Trans.State.Strict as S

newtype GetSize a
   = GetSize (State Word a) 
   deriving newtype (Functor, Applicative, Monad)

-- | Increment the current size
incSize :: Word -> GetSize ()
incSize x = GetSize (state (\s -> ((),s+x)))

-- | Get the total size
runGetSize :: GetSize a -> Word
runGetSize (GetSize s) = execState s 0

instance PutMonad GetSize where
   putWord8 _  = incSize 1
   putWord16 _ = incSize 2
   putWord32 _ = incSize 4
   putWord64 _ = incSize 8
   putBuffer b = incSize (bufferSize b)
