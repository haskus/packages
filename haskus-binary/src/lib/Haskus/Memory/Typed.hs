{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Typed memory
--
-- Pointer-like datatypes with an additional phantom type indicating their
-- memory layout
module Haskus.Memory.Typed
   ( BufferT (..)
   , PointerT (..)
   , PtrT (..)
   )
where

import Haskus.Memory.Buffer
import Haskus.Memory.Ptr
import GHC.Exts

-- | Typed pointer 
newtype PointerT (t :: k) mut fin = PointerT (Pointer mut fin)

-- | Typed buffer
newtype BufferT (t :: k) mut pin fin heap = BufferT (Buffer mut pin fin heap)

-- | Typed raw pointer
newtype PtrT (t :: k) = PtrT (Ptr ())
