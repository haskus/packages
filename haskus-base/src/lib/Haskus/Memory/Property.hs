-- | Memory properties
module Haskus.Memory.Property
   ( Mutability (..)
   , Heap (..)
   , Pinning (..)
   , Finalization (..)
   )
where

-- | Is the memory mutable or not?
data Mutability
   = Mutable   -- ^ Memory cells are mutable
   | Immutable -- ^ Memory cells are immutable
   deriving (Show,Eq)

-- | Allocation heap
data Heap
   = Internal -- ^ GHC heap
   | External -- ^ External heap

-- | Is the buffer pinned into memory?
data Pinning
   = Pinned    -- ^ The buffer has a fixed associated memory address
   | NotPinned -- ^ The buffer contents can be freely moved to another address
   deriving (Show,Eq)

-- | Is the memory automatically garbage collected?
data Finalization
   = Collected    -- ^ Automatically collected by the garbage-collector
   | Finalized    -- ^ Finalizers are run just before the garbage collector
                  -- collects the referencing entity (buffer, pointer...). The
                  -- memory used by the entity may be collected too (Internal
                  -- heap), explicitly freed by a finalizer or not freed at all.
   | NotFinalized -- ^ The memory is not automatically freed and we
                  -- can't attach finalizers to the buffer.
   deriving (Show,Eq)

