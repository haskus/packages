{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

-- | Pointers
--
-- A pointer is a number: an offset into a memory. This is the `Addr#` type.
--
-- We want the type-system to help us avoid errors when we use pointers, hence
-- we decorate them with phantom types describing the memory layout at the
-- pointed address. This is the `Ptr a` data type that wraps an `Addr#`.
--
-- We often want to associate finalizers to pointers, i.e., actions to be run
-- when the pointer is collected by the GC. These actions take the pointer as a
-- parameter. This is the `ForeignPtr a` data type.
--
-- A `ForeignPtr a` cannot be manipulated like a number because somehow we need
-- to keep the pointer value that will be passed to the finalizers. Moreover we
-- don't want finalizers to be executed too early, so we can't easily create a
-- new ForeignPtr from another (it would require a way to disable the existing
-- finalizers of a ForeignPtr, which would in turn open a whole can of worms).
-- Hence we use the `FinalizedPtr a` pointer type, which has an additional
-- offset field.
module Haskus.Memory.Ptr
   ( Pointer (..)
   , AnyPointer (..)
   , RawPtr
   , FinPtr
   , PtrI
   , PtrM
   , PtrIF
   , PtrMF
   , isNullPtr
   , nullPtrI
   , nullPtrM
   , indexPtr
   , distancePtr
   , withPtr
   , withFinalizedPtr
   , allocFinalizedPtr
   , allocPtr
   , freePtr

   -- * Function pointer
   , P.FunPtr
   , P.nullFunPtr
   , P.castPtrToFunPtr
   , P.castFunPtrToPtr
   -- * Pointer as a Word
   , P.WordPtr
   , P.wordPtrToPtr
   , P.ptrToWordPtr
   )
where

import qualified Foreign.Ptr               as P
import qualified Foreign.Marshal.Alloc     as P
import qualified Foreign.ForeignPtr        as FP
import qualified Foreign.ForeignPtr.Unsafe as FP

import Haskus.Memory.Property
import Haskus.Utils.Monad
import Haskus.Utils.Flow

-- | A pointer in memory
data Pointer (mut :: Mutability) (fin :: Finalization) where
   PtrI  :: {-# UNPACK #-} !RawPtr                        -> PtrI
   PtrM  :: {-# UNPACK #-} !RawPtr                        -> PtrM
   PtrIF :: {-# UNPACK #-} !FinPtr -> {-# UNPACK #-} !Int -> PtrIF
   PtrMF :: {-# UNPACK #-} !FinPtr -> {-# UNPACK #-} !Int -> PtrMF

type RawPtr = P.Ptr ()
type FinPtr = FP.ForeignPtr ()

type PtrI   = Pointer 'Immutable 'NotFinalized
type PtrM   = Pointer 'Mutable   'NotFinalized
type PtrIF  = Pointer 'Immutable 'Finalized
type PtrMF  = Pointer 'Mutable   'Finalized

-- | Wrapper containing any kind of buffer
newtype AnyPointer = AnyPointer (forall mut fin . Pointer mut fin)

instance Show (Pointer mut fin) where
   show = \case
      PtrI p    -> show p
      PtrM p    -> show p
      PtrIF p o -> show (fToR p `P.plusPtr` o)
      PtrMF p o -> show (fToR p `P.plusPtr` o)

-- | Unsafe Finalized to Raw pointer
fToR :: FinPtr -> RawPtr
fToR = FP.unsafeForeignPtrToPtr

-- | Test if a pointer is Null
{-# SPECIALIZE INLINE isNullPtr :: PtrI  -> Bool #-}
{-# SPECIALIZE INLINE isNullPtr :: PtrM  -> Bool #-}
{-# SPECIALIZE INLINE isNullPtr :: PtrIF -> Bool #-}
{-# SPECIALIZE INLINE isNullPtr :: PtrMF -> Bool #-}
isNullPtr :: Pointer mut fin -> Bool
isNullPtr = \case
   PtrI  p   -> p == P.nullPtr
   PtrM  p   -> p == P.nullPtr
   PtrIF p 0 -> fToR p == P.nullPtr
   PtrIF _ _ -> False
   PtrMF p 0 -> fToR p == P.nullPtr
   PtrMF _ _ -> False

-- | Null pointer
nullPtrI :: PtrI
nullPtrI = PtrI P.nullPtr

-- | Null pointer
nullPtrM :: PtrM
nullPtrM = PtrM P.nullPtr

-- | Index a pointer
{-# SPECIALIZE INLINE indexPtr :: PtrI  -> Int -> PtrI  #-}
{-# SPECIALIZE INLINE indexPtr :: PtrM  -> Int -> PtrM  #-}
{-# SPECIALIZE INLINE indexPtr :: PtrIF -> Int -> PtrIF #-}
{-# SPECIALIZE INLINE indexPtr :: PtrMF -> Int -> PtrMF #-}
indexPtr :: Pointer mut fin -> Int -> Pointer mut fin
indexPtr ptr i = case ptr of
   PtrI  p   -> PtrI (p `P.plusPtr` i)
   PtrM  p   -> PtrM (p `P.plusPtr` i)
   PtrIF p o -> PtrIF p (o+i)
   PtrMF p o -> PtrMF p (o+i)

-- | Distance between two pointers
{-# SPECIALIZE INLINE distancePtr :: PtrI  -> PtrI -> Int  #-}
{-# SPECIALIZE INLINE distancePtr :: PtrM  -> PtrM -> Int  #-}
{-# SPECIALIZE INLINE distancePtr :: PtrI  -> PtrM -> Int  #-}
{-# SPECIALIZE INLINE distancePtr :: PtrM  -> PtrI -> Int  #-}
distancePtr :: Pointer mut0 fin0 -> Pointer mut1 fin1 -> Int
distancePtr p1 p2 = P.minusPtr p1' p2' + o2 - o1
   where
      dec :: Pointer mut fin -> (RawPtr,Int)
      dec = \case
         PtrI p    -> (p,0)
         PtrM p    -> (p,0)
         PtrIF p o -> (fToR p,o)
         PtrMF p o -> (fToR p,o)
      (p1',o1) = dec p1
      (p2',o2) = dec p2

-- | Use a finalized pointer as a non finalized pointer
{-# INLINABLE withFinalizedPtr #-}
withFinalizedPtr :: (MonadInIO m) => Pointer mut 'Finalized -> (Pointer mut 'NotFinalized -> m b) -> m b
withFinalizedPtr ptr f = case ptr of
   PtrIF p o -> liftWith (FP.withForeignPtr p) <| \r ->
                  f (PtrI (r `P.plusPtr` o))
   PtrMF p o -> liftWith (FP.withForeignPtr p) <| \r ->
                  f (PtrM (r `P.plusPtr` o))

-- | Use a pointer (finalized or not) as a non finalized pointer
{-# INLINABLE withPtr #-}
withPtr :: (MonadInIO m) => Pointer mut fin -> (Pointer mut 'NotFinalized -> m b) -> m b
withPtr ptr f = case ptr of
   PtrI _    -> f ptr
   PtrM _    -> f ptr
   PtrIF p o -> liftWith (FP.withForeignPtr p) <| \r ->
                  f (PtrI (r `P.plusPtr` o))
   PtrMF p o -> liftWith (FP.withForeignPtr p) <| \r ->
                  f (PtrM (r `P.plusPtr` o))

-- | Alloc mutable finalized memory
allocFinalizedPtr :: MonadIO m => Word -> m PtrMF
allocFinalizedPtr = liftIO . fmap (`PtrMF` 0) . FP.mallocForeignPtrBytes . fromIntegral

-- | Alloc mutable non-finalized memory
allocPtr :: MonadIO m => Word -> m PtrM
allocPtr = liftIO . fmap PtrM . P.mallocBytes . fromIntegral


-- | Free a non-finalized memory
{-# SPECIALIZE INLINE freePtr :: MonadIO m => PtrI -> m () #-}
{-# SPECIALIZE INLINE freePtr :: MonadIO m => PtrM -> m () #-}
freePtr :: MonadIO m => Pointer mut 'NotFinalized -> m ()
freePtr = \case
   PtrI p -> liftIO (P.free p)
   PtrM p -> liftIO (P.free p)
