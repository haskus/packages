{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

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
   ( PtrLike (..)
   , indexPtr'
   -- * Pointer
   , Ptr (..)
   , free
   -- * Finalized pointer
   , FinalizedPtr (..)
   , withFinalizedPtr
   -- * Foreign pointer
   , ForeignPtr
   , withForeignPtr
   , mallocForeignPtrBytes
   , nullForeignPtr
   -- * Function pointer
   , Ptr.FunPtr
   , Ptr.nullFunPtr
   , Ptr.castPtrToFunPtr
   , Ptr.castFunPtrToPtr
   -- * Pointer as a Word
   , Ptr.WordPtr
   , Ptr.wordPtrToPtr
   , Ptr.ptrToWordPtr
   )
where

import qualified Foreign.Ptr               as Ptr
import qualified Foreign.Marshal.Alloc     as Ptr
import qualified Foreign.ForeignPtr        as FP
import qualified Foreign.ForeignPtr.Unsafe as FP
-- we import GHC.Ptr instead of Foreign.Ptr to have access to Ptr constructors
import GHC.Ptr (Ptr (..))
import Foreign.ForeignPtr (ForeignPtr)
import Data.Coerce
import System.IO.Unsafe

import Haskus.Format.Binary.Layout
import Haskus.Utils.Types
import Haskus.Utils.Monad


-- | A finalized pointer
--
-- We use an offset because we can't modify the pointer directly (it is
-- passed to the foreign pointer destructors)
data FinalizedPtr l = FinalizedPtr {-# UNPACK #-} !(ForeignPtr l)
                                   {-# UNPACK #-} !Word  -- offset

type role FinalizedPtr phantom

instance Show (FinalizedPtr l) where
   show (FinalizedPtr fp o) = show (FP.unsafeForeignPtrToPtr fp 
                                    `indexPtr` fromIntegral o)

-- | Null foreign pointer
nullForeignPtr :: ForeignPtr a
{-# NOINLINE nullForeignPtr #-}
nullForeignPtr = unsafePerformIO $ FP.newForeignPtr_ nullPtr

-- | Null finalized pointer
nullFinalizedPtr :: FinalizedPtr a
nullFinalizedPtr = FinalizedPtr nullForeignPtr 0

-- | Use a finalized pointer
withFinalizedPtr :: FinalizedPtr a -> (Ptr a -> IO b) -> IO b
{-# INLINABLE withFinalizedPtr #-}
withFinalizedPtr (FinalizedPtr fp o) f =
   FP.withForeignPtr fp (f . (`indexPtr` fromIntegral o))

-- | Pointer operations
class PtrLike (p :: * -> *) where
   -- | Cast a pointer from one type to another
   castPtr :: p a -> p b

   -- | Null pointer (offset is 0)
   nullPtr :: forall a. p a

   -- | Advance a pointer by the given amount of bytes (may be negative)
   indexPtr :: p a -> Int -> p a

   -- | Distance between two pointers in bytes (p2 - p1)
   ptrDistance :: p a -> p b -> Int

   -- | Use the pointer
   withPtr :: p a -> (Ptr a -> IO b) -> IO b

   -- | Malloc the given number of bytes
   mallocBytes :: MonadIO m => Word -> m (p a)

   -- | Add offset to the given layout field
   (-->) :: forall path l.
      ( KnownNat (LPathOffset path l)
      ) => p l -> path -> p (LPathType path l)
   {-# INLINABLE (-->) #-}
   (-->) p _ = castPtr (p `indexPtr` natValue @(LPathOffset path l))

-- | Generalized version of 'indexPtr'
indexPtr' :: Integral b => Ptr a -> b -> Ptr a
indexPtr' p a = indexPtr p (fromIntegral a)


instance PtrLike Ptr where
   {-# INLINABLE castPtr #-}
   castPtr = coerce

   {-# INLINABLE nullPtr #-}
   nullPtr = Ptr.nullPtr

   {-# INLINABLE indexPtr #-}
   indexPtr = Ptr.plusPtr

   {-# INLINABLE ptrDistance #-}
   ptrDistance = Ptr.minusPtr

   {-# INLINABLE withPtr #-}
   withPtr p f = f p

   {-# INLINABLE mallocBytes #-}
   mallocBytes = liftIO . Ptr.mallocBytes . fromIntegral


instance PtrLike FinalizedPtr where
   {-# INLINABLE castPtr #-}
   castPtr = coerce

   {-# INLINABLE nullPtr #-}
   nullPtr = nullFinalizedPtr

   {-# INLINABLE indexPtr #-}
   indexPtr (FinalizedPtr fp o) n
      | n >= 0    = FinalizedPtr fp (o+fromIntegral n)
      | otherwise = FinalizedPtr fp (o-fromIntegral (abs n))

   {-# INLINABLE ptrDistance #-}
   ptrDistance (FinalizedPtr fp1 o1) (FinalizedPtr fp2 o2)
      | o2 > o1   = d + fromIntegral (o2 - o1)
      | otherwise = d - fromIntegral (o1 - o2)
      where
         d = ptrDistance (FP.unsafeForeignPtrToPtr fp1)
                         (FP.unsafeForeignPtrToPtr fp2)

   {-# INLINABLE withPtr #-}
   withPtr = withFinalizedPtr

   {-# INLINABLE mallocBytes #-}
   mallocBytes n = do
      fp <- mallocForeignPtrBytes (fromIntegral n)
      return (FinalizedPtr fp 0)

-- | Malloc a foreign pointer
mallocForeignPtrBytes :: MonadIO m => Word -> m (ForeignPtr a)
mallocForeignPtrBytes = liftIO . FP.mallocForeignPtrBytes . fromIntegral

-- | Use a foreign pointer
withForeignPtr :: (MonadInIO m) => ForeignPtr a -> (Ptr a -> m b) -> m b
withForeignPtr p = liftWith (FP.withForeignPtr p)

-- | Free a malloced memory
free :: MonadIO m => Ptr a -> m ()
free = liftIO . Ptr.free
