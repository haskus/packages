{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}


-- | Vector with size in the type
module Haskus.Format.Binary.Vector
   ( Vector (..)
   , vectorBuffer
   , take
   , drop
   , index
   , fromList
   , fromFilledList
   , fromFilledListZ
   , toList
   , replicate
   , concat
   , zipWith
   )
where

import Prelude hiding ( replicate, head, last
                      , tail, init, map, length, drop, take, concat
                      , zipWith )
import System.IO.Unsafe (unsafePerformIO)

import qualified Haskus.Utils.List as List
import Haskus.Utils.Types
import Haskus.Utils.HList
import Haskus.Utils.Maybe
import Haskus.Utils.Flow
import Haskus.Format.Binary.Storable
import Haskus.Format.Binary.Ptr
import Haskus.Format.Binary.Buffer
import Haskus.Format.Binary.Bits

-- | Vector with type-checked size
data Vector (n :: Nat) a = Vector Buffer

instance (Storable a, Show a, KnownNat n) => Show (Vector n a) where
   show v = "fromList " ++ show (toList v)

-- | Return the buffer backing the vector
vectorBuffer :: Vector n a -> Buffer
vectorBuffer (Vector b) = b

-- | Offset of the i-th element in a stored vector
type family ElemOffset a i n where
   ElemOffset a i n = IfNat (i+1 <=? n)
      (i * (SizeOf a))
      (TypeError ('Text "Invalid vector index: " ':<>: 'ShowType i
                 ':$$: 'Text "Vector size: "     ':<>: 'ShowType n))

instance forall a n.
   ( KnownNat (SizeOf a * n)
   ) => StaticStorable (Vector n a) where

   type SizeOf (Vector n a)    = SizeOf a * n
   type Alignment (Vector n a) = Alignment a

   staticPeekIO ptr =
      Vector <$> bufferPackPtr (natValue @(SizeOf a * n)) (castPtr ptr)

   staticPokeIO ptr (Vector b) = bufferPoke ptr b

instance forall a n.
   ( KnownNat n
   , Storable a
   ) => Storable (Vector n a) where
   sizeOf _    = natValue @n * sizeOfT @a
   alignment _ = alignmentT @a
   peekIO ptr  = 
      Vector <$> bufferPackPtr (sizeOfT' @(Vector n a)) (castPtr ptr)

   pokeIO ptr (Vector b) = bufferPoke ptr b

-- | Yield the first n elements
take :: forall n m a.
   ( KnownNat (SizeOf a * n)
   ) => Vector (m+n) a -> Vector n a
{-# INLINE take #-}
take (Vector b) = Vector (bufferTake (natValue @(SizeOf a * n)) b)

-- | Drop the first n elements
drop :: forall n m a.
   ( KnownNat (SizeOf a * n)
   ) => Vector (m+n) a -> Vector m a
{-# INLINE drop #-}
drop (Vector b) = Vector (bufferDrop (natValue @(SizeOf a * n)) b)

-- | /O(1)/ Index safely into the vector using a type level index.
index :: forall i a n.
   ( KnownNat (ElemOffset a i n)
   , Storable a
   ) => Vector n a -> a
{-# INLINE index #-}
index (Vector b) = bufferPeekStorableAt b (natValue @(ElemOffset a i n))

-- | Convert a list into a vector if the number of elements matches
fromList :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => [a] -> Maybe (Vector n a)
{-# INLINE fromList #-}
fromList v
   | n' /= n   = Nothing
   | n' == 0   = Just $ Vector $ emptyBuffer
   | otherwise = Just $ Vector $ bufferPackStorableList v
   where
      n' = natValue' @n
      n  = fromIntegral (List.length v)

-- | Take at most n element from the list, then use z
fromFilledList :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => a -> [a] -> Vector n a
{-# INLINE fromFilledList #-}
fromFilledList z v = Vector $ bufferPackStorableList v'
   where
      v' = List.take (natValue @n) (v ++ repeat z)

-- | Take at most (n-1) element from the list, then use z
fromFilledListZ :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => a -> [a] -> Vector n a
{-# INLINE fromFilledListZ #-}
fromFilledListZ z v = fromFilledList z v'
   where
      v' = List.take (natValue @n - 1) v

-- | Convert a vector into a list
toList :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => Vector n a -> [a]
{-# INLINE toList #-}
toList (Vector b)
   | n == 0    = []
   | otherwise = fmap (bufferPeekStorableAt b . (sza*)) [0..n-1]
   where
      n   = natValue @n
      sza = sizeOfT' @a

-- | Create a vector by replicating a value
replicate :: forall a (n :: Nat) .
   ( KnownNat n
   , Storable a
   ) => a -> Vector n a
{-# INLINE replicate #-}
replicate v = fromFilledList v []


data StoreVector = StoreVector -- Store a vector at the right offset

instance forall n v a r.
   ( v ~ Vector n a
   , r ~ IO (Ptr a)
   , KnownNat n
   , KnownNat (SizeOf a)
   , StaticStorable a
   , Storable a
   ) => Apply StoreVector (v, IO (Ptr a)) r where
      apply _ (v, getP) = do
         p <- getP
         let
            vsz = natValue @n
            p'  = p `indexPtr'` (-1 * vsz * sizeOfT @a)
         poke (castPtr p') v 
         return p'

type family WholeSize fs :: Nat where
   WholeSize '[]                 = 0
   WholeSize (Vector n s ': xs)  = n + WholeSize xs

-- | Concat several vectors into a single one
concat :: forall l (n :: Nat) a .
   ( n ~ WholeSize l
   , KnownNat n
   , Storable a
   , StaticStorable a
   , HFoldr StoreVector (IO (Ptr a)) l (IO (Ptr a))
   )
   => HList l -> Vector n a
concat vs = unsafePerformIO $ do
   let sz = sizeOfT @a * natValue @n
   p <- mallocBytes (fromIntegral sz) :: IO (Ptr ())
   _ <- hFoldr StoreVector (return (castPtr p `indexPtr'` sz) :: IO (Ptr a)) vs :: IO (Ptr a)
   Vector <$> bufferUnsafePackPtr (fromIntegral sz) p


-- | Zip two vectors
zipWith ::
   ( KnownNat n
   , Storable a
   , Storable b
   , Storable c
   ) => (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWith f u v = fromJust . fromList <| List.zipWith f (toList u) (toList v)

-- | map
map ::
   ( KnownNat n
   , Storable a
   , Storable b
   ) => (a -> b) -> Vector n a -> Vector n b
map f = fromJust . fromList . fmap f . toList

instance
   ( KnownNat n
   , Storable a
   , Eq a
   )
   => Eq (Vector n a)
   where
      u == v = toList u == toList v

instance
   ( Bits a
   , FiniteBits a
   , Eq (Vector n a)
   , KnownNat n
   , Storable a
   )
   => Bits (Vector n a)
   where
      u .&. v        = zipWith (.&.) u v
      u .|. v        = zipWith (.|.) u v
      u `xor` v      = zipWith xor u v
      bitSizeMaybe _ = Just (natValue @n * finiteBitSize @a undefined)
      bitSize a      = fromJust (bitSizeMaybe a)
      isSigned _     = False
      complement u   = map complement u
      zeroBits       = fromJust (fromList (List.replicate (natValue @n) zeroBits))
      shiftL u c     = let n  = natValue @n
                           sa = finiteBitSize @a undefined
                           go _ 0 _       = []
                           go 0 k xs      = List.take k xs
                           go s k xs
                              | s >= sa   = go (s-sa) k (List.tail xs)
                              | otherwise =
                                 let (x:y:zs) = xs
                                 in ((x `shiftL` s) .|. (y `shiftR` (sa-s))) : go s (k-1) (y:zs)
                       in fromJust (fromList (go c n (toList u ++ List.repeat zeroBits)))

      shiftR u c     = let n  = natValue @n
                           sa = finiteBitSize @a undefined
                           go _ 0 _       = []
                           go 0 k xs      = List.take k (List.tail xs)
                           go s k xs
                              | s >= sa   = zeroBits : go (s-sa) (k-1) xs
                              | otherwise =
                                 let (x:y:zs) = xs
                                 in ((x `shiftL` (sa-s)) .|. (y `shiftR` s)) : go s (k-1) (y:zs)
                       in fromJust (fromList (go c n (zeroBits : toList u)))

      rotateL u c    = let n  = natValue @n
                           sa = finiteBitSize @a undefined
                           go _ 0 _       = []
                           go 0 k xs      = List.take k xs
                           go s k xs
                              | s >= sa   = go (s-sa) k (List.tail xs)
                              | otherwise =
                                 let (x:y:zs) = xs
                                 in ((x `shiftL` s) .|. (y `shiftR` (sa-s))) : go s (k-1) (y:zs)
                       in if | c == 0    -> u
                             | c <  0    -> rotateR u (abs c)
                             | otherwise -> fromJust (fromList (go c n (cycle (toList u))))

      rotateR u c    = let n  = natValue @n
                           sa = finiteBitSize @a undefined
                       in if | c == 0    -> u
                             | c <  0    -> rotateL u (abs c)
                             | otherwise -> rotateL u (n * sa - c)

      popCount = sum . fmap popCount . toList

      bit i    = let n     = natValue @n
                     sa    = finiteBitSize @a undefined
                     (f,r) = i `divMod` sa
                     xs    = List.replicate (n - f - 1) zeroBits
                              ++ [bit r]
                              ++ List.replicate f zeroBits
                 in fromJust (fromList xs)

      testBit u i = let n     = natValue @n
                        sa    = finiteBitSize @a undefined
                        (f,r) = i `divMod` sa
                    in testBit (List.head (List.drop (n - f - 1) (toList u))) r


instance
   ( FiniteBits a
   , KnownNat n
   , Eq (Vector n a)
   , Storable a
   )
   => FiniteBits (Vector n a)
   where
      finiteBitSize _ = natValue @n * finiteBitSize @a undefined
