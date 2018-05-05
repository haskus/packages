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
{-# LANGUAGE BangPatterns #-}


-- | Vector with size in the type
module Haskus.Format.Binary.Vector
   ( Vector (..)
   , vectorBuffer
   , vectorReverse
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

-- | Reverse a vector
vectorReverse :: (KnownNat n, Storable a) => Vector n a -> Vector n a
vectorReverse = fromJust . fromList . reverse . toList

-- | Offset of the i-th element in a stored vector
type family ElemOffset a i n where
   ElemOffset a i n = Assert (i+1 <=? n)
      (i * (SizeOf a))
      (('Text "Invalid vector index: " ':<>: 'ShowType i
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
   ( KnownNat n
   , Bitwise a
   , Storable a
   ) => Bitwise (Vector n a)
   where
      u .&. v        = zipWith (.&.) u v
      u .|. v        = zipWith (.|.) u v
      u `xor` v      = zipWith xor u v
      complement u   = map complement u


instance
   ( KnownNat (BitSize a)
   , FiniteBits a
   , KnownNat n
   , Storable a
   ) => FiniteBits (Vector n a)
   where
      type BitSize (Vector n a) = n * BitSize a
      zeroBits = fromJust (fromList (List.replicate (natValue @n) zeroBits))
      oneBits  = fromJust (fromList (List.replicate (natValue @n) oneBits))
      countLeadingZeros = go 0 . toList
         where
            go !n []     = n
            go !n (x:xs) = let c = countLeadingZeros x
                           in if c == natValue @(BitSize a)
                                 then go (n+c) xs
                                 else n+c

      countTrailingZeros = go 0 . reverse . toList
         where
            go !n []     = n
            go !n (x:xs) = let c = countTrailingZeros x
                           in if c == natValue @(BitSize a)
                                 then go (n+c) xs
                                 else n+c

instance
   ( Storable a
   , ShiftableBits a
   , Bitwise a
   , FiniteBits a
   , KnownNat (BitSize a)
   , KnownNat (n * BitSize a)
   , KnownNat n
   ) => ShiftableBits (Vector n a)
   where
      shiftL u c = uncheckedShiftL u (c `mod` natValue @(BitSize (Vector n a)))
      shiftR u c = uncheckedShiftR u (c `mod` natValue @(BitSize (Vector n a)))

      uncheckedShiftL u c =
         let n  = natValue @n
             sa = natValue @(BitSize a)
             go _ 0 _       = []
             go 0 k xs      = List.take k xs
             go s k xs
                | s >= sa   = go (s-sa) k (List.tail xs)
                | otherwise =
                   let (x:y:zs) = xs
                   in ((x `shiftL` s) .|. (y `shiftR` (sa-s))) : go s (k-1) (y:zs)
         in fromJust (fromList (go c n (toList u ++ List.repeat zeroBits)))

      uncheckedShiftR u c  =
         let n  = natValue @n
             sa = natValue @(BitSize a)
             go _ 0 _       = []
             go 0 k xs      = List.take k (List.tail xs)
             go s k xs
                | s >= sa   = zeroBits : go (s-sa) (k-1) xs
                | otherwise =
                   let (x:y:zs) = xs
                   in ((x `shiftL` (sa-s)) .|. (y `shiftR` s)) : go s (k-1) (y:zs)
         in fromJust (fromList (go c n (zeroBits : toList u)))


instance
   ( Storable a
   , IndexableBits a
   , FiniteBits a
   , KnownNat (BitSize a)
   , KnownNat n
   , Bitwise a
   ) => IndexableBits (Vector n a) where

      popCount = sum . fmap popCount . toList

      bit i    = let n     = natValue @n
                     sa    = natValue @(BitSize a)
                     (f,r) = i `divMod` sa
                     toRep = fromIntegral (n - f - 1)
                     xs    = List.replicate toRep zeroBits
                              ++ [bit r]
                              ++ List.replicate (fromIntegral f) zeroBits
                 in fromJust <| fromList <| if i >= n * sa
                     then List.replicate (fromIntegral n) zeroBits
                     else xs

      testBit u i = let n      = natValue @n
                        sa     = natValue @(BitSize a)
                        (f,r)  = i `divMod` sa
                        toDrop = fromIntegral (n - f - 1)
                    in if i >= n * sa
                        then False
                        else testBit (List.head (List.drop toDrop (toList u))) r


instance
   ( Storable a
   , Bits a
   , KnownNat n
   , KnownNat (n * BitSize a)
   ) => RotatableBits (Vector n a)
