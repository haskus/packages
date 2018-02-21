{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

-- | Utils for type lists
module Haskus.Utils.Types.List
   ( Map
   , Max
   , Tail
   , Drop
   , Take
   , Init
   , Head
   , Snoc
   , ReplaceAt
   , Replace
   , ReplaceN
   , ReplaceNS
   , Reverse
   , RemoveAt
   , RemoveAt1
   , Concat
   , Length
   , Replicate
   , MapMaybe
   , Generate
   , IsMember
   , IsSubset
   , Indexes
   , MapTest
   , Zip
   , Filter
   , Nub
   , NubHead
   , IndexOf
   , IndexesOf
   , MaybeIndexOf
   , Index
   , Union
   , Member
   , CheckNub
   )
where

import Haskus.Utils.Types

-- | Map a type function
type family Map (f :: a -> k) (xs :: [a]) where
   Map f '[]       = '[]
   Map f (x ': xs) = f x ': Map f xs

-- | Get the max of a list of Nats
type family Max (xs :: [Nat]) where
   Max (x ': xs) = Max' x xs

-- | Helper for Max
type family Max' (x :: Nat) (xs :: [Nat]) where
   Max' x '[]       = x
   Max' x (a ': xs) = Max' (If (x <=? a) a x) xs

-- | Tail of a list
type family Tail (xs :: [*]) where
   Tail (x ': xs) = xs

-- | Drop elements in a list
type family Drop (n :: Nat) (xs :: [*]) where
   Drop 0 xs        = xs
   Drop n (x ': xs) = Drop (n-1) xs

-- | Take elements in a list
type family Take (n :: Nat) (xs :: [*]) where
   Take 0 xs        = '[]
   Take n (x ': xs) = x ': (Take (n-1) xs)

-- | Init of a list
type family Init (xs :: [*]) where
   Init '[x]      = '[]
   Init (x ': xs) = x ': (Init xs)

-- | Snoc
type family Snoc (xs :: [*]) x where
   Snoc '[] x       = '[x]
   Snoc (y ': ys) x = y ': (Snoc ys x)

-- | Head of a list
type family Head (xs :: [*]) where
   Head (x ': xs) = x

-- | Concat two type lists
type family Concat (xs :: [*]) (ys :: [*]) where
   Concat '[] '[]      = '[]
   Concat '[] ys       = ys
   Concat (x ': xs) ys = x ': Concat xs ys

-- | Get list length
type family Length xs where
   Length xs = Length' 0 xs

type family Length' n xs where
   Length' n '[]       = n
   Length' n (x ': xs) = Length' (n+1) xs

-- | Replicate
type family Replicate n s where
   Replicate n s = Replicate' s n '[]

type family Replicate' x n xs where
   Replicate' x 0 xs = xs
   Replicate' x n xs = Replicate' x (n-1) (x ': xs)

-- | replace l[n] with l2 (folded)
type family ReplaceAt (n :: Nat) l l2 where
   ReplaceAt 0 (x ': xs) ys = Concat ys xs
   ReplaceAt n (x ': xs) ys = x ': ReplaceAt (n-1) xs ys

-- | replace a type by another in l
type family Replace t1 t2 l where
   Replace t1 t2 '[]        = '[]
   Replace t1 t2 (t1 ': xs) = t2 ': (Replace t1 t2 xs)
   Replace t1 t2 (x ': xs)  = x ': (Replace t1 t2 xs)

-- | replace a type at offset n in l
type family ReplaceN n t l where
   ReplaceN 0 t (x ': xs)  = (t ': xs)
   ReplaceN n t (x ': xs)  = x ': ReplaceN (n-1) t xs

-- | replace types at offsets ns in l
type family ReplaceNS ns t l where
   ReplaceNS '[] t l       = l
   ReplaceNS (i ': is) t l = ReplaceNS is t (ReplaceN i t l)

-- | Reverse a list
type family Reverse (l :: [*]) where
   Reverse l = Reverse' l '[]

type family Reverse' (l :: [*]) (l2 :: [*]) where
   Reverse' '[] l       = l
   Reverse' (x ': xs) l = Reverse' xs (x ': l)


-- | Remove a type at index
type family RemoveAt (n :: Nat) l where
   RemoveAt 0 (x ': xs) = xs
   RemoveAt n (x ': xs) = x ': RemoveAt (n-1) xs

-- | Remove a type at index (0 == don't remove)
type family RemoveAt1 (n :: Nat) l where
   RemoveAt1 0 xs        = xs
   RemoveAt1 1 (x ': xs) = xs
   RemoveAt1 n (x ': xs) = x ': RemoveAt1 (n-1) xs

-- | Apply Maybe to all the elements of the list
type family MapMaybe l where
   MapMaybe '[]       = '[]
   MapMaybe (x ': xs) = Maybe x ': MapMaybe xs

-- | Generate a list of Nat [n..m-1]
type family Generate (n :: Nat) (m :: Nat) :: [Nat] where
   Generate n n = '[]
   Generate n m = n ': Generate (n+1) m

-- | Check that a type is member of a type list
type family IsMember a (l :: [*]) :: Bool where
   IsMember a l = IsMember' l a l

-- | Check that a type is member of a type list
type family IsMember' (i :: [*]) a (l :: [*]) :: Bool where
   IsMember' i a (a ': l) = 'True
   IsMember' i a (b ': l) = IsMember' i a l
   IsMember' i a '[]      = TypeError ( 'Text "`"
                                   ':<>: 'ShowType a
                                   ':<>: 'Text "'"
                                   ':<>: 'Text " is not a member of "
                                   ':<>: 'ShowType i)


-- | Check that a list is a subset of another
type family IsSubset l1 l2 :: Bool where
   IsSubset l1 l1 = 'True
   IsSubset l1 l2 = IsSubset' l2 l1 l2

-- | Helper for IsSubset
type family IsSubset' i l1 l2 :: Bool where
   IsSubset' i '[] l2 = 'True
   IsSubset' i l1 '[] = TypeError (     'ShowType l1
                                   ':$$: 'Text "is not a subset of"
                                   ':$$: 'ShowType i)
   IsSubset' i (x ': xs) (x ': ys) = IsSubset' i xs i
   IsSubset' i (x ': xs) (y ': ys) = IsSubset' i (x ': xs) ys

-- | Get list indexes
type family Indexes (l :: [*]) where
   Indexes xs      = IndexesFrom 0 xs

type family IndexesFrom (n :: Nat) (xs :: [*]) where
   IndexesFrom n '[]       = '[]
   IndexesFrom n (x ': xs) = Proxy n ': IndexesFrom (n+1) xs

-- | Map to 1 if type equality, 0 otherwise
type family MapTest a (l :: [*]) where
   MapTest a '[]       = '[]
   MapTest a (a ': xs) = Proxy 1 ': MapTest a xs
   MapTest a (x ': xs) = Proxy 0 ': MapTest a xs

-- | Zip two lists
type family Zip (l :: [*]) (l2 :: [*]) where
   Zip '[] xs              = '[]
   Zip xs '[]              = '[]
   Zip (x ': xs) (y ': ys) = (x,y) ': Zip xs ys

-- | Remove `a` in `l`
type family Filter a (l :: [*]) where
   Filter a '[]       = '[]
   Filter a (a ': as) = Filter a as
   Filter a (b ': as) = b ': Filter a as

-- | Keep only a single value of each type
type family Nub (l :: [*]) where
   Nub xs = Reverse (Nub' xs '[])

type family Nub' as xs where
   Nub' '[]       xs = xs
   Nub' (x ': as) xs = Nub' (Filter x as) (x ': xs) 

-- | Keep only a single value of the head type
type family NubHead (l :: [*]) where
   NubHead '[]       = '[]
   NubHead (x ': xs) = x ': Filter x xs

-- | Get the first index of a type
type family IndexOf a (l :: [*]) :: Nat where
   IndexOf x xs = IndexOf' x xs xs

-- | Get the first index of a type
type family IndexOf' a (l :: [*]) (l2 :: [*]) :: Nat where
   IndexOf' x (x ': xs) l2 = 0
   IndexOf' y (x ': xs) l2 = 1 + IndexOf' y xs l2
   IndexOf' y '[]       l2 = TypeError ( 'Text "`"
                                    ':<>: 'ShowType y
                                    ':<>: 'Text "'"
                                    ':<>: 'Text " is not a member of "
                                    ':<>: 'ShowType l2)

-- | Get all the indexes of a type
type family IndexesOf a (l :: [*]) :: [Nat] where
   IndexesOf x xs = IndexesOf' 0 x xs

-- | Get the first index of a type
type family IndexesOf' n a (l :: [*]) :: [Nat] where
   IndexesOf' n x '[]       = '[]
   IndexesOf' n x (x ': xs) = n ': IndexesOf' (n+1) x xs
   IndexesOf' n x (y ': xs) = IndexesOf' (n+1) x xs

-- | Get the first index (starting from 1) of a type or 0 if none
type family MaybeIndexOf a (l :: [*]) where
   MaybeIndexOf x xs = MaybeIndexOf' 0 x xs

-- | Helper for MaybeIndexOf
type family MaybeIndexOf' (n :: Nat) a (l :: [*]) where
   MaybeIndexOf' n x '[]       = 0
   MaybeIndexOf' n x (x ': xs) = 1 + n
   MaybeIndexOf' n x (y ': xs) = MaybeIndexOf' (n+1) x xs

-- | Indexed access into the list
type family Index (n :: Nat) (l :: [*]) where
   Index 0 (x ': xs) = x
   Index n (x ': xs) = Index (n-1) xs

-- | Union two lists
type family Union (xs :: [*]) (ys :: [*]) where
   Union xs ys = Nub (Concat xs ys)

--------------------------------------
-- Constraints
--------------------------------------

-- | Constraint: x member of xs
type Member x xs =
   ( IsMember x xs ~ 'True
   , x ~ Index (IndexOf x xs) xs
   , KnownNat (IndexOf x xs)
   )

-- | Check that a list only contain a value of each type
type CheckNub (l :: [*]) =
   ( CheckNubEx l (Nub l) ~ 'True
   )

type family CheckNubEx (l1 :: [*]) (l2 :: [*]) where
   CheckNubEx l l   = 'True
   CheckNubEx l1 l2 = TypeError
      ( 'Text "Type-list contains unallowed redundant types."
      ':$$: 'Text "Got: "      ':<>: 'ShowType l1
      ':$$: 'Text "Expected: " ':<>: 'ShowType l2
      )

