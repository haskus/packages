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
   (
   -- * Construction
     Snoc
   , Concat
   , Replicate
   , Zip
   -- * Removal
   , RemoveAt
   , RemoveAt1
   , RemoveAtN
   , Remove
   , Nub
   , NubHead
   -- * Sublist
   , Head
   , Last
   , Tail
   , Init
   , Take
   , Drop
   -- * Insert/replace
   , InsertAt
   , ReplaceAt
   , Replace
   , ReplaceN
   , ReplaceNS
   -- * Set operations
   , Subset
   , SetEq
   , CheckMember
   , CheckMembers
   , Union
   , Complement
   , Product
   , Member
   , Members
   , CheckNub
   -- * Index operations
   , IndexOf
   , IndexesOf
   , MaybeIndexOf
   , Index
   , Elem
   , MapElem
   , Reverse
   -- * Nat list
   , Generate
   -- * Others
   , Map
   , ListMax
   , ListMin
   , Length
   , Indexes
   , MapTest
   )
where

import Haskus.Utils.Types.Bool
import Haskus.Utils.Types.Error
import Haskus.Utils.Types.Nat
import Haskus.Utils.Types.Constraint

-- | Map a type function
type family Map (f :: a -> k) (xs :: [a]) :: [k] where
   Map f '[]       = '[]
   Map f (x ': xs) = f x ': Map f xs

-- | Get the max of a list of Nats
type family ListMax (xs :: [Nat]) where
   ListMax (x ': xs) = Max' x xs

-- | Helper for Max
type family Max' (x :: Nat) (xs :: [Nat]) where
   Max' x '[]       = x
   Max' x (a ': xs) = Max' (If (x <=? a) a x) xs

-- | Get the min of a list of Nats
type family ListMin (xs :: [Nat]) where
   ListMin (x ': xs) = Min' x xs

-- | Helper for Min
type family Min' (x :: Nat) (xs :: [Nat]) where
   Min' x '[]       = x
   Min' x (a ': xs) = Min' (If (x <=? a) x a) xs

-- | Tail of a list
type family Tail (xs :: [k]) :: [k] where
   Tail (x ': xs) = xs

-- | Drop elements in a list
type family Drop (n :: Nat) (xs :: [k]) :: [k] where
   Drop 0 xs        = xs
   Drop n (x ': xs) = Drop (n-1) xs

-- | Take elements in a list
type family Take (n :: Nat) (xs :: [k]) :: [k] where
   Take 0 xs        = '[]
   Take n (x ': xs) = x ': (Take (n-1) xs)

-- | Init of a list
type family Init (xs :: [k]) ::  [k] where
   Init '[x]      = '[]
   Init (x ': xs) = x ': (Init xs)

-- | Snoc
type family Snoc (xs :: [k]) (x :: k) :: [k] where
   Snoc '[] x       = '[x]
   Snoc (y ': ys) x = y ': (Snoc ys x)

-- | Head of a list
type family Head (xs :: [k]) :: k where
   Head (x ': xs) = x

-- | Last of a list
type family Last (xs :: [k]) :: k where
   Last '[x]      = x
   Last (x ': xs) = Last xs

-- | Concat two type lists
type family Concat (xs :: [k]) (ys :: [k]) :: [k] where
   Concat '[] '[]      = '[]
   Concat '[] ys       = ys
   Concat (x ': xs) ys = x ': Concat xs ys

-- | Get list length
type family Length (xs :: [k]) :: Nat where
   Length xs = Length' 0 xs

type family Length' n (xs :: [k]) :: Nat where
   Length' n '[]       = n
   Length' n (x ': xs) = Length' (n+1) xs

-- | Replicate
type family Replicate (n :: Nat) (s :: k) :: [k] where
   Replicate n s = Replicate' s n '[]

type family Replicate' (x :: k) (n :: Nat) (xs :: [k]) :: [k] where
   Replicate' x 0 xs = xs
   Replicate' x n xs = Replicate' x (n-1) (x ': xs)

-- | Insert a list at n
type family InsertAt (n :: Nat) (l :: [k]) (l2 :: [k]) :: [k] where
   InsertAt 0 xs ys        = Concat ys xs
   InsertAt n (x ': xs) ys = x ': InsertAt (n-1) xs ys

-- | replace l[n] with l2 (folded)
type family ReplaceAt (n :: Nat) (l :: [k]) (l2 :: [k]) :: [k] where
   ReplaceAt 0 (x ': xs) ys = Concat ys xs
   ReplaceAt n (x ': xs) ys = x ': ReplaceAt (n-1) xs ys

-- | replace a type by another in l
type family Replace (t1 :: k) (t2 :: k) (l :: [k]) :: [k] where
   Replace t1 t2 '[]        = '[]
   Replace t1 t2 (t1 ': xs) = t2 ': (Replace t1 t2 xs)
   Replace t1 t2 (x ': xs)  = x ': (Replace t1 t2 xs)

-- | replace a type at offset n in l
type family ReplaceN (n :: Nat) (t :: k) (l :: [k]) :: [k] where
   ReplaceN 0 t (x ': xs)  = (t ': xs)
   ReplaceN n t (x ': xs)  = x ': ReplaceN (n-1) t xs

-- | replace types at offsets ns in l
type family ReplaceNS (ns :: [Nat]) (t :: k) (l :: [k]) :: [k] where
   ReplaceNS '[] t l       = l
   ReplaceNS (i ': is) t l = ReplaceNS is t (ReplaceN i t l)

-- | Reverse a list
type family Reverse (l :: [k]) :: [k] where
   Reverse l = Reverse' l '[]

type family Reverse' (l :: [k]) (l2 :: [k]) :: [k]  where
   Reverse' '[] l       = l
   Reverse' (x ': xs) l = Reverse' xs (x ': l)


-- | Remove a type at index
type family RemoveAt (n :: Nat) (l :: [k]) :: [k] where
   RemoveAt 0 (x ': xs) = xs
   RemoveAt n (x ': xs) = x ': RemoveAt (n-1) xs

-- | Remove a type at index (0 == don't remove)
type family RemoveAt1 (n :: Nat) (l :: [k]) :: [k]  where
   RemoveAt1 0 xs        = xs
   RemoveAt1 1 (x ': xs) = xs
   RemoveAt1 n (x ': xs) = x ': RemoveAt1 (n-1) xs

-- | Remove types at several indexes
type family RemoveAtN (ns :: [Nat]) (l :: [k]) :: [k]  where
   RemoveAtN '[] xs       = xs
   RemoveAtN (i ': is) xs = RemoveAtN is (RemoveAt i xs)

-- | Generate a list of Nat [n..m-1]
type family Generate (n :: Nat) (m :: Nat) :: [Nat] where
   Generate n n = '[]
   Generate n m = n ': Generate (n+1) m

-- | Check that a type is member of a type list
type family CheckMember (a :: k) (l :: [k]) :: Constraint where
   CheckMember a l = CheckMember' (MaybeIndexOf a l) a l

-- | Helper for CheckMember
type family CheckMember' (i :: Nat) (a :: k) (l :: [k]) :: Constraint where
   CheckMember' 0 a l = TypeError ( 'Text "`"
                              ':<>: 'ShowType a
                              ':<>: 'Text "'"
                              ':<>: 'Text " is not a member of "
                              ':<>: 'ShowType l)
   CheckMember' _ _ _ = ()


-- | Check that a list is a subset of another
type family CheckMembers (l1 :: [k]) (l2 :: [k]) :: Constraint where
   CheckMembers '[]       l1 = ()
   CheckMembers (x ': xs) l2 = CheckMembers' (MaybeIndexOf x l2) x xs (x ': xs) l2

-- | Helper for CheckMembers
type family CheckMembers' (i :: Nat) (x :: k) (xs :: [k]) (l1 :: [k]) (l2 :: [k]) :: Constraint where
   CheckMembers' 0 x _ l1 l2 = TypeError (     'ShowType l1
                                         ':$$: 'Text "is not a subset of"
                                         ':$$: 'ShowType l2
                                         ':$$: 'ShowType x
                                         ':<>: 'Text " is missing from the latter.")
   CheckMembers' _ _ '[] _ _ = ()
   CheckMembers' _ _ (x ': xs) l1 l2 = CheckMembers' (MaybeIndexOf x l2) x xs l1 l2

-- | Get list indexes
type family Indexes (l :: [k]) :: [Nat] where
   Indexes xs      = IndexesFrom 0 xs

type family IndexesFrom (n :: Nat) (xs :: [k]) :: [Nat] where
   IndexesFrom n '[]       = '[]
   IndexesFrom n (x ': xs) = n ': IndexesFrom (n+1) xs

-- | Map to 1 if type equality, 0 otherwise
type family MapTest (a :: k) (l :: [k]) :: [Nat] where
   MapTest a '[]       = '[]
   MapTest a (a ': xs) = 1 ': MapTest a xs
   MapTest a (x ': xs) = 0 ': MapTest a xs

-- | Zip two lists
type family Zip (l :: [*]) (l2 :: [*]) where
   Zip '[] xs              = '[]
   Zip xs '[]              = '[]
   Zip (x ': xs) (y ': ys) = (x,y) ': Zip xs ys

-- | Remove `a` in `l`
type family Remove (a :: k) (l :: [k]) :: [k] where
   Remove a '[]       = '[]
   Remove a (a ': as) = Remove a as
   Remove a (b ': as) = b ': Remove a as

-- | Keep only a single value of each type
type family Nub (l :: [k]) :: [k] where
   Nub xs = Reverse (Nub' xs '[])

type family Nub' (as :: [k]) (xs :: [k]) :: [k] where
   Nub' '[]       xs = xs
   Nub' (x ': as) xs = Nub' (Remove x as) (x ': xs) 

-- | Keep only a single value of the head type
type family NubHead (l :: [k]) :: [k] where
   NubHead '[]       = '[]
   NubHead (x ': xs) = x ': Remove x xs

-- | Get the first index of a type
type IndexOf (x :: k) (xs :: [k]) = IndexOf' (MaybeIndexOf x xs) x xs

-- | Get the first index of a type
type family IndexOf' (i :: Nat) (a :: k) (l :: [k]) :: Nat where
   IndexOf' 0 x l = TypeError ( 'ShowType x
                          ':<>: 'Text " not found in list:"
                          ':$$: 'Text " "
                          ':<>: 'ShowType l )
   IndexOf' i _ _ = i - 1

-- | Get all the indexes of a type
type family IndexesOf (a :: k) (l :: [k]) :: [Nat] where
   IndexesOf x xs = IndexesOf' 0 x xs

-- | Get the first index of a type
type family IndexesOf' n (a :: k) (l :: [k]) :: [Nat] where
   IndexesOf' n x '[]       = '[]
   IndexesOf' n x (x ': xs) = n ': IndexesOf' (n+1) x xs
   IndexesOf' n x (y ': xs) = IndexesOf' (n+1) x xs

-- | Get the first index (starting from 1) of a type or 0 if none
type family MaybeIndexOf (a :: k) (l :: [k]) where
   MaybeIndexOf x xs = MaybeIndexOf' 0 x xs

-- | Helper for MaybeIndexOf
type family MaybeIndexOf' (n :: Nat) (a :: k) (l :: [k]) where
   MaybeIndexOf' n x '[]       = 0
   MaybeIndexOf' n x (x ': xs) = n + 1
   MaybeIndexOf' n x (y ': xs) = MaybeIndexOf' (n+1) x xs

-- | Indexed access into the list
type Index (n :: Nat) (l :: [k]) = Index' n l l

-- | Indexed access into the list
type family Index' (n :: Nat) (l :: [k]) (l2 :: [k]) = (r :: k) where
   Index' 0 (x ': _ ) _  = x
   Index' n (_ ': xs) l2 = Index' (n-1) xs l2
   Index' n '[]       l2 = TypeError ( 'Text "Index "
                                ':<>: 'ShowType n
                                ':<>: 'Text " out of bounds for list:"
                                ':$$: 'Text " "
                                ':<>: 'ShowType l2 )

-- | List membership test
type family Elem (t :: b) (f :: b) (x :: k) (xs :: [k]) :: b where
   Elem t f x '[]       = f
   Elem t f x (x ': xs) = t
   Elem t f x (y ': xs) = Elem t f x xs

-- | MapElem t f xs ys = Map (\x -> Elem t f x ys) xs
type family MapElem (t :: b) (f :: b) (xs :: [a]) (ys :: [a]) :: [b] where
   MapElem t f '[]       ys = '[]
   MapElem t f (x ': xs) ys = Elem t f x ys ':  MapElem t f xs ys

-- | Subset test
type family Subset (t :: b) (f :: b) (xs :: [a]) (ys :: [a]) :: b where
   Subset t f '[] '[] = t
   Subset t f xs ys   = AndMany t f (MapElem t f xs ys)

-- | Set equality
type family SetEq (t :: b) (f :: b) (xs :: [a]) (ys :: [a]) :: b where
   SetEq t f xs ys = And t f (Subset t f xs ys)
                             (Subset t f ys xs)

-- | Union two lists
type family Union (xs :: [k]) (ys :: [k]) :: [k] where
   Union xs ys = Nub (Concat xs ys)

-- | Complement xs \ ys
type family Complement (xs :: [k]) (ys :: [k]) :: [k] where
   Complement xs '[]    = xs
   Complement xs (y:ys) = Complement (Remove y xs) ys

-- | Product of two lists
type family Product (xs :: [*]) (ys :: [*]) :: [*] where
   Product '[] ys    = '[]
   Product xy '[]    = '[]
   Product (x:xs) ys = Concat (Product' x ys) (Product xs ys)

type family Product' (x :: *) (ys :: [*]) :: [*] where
   Product' x '[]       = '[]
   Product' x (y ': ys) = (x,y) ': Product' x ys

--------------------------------------
-- Constraints
--------------------------------------

-- | Constraint: x member of xs
type family Member x xs :: Constraint where
   Member x xs = MemberAtIndex (IndexOf x xs) x xs
   
type MemberAtIndex i x xs =
   ( x ~ Index i xs
   , KnownNat i
   )

-- | Constraint: all the xs are members of ys
type family Members xs ys :: Constraint where
   Members '[] ys       = ()
   Members (x ': xs) ys = (Member x ys, Members xs ys)

-- | Check that a list only contain a value of each type
type CheckNub (l :: [k]) =
   ( CheckNubEx l (Nub l) ~ 'True
   )

type family CheckNubEx (l1 :: [k]) (l2 :: [k]) where
   CheckNubEx l l   = 'True
   CheckNubEx l1 l2 = TypeError
      ( 'Text "Type-list contains unallowed redundant types."
      ':$$: 'Text "Got: "      ':<>: 'ShowType l1
      ':$$: 'Text "Expected: " ':<>: 'ShowType l2
      )

