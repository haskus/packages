{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Open sum type
module Haskus.Utils.Variant
   ( Variant
   , V
   , variantIndex
   -- * Patterns
   , pattern V
   , pattern VMaybe
   , (:<)
   , (:<?)
   -- * Operations by index
   , toVariantAt
   , toVariantHead
   , toVariantTail
   , fromVariantAt
   , popVariantAt
   , popVariantHead
   , updateVariantAt
   , foldMapVariantAt
   , foldMapVariantAtM
   -- * Operations by type
   , toVariant
   , Member
   , Filter
   , Popable
   , MaybePopable
   , popVariant
   , popVariantMaybe
   , fromVariant
   , fromVariantMaybe
   , fromVariantFirst
   , updateVariantFirst
   , updateVariantFirstM
   , MappableVariant
   , mapVariant
   , foldMapVariantFirst
   , foldMapVariantFirstM
   , foldMapVariant
   -- * Generic operations with type classes
   , AlterVariant
   , TraverseVariant
   , NoConstraint
   , alterVariant
   , traverseVariant
   , traverseVariant_
   -- * Conversions between variants
   , appendVariant
   , prependVariant
   , Liftable
   , liftVariant
   , nubVariant
   , productVariant
   , Flattenable
   , FlattenVariant
   , flattenVariant
   , ExtractMonad
   , joinVariant
   -- * Conversions to/from other data types
   , variantToValue
   , variantFromValue
   , variantToEither
   , variantFromEither
   , variantToHList
   , variantToTuple
   -- ** Continuations
   , ContVariant (..)
   -- ** Internals
   , pattern V'
   , liftVariant'
   , fromVariant'
   , popVariant'
   , toVariant'
   , LiftVariant
   , PopVariant
   )
where

import Unsafe.Coerce
import GHC.Exts (Any,Constraint)

import Haskus.Utils.Monad
import Haskus.Utils.Types
import Haskus.Utils.Tuple
import Haskus.Utils.HList
import Haskus.Utils.ContFlow
import Haskus.Utils.Types.List

-- | A variant contains a value whose type is at the given position in the type
-- list
data Variant (l :: [*]) = Variant {-# UNPACK #-} !Word Any

type V = Variant

-- Make GHC consider `l` as a representational parameter to make coercions
-- between Variant values unsafe
type role Variant representational

-- | Pattern synonym for Variant
--
-- Usage: case v of
--          V (x :: Int)    -> ...
--          V (x :: String) -> ...
pattern V :: forall c cs. Popable c cs => c -> Variant cs
pattern V x <- (fromVariant -> Just x)
   where
      V x = toVariant x

-- | Silent pattern synonym for Variant
--
-- Usage: case v of
--          V (x :: Int)    -> ...
--          V (x :: String) -> ...
pattern V' :: forall c cs.
   ( Member' c cs
   , PopVariant c cs
   ) => c -> Variant cs
pattern V' x <- (fromVariant' -> Just x)
   where
      V' x = toVariant' x

-- | Statically unchecked matching on a Variant
pattern VMaybe :: forall c cs. (MaybePopable c cs) => c -> Variant cs
pattern VMaybe x <- (fromVariantMaybe -> Just x)

instance Eq (Variant '[]) where
   (==) = error "Empty variant"

instance
   ( Eq (Variant xs)
   , Eq x
   ) => Eq (Variant (x ': xs))
   where
      {-# INLINE (==) #-}
      (==) v1@(Variant t1 _) v2@(Variant t2 _)
         | t1 /= t2  = False
         | otherwise = case (popVariantHead v1, popVariantHead v2) of
            (Right a, Right b) -> a == b
            (Left as, Left bs) -> as == bs
            _                  -> False

instance Ord (Variant '[]) where
   compare = error "Empty variant"

instance
   ( Ord (Variant xs)
   , Ord x
   ) => Ord (Variant (x ': xs))
   where
      compare v1 v2 = case (popVariantHead v1, popVariantHead v2) of
         (Right a, Right b) -> compare a b
         (Left as, Left bs) -> compare as bs
         (Right _, Left _)  -> LT
         (Left _, Right _)  -> GT

instance Show (Variant '[]) where
   show = error "Empty variant"

instance
   ( Show (Variant xs)
   , Show x
   ) => Show (Variant (x ': xs))
   where
      show v = case popVariantHead v of
         Right x -> show x
         Left xs -> show xs

-----------------------------------------------------------
-- Operations by index
-----------------------------------------------------------

-- | Get Variant index
variantIndex :: Variant a -> Word
variantIndex (Variant n _) = n

-- | Set the value with the given indexed type
toVariantAt :: forall (n :: Nat) (l :: [*]).
   ( KnownNat n
   ) => Index n l -> Variant l
{-# INLINE toVariantAt #-}
toVariantAt a = Variant (natValue' @n) (unsafeCoerce a)

-- | Set the first value
toVariantHead :: forall x xs. x -> Variant (x ': xs)
{-# INLINE toVariantHead #-}
toVariantHead a = Variant 0 (unsafeCoerce a)

-- | Set the tail
toVariantTail :: forall x xs. Variant xs -> Variant (x ': xs)
{-# INLINE toVariantTail #-}
toVariantTail (Variant t a) = Variant (t+1) a

-- | Get the value if it has the indexed type
fromVariantAt :: forall (n :: Nat) (l :: [*]).
   ( KnownNat n
   ) => Variant l -> Maybe (Index n l)
{-# INLINE fromVariantAt #-}
fromVariantAt (Variant t a) = do
   guard (t == natValue' @n)
   return (unsafeCoerce a) -- we know it is the effective type

-- | Pop a variant value by index, return either the value or the remaining
-- variant
popVariantAt :: forall (n :: Nat) l. 
   ( KnownNat n
   ) => Variant l -> Either (Variant (RemoveAt n l)) (Index n l)
{-# INLINE popVariantAt #-}
popVariantAt v@(Variant t a) = case fromVariantAt @n v of
   Just x  -> Right x
   Nothing -> Left $ if t > natValue' @n
      then Variant (t-1) a
      else Variant t a

-- | Pop the head of a variant value
popVariantHead :: forall x xs. Variant (x ': xs) -> Either (Variant xs) x
{-# INLINE popVariantHead #-}
popVariantHead v@(Variant t a) = case fromVariantAt @0 v of
   Just x  -> Right x
   Nothing -> Left $ Variant (t-1) a

-- | Update a variant value
updateVariantAt :: forall (n :: Nat) a b l.
   ( KnownNat n
   , a ~ Index n l
   ) => (a -> b) -> Variant l -> Variant (ReplaceN n b l)
{-# INLINE updateVariantAt #-}
updateVariantAt f v@(Variant t a) =
   case fromVariantAt @n v of
      Nothing -> Variant t a
      Just x  -> Variant t (unsafeCoerce (f x))

-----------------------------------------------------------
-- Operations by type
-----------------------------------------------------------

-- | Put a value into a Variant
--
-- Use the first matching type index.
toVariant :: forall a l.
   ( Member a l
   ) => a -> Variant l
{-# INLINE toVariant #-}
toVariant = toVariantAt @(IndexOf a l)

-- | Put a value into a Variant (silent)
--
-- Use the first matching type index.
toVariant' :: forall a l.
   ( Member' a l
   ) => a -> Variant l
{-# INLINE toVariant' #-}
toVariant' = toVariantAt @(IndexOf a l)

class PopVariant a xs where
   -- | Remove a type from a variant
   popVariant' :: Variant xs -> Either (Variant (Filter a xs)) a

instance PopVariant a '[] where
   popVariant' _ = undefined

instance forall a xs n xs' y ys.
      ( PopVariant a xs'
      , n ~ MaybeIndexOf a xs
      , xs' ~ RemoveAt1 n xs
      , Filter a xs' ~ Filter a xs
      , KnownNat n
      , xs ~ (y ': ys)
      ) => PopVariant a (y ': ys)
   where
      {-# INLINE popVariant' #-}
      popVariant' (Variant t a)
         = case natValue' @n of
            0             -> Left (Variant t a) -- no 'a' left in xs
            n | n-1 == t  -> Right (unsafeCoerce a)
              | n-1 < t   -> popVariant' @a @xs' (Variant (t-1) a)
              | otherwise -> Left (Variant t a)

-- | a is popable in xs
type Popable a xs =
   ( Member a xs
   , PopVariant a xs
   )

-- | a may be popable in xs
type MaybePopable a xs =
   ( PopVariant a xs
   )

type (:<) a xs  = Popable a xs
type (:<?) a xs = MaybePopable a xs


-- | Extract a type from a variant. Return either the value of this type or the
-- remaining variant
popVariant :: forall a xs.
   ( Popable a xs
   ) => Variant xs -> Either (Variant (Filter a xs)) a
popVariant v = popVariant' @a v

-- | Extract a type from a variant. Return either the value of this type or the
-- remaining variant
popVariantMaybe :: forall a xs.
   ( MaybePopable a xs
   ) => Variant xs -> Either (Variant (Filter a xs)) a
popVariantMaybe v = popVariant' @a v

-- | Pick the first matching type of a Variant
--
-- fromVariantFirst @A (Variant 2 undefined :: Variant '[A,B,A]) == Nothing
fromVariantFirst :: forall a l.
   ( Member a l
   ) => Variant l -> Maybe a
{-# INLINE fromVariantFirst #-}
fromVariantFirst = fromVariantAt @(IndexOf a l)

-- | Try to a get a value of a given type from a Variant
fromVariant :: forall a xs.
   ( Popable a xs
   ) => Variant xs -> Maybe a
{-# INLINE fromVariant #-}
fromVariant v = case popVariant v of
   Right a -> Just a
   Left _  -> Nothing

-- | Try to a get a value of a given type from a Variant (silent)
fromVariant' :: forall a xs.
   ( PopVariant a xs
   ) => Variant xs -> Maybe a
{-# INLINE fromVariant' #-}
fromVariant' v = case popVariant' v of
   Right a -> Just a
   Left _  -> Nothing

-- | Try to a get a value of a given type from a Variant that may not even
-- support the given type.
fromVariantMaybe :: forall a xs.
   ( MaybePopable a xs
   ) => Variant xs -> Maybe a
{-# INLINE fromVariantMaybe #-}
fromVariantMaybe v = case popVariantMaybe v of
   Right a -> Just a
   Left _  -> Nothing

-- | Update a variant value
updateVariantFirst :: forall a b n l.
   ( Member a l
   , n ~ IndexOf a l
   ) => (a -> b) -> Variant l -> Variant (ReplaceN n b l)
{-# INLINE updateVariantFirst #-}
updateVariantFirst f v = updateVariantAt @n f v

-- | Monadic update of the first matching variant value
updateVariantFirstM :: forall (n :: Nat) l l2 m .
   (KnownNat n, Monad m)
   => (Index n l -> m (Index n l2)) -> Variant l -> m (Variant l2)
{-# INLINE updateVariantFirstM #-}
updateVariantFirstM f v@(Variant t a) =
   case fromVariantAt @n v of
      Nothing -> return (Variant t a)
      Just x  -> Variant t <$> unsafeCoerce (f x)

class MapVariant a b cs (is :: [Nat]) where
   mapVariant' :: (a -> b) -> Variant cs -> Variant (ReplaceNS is b cs)

instance MapVariant a b '[] is where
   {-# INLINE mapVariant' #-}
   mapVariant' = undefined

instance MapVariant a b cs '[] where
   {-# INLINE mapVariant' #-}
   mapVariant' _ v = v

instance forall a b cs is i.
   ( MapVariant a b (ReplaceN i b cs) is
   , a ~ Index i cs
   , KnownNat i
   ) => MapVariant a b cs (i ': is) where
   {-# INLINE mapVariant' #-}
   mapVariant' f v = mapVariant' @a @b @(ReplaceN i b cs) @is f (updateVariantAt @i f v)

type MappableVariant a b cs =
   ( MapVariant a b cs (IndexesOf a cs)
   )

-- | Map the matching types of a variant
mapVariant :: forall a b cs.
   ( MappableVariant a b cs
   ) => (a -> b) -> Variant cs -> Variant (ReplaceNS (IndexesOf a cs) b cs)
mapVariant = mapVariant' @a @b @cs @(IndexesOf a cs)


-- | Update a variant value with a variant and fold the result
foldMapVariantAt :: forall (n :: Nat) l l2 .
   ( KnownNat n
   , KnownNat (Length l2)
   ) => (Index n l -> Variant l2) -> Variant l -> Variant (ReplaceAt n l l2)
foldMapVariantAt f v@(Variant t a) =
   case fromVariantAt @n v of
      Nothing ->
         -- we need to adapt the tag if new valid tags (from l2) are added before
         if t < n
            then Variant t a
            else Variant (t+nl2-1) a

      Just x  -> case f x of
         Variant t2 a2 -> Variant (t2+n) a2
   where
      n   = natValue' @n
      nl2 = natValue' @(Length l2)

-- | Update a variant value with a variant and fold the result
foldMapVariantAtM :: forall (n :: Nat) m l l2.
   ( KnownNat n
   , KnownNat (Length l2)
   , Monad m
   ) => (Index n l -> m (Variant l2)) -> Variant l -> m (Variant (ReplaceAt n l l2))
foldMapVariantAtM f v@(Variant t a) =
   case fromVariantAt @n v of
      Nothing ->
         -- we need to adapt the tag if new valid tags (from l2) are added before
         return $ if t < n
            then Variant t a
            else Variant (t+nl2-1) a

      Just x  -> do
         y <- f x
         case y of
            Variant t2 a2 -> return (Variant (t2+n) a2)
   where
      n   = natValue' @n
      nl2 = natValue' @(Length l2)

-- | Update a variant value with a variant and fold the result
foldMapVariantFirst :: forall a (n :: Nat) l l2 .
   ( KnownNat n
   , KnownNat (Length l2)
   , n ~ IndexOf a l
   , a ~ Index n l
   ) => (a -> Variant l2) -> Variant l -> Variant (ReplaceAt n l l2)
foldMapVariantFirst f v = foldMapVariantAt @n f v

-- | Update a variant value with a variant and fold the result
foldMapVariantFirstM :: forall a (n :: Nat) l l2 m.
   ( KnownNat n
   , KnownNat (Length l2)
   , n ~ IndexOf a l
   , a ~ Index n l
   , Monad m
   ) => (a -> m (V l2)) -> V l -> m (V (ReplaceAt n l l2))
foldMapVariantFirstM f v = foldMapVariantAtM @n f v



-- | Update a variant value with a variant and fold the result
foldMapVariant :: forall a cs ds i.
   ( i ~ IndexOf a cs
   , Popable a cs
   ) => (a -> V ds) -> V cs -> V (InsertAt i (Filter a cs) ds)
foldMapVariant f v = case popVariant v of
   Right a -> case f a of
      Variant t x -> Variant (i + t) x
   Left (Variant t x)
      | t < i     -> Variant t x
      | otherwise -> Variant (i+t) x
   where
      i = natValue' @i




-----------------------------------------------------------
-- Generic operations with type classes
-----------------------------------------------------------

class AlterVariant c (b :: [*]) where
   alterVariant' :: Alter c -> Word -> Any -> Any

instance AlterVariant c '[] where
   {-# INLINE alterVariant' #-}
   alterVariant' = undefined

instance
   ( AlterVariant c xs
   , c x
   ) => AlterVariant c (x ': xs)
   where
      {-# INLINE alterVariant' #-}
      alterVariant' m@(Alter f) t v =
         case t of
            0 -> unsafeCoerce (f (unsafeCoerce v :: x))
            n -> alterVariant' @c @xs m (n-1) v

-- | Wrap a function and its constraints
data Alter (c :: * -> Constraint) = Alter (forall a. c a => a -> a)

-- | Wrap a function and its constraints
data AlterM (c :: * -> Constraint) m = AlterM (forall a. (Monad m, c a) => a -> m a)

-- | Useful to specify a "* -> Constraint" function returning no constraint
class NoConstraint a
instance NoConstraint a

class TraverseVariant c (b :: [*]) m where
   traverseVariant' :: AlterM c m -> Word -> Any -> m Any

instance TraverseVariant c '[] m where
   {-# INLINE traverseVariant' #-}
   traverseVariant' = undefined

instance
   ( TraverseVariant c xs m
   , c x
   , Monad m
   ) => TraverseVariant c (x ': xs) m
   where
      {-# INLINE traverseVariant' #-}
      traverseVariant' m@(AlterM f) t v =
         case t of
            0 -> unsafeCoerce <$> f (unsafeCoerce v :: x)
            n -> traverseVariant' @c @xs m (n-1) v


-- | Alter a variant. You need to specify the constraints required by the
-- modifying function.
--
-- Usage:
--    alterVariant @NoConstraint id         v
--    alterVariant @Resizable    (resize 4) v
--
--    class (Ord a, Num a) => OrdNum a
--    instance (Ord a, Num a) => OrdNum a
--
{-# INLINE alterVariant #-}
alterVariant :: forall c (a :: [*]).
   ( AlterVariant c a
   ) => (forall x. c x => x -> x) -> Variant a  -> Variant a
alterVariant f (Variant t a) = 
   Variant t (alterVariant' @c @a (Alter @c f) t a)

-- | Traverse a variant. You need to specify the constraints required by the
-- modifying function.
{-# INLINE traverseVariant #-}
traverseVariant :: forall c (a :: [*]) m.
   ( TraverseVariant c a m
   , Monad m
   ) => (forall x. c x => x -> m x) -> Variant a  -> m (Variant a)
traverseVariant f (Variant t a) = 
   Variant t <$> traverseVariant' @c @a (AlterM @c @m f) t a

-- | Traverse a variant. You need to specify the constraints required by the
-- modifying function.
traverseVariant_ :: forall c (a :: [*]) m.
   ( TraverseVariant c a m
   , Monad m
   ) => (forall x. c x => x -> m ()) -> Variant a  -> m ()
traverseVariant_ f v = void (traverseVariant @c @a f' v)
   where
      f' :: forall x. c x => x -> m x
      f' x = f x >> return x

-----------------------------------------------------------
-- Conversions between variants
-----------------------------------------------------------

-- | Extend a variant by appending other possible values
appendVariant :: forall (ys :: [*]) (xs :: [*]). Variant xs -> Variant (Concat xs ys)
{-# INLINE appendVariant #-}
appendVariant (Variant t a) = Variant t a

-- | Extend a variant by prepending other possible values
prependVariant :: forall (ys :: [*]) (xs :: [*]).
   ( KnownNat (Length ys)
   ) => Variant xs -> Variant (Concat ys xs)
{-# INLINE prependVariant #-}
prependVariant (Variant t a) = Variant (n+t) a
   where
      n = natValue' @(Length ys)

-- | xs is liftable in ys
type Liftable xs ys =
   ( IsSubset xs ys ~ 'True
   , LiftVariant xs ys
   )

class LiftVariant xs ys where
   liftVariant' :: Variant xs -> Variant ys

instance LiftVariant '[] ys where
   liftVariant' = error "Lifting empty variant"

instance forall xs ys x.
      ( LiftVariant xs ys
      , KnownNat (IndexOf x ys)
      ) => LiftVariant (x ': xs) ys
   where
      {-# INLINE liftVariant' #-}
      liftVariant' (Variant t a)
         | t == 0    = Variant (natValue' @(IndexOf x ys)) a
         | otherwise = liftVariant' @xs (Variant (t-1) a)


-- | Lift a variant into another
--
-- Set values to the first matching type
liftVariant :: forall xs ys.
   ( Liftable xs ys
   ) => Variant xs -> Variant ys
{-# INLINE liftVariant #-}
liftVariant = liftVariant'

-- | Nub the type list
nubVariant :: (Liftable xs (Nub xs)) => V xs -> V (Nub xs)
nubVariant = liftVariant

-- | Product of two variants
productVariant :: forall xs ys.
   ( KnownNat (Length ys)
   ) => Variant xs -> Variant ys -> Variant (Product xs ys)
productVariant (Variant n1 a1) (Variant n2 a2)
   = Variant (n1 * natValue @(Length ys) + n2) (unsafeCoerce (a1,a2))

type family FlattenVariant (xs :: [*]) :: [*] where
   FlattenVariant '[]             = '[]
   FlattenVariant (Variant xs:ys) = Concat xs (FlattenVariant ys)
   FlattenVariant (y:ys)          = y ': FlattenVariant ys

class Flattenable a rs where
   toFlattenVariant :: Word -> a -> rs

instance Flattenable (Variant '[]) rs where
   {-# INLINE toFlattenVariant #-}
   toFlattenVariant _ _ = undefined

instance forall xs ys rs.
   ( Flattenable (Variant ys) (Variant rs)
   , KnownNat (Length xs)
   ) => Flattenable (Variant (Variant xs ': ys)) (Variant rs)
   where
   {-# INLINE toFlattenVariant #-}
   toFlattenVariant i v = case popVariantHead v of
      Right (Variant n a) -> Variant (i+n) a
      Left vys            -> toFlattenVariant (i+natValue @(Length xs)) vys

-- | Flatten variants in a variant
flattenVariant :: forall xs.
   ( Flattenable (Variant xs) (Variant (FlattenVariant xs))
   ) => Variant xs -> Variant (FlattenVariant xs)
flattenVariant v = toFlattenVariant 0 v

type family ExtractMonad m f where
   ExtractMonad m '[m x]      = '[x]
   ExtractMonad m (m x ': xs) = x ': ExtractMonad m xs

-- | Join on a variant
--
-- Transform a variant of applicatives as follow:
--    V'[m a, m b, m c] ===> m (V'[a,b,c])
--
joinVariant :: forall m xs ys.
   ( Applicative m
   , ys ~ ExtractMonad m xs
   ) => Variant xs -> m (Variant ys)
joinVariant (Variant t act) = Variant t <$> (unsafeCoerce act :: m Any)

-----------------------------------------------------------
-- Conversions to other data types
-----------------------------------------------------------

-- | Retrieve a single value
variantToValue :: Variant '[a] -> a
{-# INLINE variantToValue #-}
variantToValue (Variant _ a) = unsafeCoerce a

-- | Create a variant from a single value
variantFromValue :: a -> Variant '[a]
{-# INLINE variantFromValue #-}
variantFromValue a = Variant 0 (unsafeCoerce a)


-- | Convert a variant of two values in a Either
variantToEither :: forall a b. Variant '[a,b] -> Either b a
variantToEither (Variant 0 a) = Right (unsafeCoerce a)
variantToEither (Variant _ a) = Left (unsafeCoerce a)

class VariantToHList xs where
   -- | Convert a variant into a HList of Maybes
   variantToHList :: Variant xs -> HList (Map Maybe xs)

instance VariantToHList '[] where
   variantToHList _ = HNil

instance
   ( VariantToHList xs
   ) => VariantToHList (x ': xs)
   where
      variantToHList v@(Variant t a) =
            fromVariantAt @0 v `HCons` variantToHList v'
         where
            v' :: Variant xs
            v' = Variant (t-1) a

-- | Get variant possible values in a tuple of Maybe types
variantToTuple :: forall l t.
   ( VariantToHList l
   , HTuple' (Map Maybe l) t
   ) => Variant l -> t
variantToTuple = hToTuple' . variantToHList


-- | Lift an Either into a Variant (reversed order by convention)
variantFromEither :: Either a b -> Variant '[b,a]
{-# INLINE variantFromEither #-}
variantFromEither (Left a)  = toVariantAt @1 a
variantFromEither (Right b) = toVariantAt @0 b


class ContVariant xs where
   -- | Convert a variant into a multi-continuation
   variantToCont :: Variant xs -> ContFlow xs r

   -- | Convert a variant into a multi-continuation
   variantToContM :: Monad m => m (Variant xs) -> ContFlow xs (m r)

   -- | Convert a multi-continuation into a Variant
   contToVariant :: ContFlow xs (Variant xs) -> Variant xs

   -- | Convert a multi-continuation into a Variant
   contToVariantM :: Monad m => ContFlow xs (m (Variant xs)) -> m (Variant xs)

instance ContVariant '[a] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant _ a) = ContFlow $ \(Single f) ->
      f (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(Single f) -> do
      Variant _ a <- act
      f (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      Single (toVariantAt @0)

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      Single (return . toVariantAt @0)

instance ContVariant '[a,b] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         _ -> f2 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         _ -> f2 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( toVariantAt @0
      , toVariantAt @1
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . toVariantAt @0
      , return . toVariantAt @1
      )

instance ContVariant '[a,b,c] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         _ -> f3 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         _ -> f3 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( toVariantAt @0
      , toVariantAt @1
      , toVariantAt @2
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . toVariantAt @0
      , return . toVariantAt @1
      , return . toVariantAt @2
      )

instance ContVariant '[a,b,c,d] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         _ -> f4 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         _ -> f4 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( toVariantAt @0
      , toVariantAt @1
      , toVariantAt @2
      , toVariantAt @3
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . toVariantAt @0
      , return . toVariantAt @1
      , return . toVariantAt @2
      , return . toVariantAt @3
      )

instance ContVariant '[a,b,c,d,e] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         _ -> f5 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         _ -> f5 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( toVariantAt @0
      , toVariantAt @1
      , toVariantAt @2
      , toVariantAt @3
      , toVariantAt @4
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . toVariantAt @0
      , return . toVariantAt @1
      , return . toVariantAt @2
      , return . toVariantAt @3
      , return . toVariantAt @4
      )

instance ContVariant '[a,b,c,d,e,f] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5,f6) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         _ -> f6 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5,f6) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         _ -> f6 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( toVariantAt @0
      , toVariantAt @1
      , toVariantAt @2
      , toVariantAt @3
      , toVariantAt @4
      , toVariantAt @5
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . toVariantAt @0
      , return . toVariantAt @1
      , return . toVariantAt @2
      , return . toVariantAt @3
      , return . toVariantAt @4
      , return . toVariantAt @5
      )

instance ContVariant '[a,b,c,d,e,f,g] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         5 -> f6 (unsafeCoerce a)
         _ -> f7 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         5 -> f6 (unsafeCoerce a)
         _ -> f7 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( toVariantAt @0
      , toVariantAt @1
      , toVariantAt @2
      , toVariantAt @3
      , toVariantAt @4
      , toVariantAt @5
      , toVariantAt @6
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . toVariantAt @0
      , return . toVariantAt @1
      , return . toVariantAt @2
      , return . toVariantAt @3
      , return . toVariantAt @4
      , return . toVariantAt @5
      , return . toVariantAt @6
      )

instance ContVariant '[a,b,c,d,e,f,g,h] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         5 -> f6 (unsafeCoerce a)
         6 -> f7 (unsafeCoerce a)
         _ -> f8 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         5 -> f6 (unsafeCoerce a)
         6 -> f7 (unsafeCoerce a)
         _ -> f8 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( toVariantAt @0
      , toVariantAt @1
      , toVariantAt @2
      , toVariantAt @3
      , toVariantAt @4
      , toVariantAt @5
      , toVariantAt @6
      , toVariantAt @7
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . toVariantAt @0
      , return . toVariantAt @1
      , return . toVariantAt @2
      , return . toVariantAt @3
      , return . toVariantAt @4
      , return . toVariantAt @5
      , return . toVariantAt @6
      , return . toVariantAt @7
      )

instance ContVariant '[a,b,c,d,e,f,g,h,i] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9) ->
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         5 -> f6 (unsafeCoerce a)
         6 -> f7 (unsafeCoerce a)
         7 -> f8 (unsafeCoerce a)
         _ -> f9 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9) -> do
      Variant t a <- act
      case t of
         0 -> f1 (unsafeCoerce a)
         1 -> f2 (unsafeCoerce a)
         2 -> f3 (unsafeCoerce a)
         3 -> f4 (unsafeCoerce a)
         4 -> f5 (unsafeCoerce a)
         5 -> f6 (unsafeCoerce a)
         6 -> f7 (unsafeCoerce a)
         7 -> f8 (unsafeCoerce a)
         _ -> f9 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( toVariantAt @0
      , toVariantAt @1
      , toVariantAt @2
      , toVariantAt @3
      , toVariantAt @4
      , toVariantAt @5
      , toVariantAt @6
      , toVariantAt @7
      , toVariantAt @8
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . toVariantAt @0
      , return . toVariantAt @1
      , return . toVariantAt @2
      , return . toVariantAt @3
      , return . toVariantAt @4
      , return . toVariantAt @5
      , return . toVariantAt @6
      , return . toVariantAt @7
      , return . toVariantAt @8
      )

instance ContVariant '[a,b,c,d,e,f,g,h,i,j] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10) ->
      case t of
         0 -> f1  (unsafeCoerce a)
         1 -> f2  (unsafeCoerce a)
         2 -> f3  (unsafeCoerce a)
         3 -> f4  (unsafeCoerce a)
         4 -> f5  (unsafeCoerce a)
         5 -> f6  (unsafeCoerce a)
         6 -> f7  (unsafeCoerce a)
         7 -> f8  (unsafeCoerce a)
         8 -> f9  (unsafeCoerce a)
         _ -> f10 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10) -> do
      Variant t a <- act
      case t of
         0 -> f1  (unsafeCoerce a)
         1 -> f2  (unsafeCoerce a)
         2 -> f3  (unsafeCoerce a)
         3 -> f4  (unsafeCoerce a)
         4 -> f5  (unsafeCoerce a)
         5 -> f6  (unsafeCoerce a)
         6 -> f7  (unsafeCoerce a)
         7 -> f8  (unsafeCoerce a)
         8 -> f9  (unsafeCoerce a)
         _ -> f10 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( toVariantAt @0
      , toVariantAt @1
      , toVariantAt @2
      , toVariantAt @3
      , toVariantAt @4
      , toVariantAt @5
      , toVariantAt @6
      , toVariantAt @7
      , toVariantAt @8
      , toVariantAt @9
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . toVariantAt @0
      , return . toVariantAt @1
      , return . toVariantAt @2
      , return . toVariantAt @3
      , return . toVariantAt @4
      , return . toVariantAt @5
      , return . toVariantAt @6
      , return . toVariantAt @7
      , return . toVariantAt @8
      , return . toVariantAt @9
      )

instance ContVariant '[a,b,c,d,e,f,g,h,i,j,k] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11) ->
      case t of
         0 -> f1  (unsafeCoerce a)
         1 -> f2  (unsafeCoerce a)
         2 -> f3  (unsafeCoerce a)
         3 -> f4  (unsafeCoerce a)
         4 -> f5  (unsafeCoerce a)
         5 -> f6  (unsafeCoerce a)
         6 -> f7  (unsafeCoerce a)
         7 -> f8  (unsafeCoerce a)
         8 -> f9  (unsafeCoerce a)
         9 -> f10 (unsafeCoerce a)
         _ -> f11 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11) -> do
      Variant t a <- act
      case t of
         0 -> f1  (unsafeCoerce a)
         1 -> f2  (unsafeCoerce a)
         2 -> f3  (unsafeCoerce a)
         3 -> f4  (unsafeCoerce a)
         4 -> f5  (unsafeCoerce a)
         5 -> f6  (unsafeCoerce a)
         6 -> f7  (unsafeCoerce a)
         7 -> f8  (unsafeCoerce a)
         8 -> f9  (unsafeCoerce a)
         9 -> f10 (unsafeCoerce a)
         _ -> f11 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( toVariantAt @0
      , toVariantAt @1
      , toVariantAt @2
      , toVariantAt @3
      , toVariantAt @4
      , toVariantAt @5
      , toVariantAt @6
      , toVariantAt @7
      , toVariantAt @8
      , toVariantAt @9
      , toVariantAt @10
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . toVariantAt @0
      , return . toVariantAt @1
      , return . toVariantAt @2
      , return . toVariantAt @3
      , return . toVariantAt @4
      , return . toVariantAt @5
      , return . toVariantAt @6
      , return . toVariantAt @7
      , return . toVariantAt @8
      , return . toVariantAt @9
      , return . toVariantAt @10
      )

instance ContVariant '[a,b,c,d,e,f,g,h,i,j,k,l] where
   {-# INLINE variantToCont #-}
   variantToCont (Variant t a) = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12) ->
      case t of
         0  -> f1  (unsafeCoerce a)
         1  -> f2  (unsafeCoerce a)
         2  -> f3  (unsafeCoerce a)
         3  -> f4  (unsafeCoerce a)
         4  -> f5  (unsafeCoerce a)
         5  -> f6  (unsafeCoerce a)
         6  -> f7  (unsafeCoerce a)
         7  -> f8  (unsafeCoerce a)
         8  -> f9  (unsafeCoerce a)
         9  -> f10 (unsafeCoerce a)
         10 -> f11 (unsafeCoerce a)
         _  -> f12 (unsafeCoerce a)

   {-# INLINE variantToContM #-}
   variantToContM act = ContFlow $ \(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12) -> do
      Variant t a <- act
      case t of
         0  -> f1  (unsafeCoerce a)
         1  -> f2  (unsafeCoerce a)
         2  -> f3  (unsafeCoerce a)
         3  -> f4  (unsafeCoerce a)
         4  -> f5  (unsafeCoerce a)
         5  -> f6  (unsafeCoerce a)
         6  -> f7  (unsafeCoerce a)
         7  -> f8  (unsafeCoerce a)
         8  -> f9  (unsafeCoerce a)
         9  -> f10 (unsafeCoerce a)
         10 -> f11 (unsafeCoerce a)
         _  -> f12 (unsafeCoerce a)

   {-# INLINE contToVariant #-}
   contToVariant c = c >::>
      ( toVariantAt @0
      , toVariantAt @1
      , toVariantAt @2
      , toVariantAt @3
      , toVariantAt @4
      , toVariantAt @5
      , toVariantAt @6
      , toVariantAt @7
      , toVariantAt @8
      , toVariantAt @9
      , toVariantAt @10
      , toVariantAt @11
      )

   {-# INLINE contToVariantM #-}
   contToVariantM c = c >::>
      ( return . toVariantAt @0
      , return . toVariantAt @1
      , return . toVariantAt @2
      , return . toVariantAt @3
      , return . toVariantAt @4
      , return . toVariantAt @5
      , return . toVariantAt @6
      , return . toVariantAt @7
      , return . toVariantAt @8
      , return . toVariantAt @9
      , return . toVariantAt @10
      , return . toVariantAt @11
      )
