{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

-- | Infinite list
module Haskus.Utils.InfList
   ( InfList (..)
   , toList
   , repeat
   , take
   , drop
   , replicate
   , filter
   , head
   , tail
   )
where

import Prelude (Word,Foldable,Functor,Bool,(-),otherwise)

-- | An infinite list
data InfList a
   = a :> InfList a
   deriving (Functor,Foldable)

infixr 5 :>

-- | Convert to a standard list
toList :: InfList a -> [a]
toList (a :> as) = a : toList as

-- | Take for infinite list
take :: Word -> InfList a -> [a]
take 0 _         = []
take n (x :> xs) = x : take (n-1) xs

-- | Drop n elements from an infinite list
drop :: Word -> InfList a -> InfList a
drop 0 as        = as
drop n (_ :> as) = drop (n-1) as

-- | Repeat for infinite list
repeat :: a -> InfList a
repeat a = go
   where
      go = a :> go

-- | Replicate for infinite list
replicate :: Word -> a -> InfList a -> InfList a
replicate 0 _ xs = xs
replicate n a xs = a :> replicate (n-1) a xs

-- | Filter an infinite list
filter :: (a -> Bool) -> InfList a -> InfList a
filter f (a :> as)
  | f a       = a :> filter f as
  | otherwise = filter f as

-- | Head of an infinite list
head :: InfList a -> a
head (a :> _) = a

-- | Tail of an infinite list
tail :: InfList a -> InfList a
tail (_ :> as) = as
