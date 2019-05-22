{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

-- | Infinite list
module Haskus.Utils.InfList
   ( InfList (..)
   , toList
   , repeat
   , take
   , replicate
   )
where

import Prelude hiding (take,repeat,replicate)

-- | An infinite list
data InfList a
   = a :> InfList a
   deriving (Functor,Foldable)

-- | Convert to a standard list
toList :: InfList a -> [a]
toList (a :> as) = a : toList as

-- | Take for infinite list
take :: Word -> InfList a -> [a]
take 0 _         = []
take n (x :> xs) = x : take (n-1) xs

-- | Repeat for infinite list
repeat :: a -> InfList a
repeat a = go
   where
      go = a :> go

-- | Replicate for infinite list
replicate :: Word -> a -> InfList a -> InfList a
replicate 0 _ xs = xs
replicate n a xs = a :> replicate (n-1) a xs
