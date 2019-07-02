{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | List utils
module Haskus.Utils.List
   ( at
   , unsafeAt
   , checkLength
   , (++)
   , replicate
   , drop
   , length
   , take
   , chunksOf
   , pick1
   , enumList
   , zipLeftWith
   , zipRightWith
   , L.partition
   , L.nub
   , L.sort
   , L.intersperse
   , L.foldl'
   , L.head
   , L.tail
   , L.zipWith
   , L.repeat
   , L.nubOn
   , L.nubBy
   , L.sortOn
   , L.sortBy
   , L.split
   , L.splitOn
   , L.groupOn
   , L.groupBy
   , L.transpose
   , (L.\\)
   , L.intersect
   , L.find
   , L.zip3
   , L.zip4
   , L.zip5
   , L.zip6
   , L.zip7
   , L.stripPrefix
   , L.isPrefixOf
   , L.deleteBy
   , L.isSuffixOf
   )
where

import Prelude hiding (replicate, length, drop, take)

import qualified Data.List as L
import qualified Data.List.Extra as L

-- | Safely index into a list
--
-- >>> [0,1,2,3] `at` 10
-- Nothing
--
-- >>> [0,1,2,3] `at` 2
-- Just 2
at :: [a] -> Word -> Maybe a
{-# INLINABLE at #-}
at = go
   where
      go []       _ = Nothing
      go (x:_xs)  0 = Just x
      go (_x:xs) !n = go xs (n-1)

-- | Unsafe `a`
--
-- >>> [0,1,2,3] `unsafeAt` 2
-- 2
unsafeAt :: [a] -> Word -> a
{-# INLINABLE unsafeAt #-}
unsafeAt vs k = go vs k
   where
      go []       _ = error ("Unsafe list index too large: " ++ show k)
      go (x:_xs)  0 = x
      go (_x:xs) !n = go xs (n-1)

-- | Check that a list has the given length (support infinite lists)
checkLength :: Word -> [a] -> Bool
checkLength 0 []     = True
checkLength 0 _      = False
checkLength _ []     = False
checkLength i (_:xs) = checkLength (i-1) xs

-- | Replicate
replicate :: Word -> a -> [a]
replicate n a = L.replicate (fromIntegral n) a

-- | Take
take :: Word -> [a] -> [a]
take n = L.take (fromIntegral n)

-- | Length
length :: Foldable t => t a -> Word
length = fromIntegral . L.length

-- | Drop
drop :: Word -> [a] -> [a]
drop n = L.drop (fromIntegral n)

-- | Split a list into chunks of a given size. The last chunk may contain fewer
-- than n elements.
chunksOf :: Word -> [a] -> [[a]]
chunksOf n = L.chunksOf (fromIntegral n)


-- | Pick each element and return the element and the rest of the list
--
-- >>> pick1 [1,2,3,4]
-- [(1,[2,3,4]),(2,[1,3,4]),(3,[1,2,4]),(4,[1,2,3])]
pick1 :: [a] -> [(a,[a])]
pick1 = go []
   where
      go _  []     = []
      go ys (x:xs) = (x,reverse ys++xs) : go (x:ys) xs

-- | Get members of a bounded enum in a list
--
-- >>> :set -XTypeApplications
-- >>> data Letters = A | B | C | D deriving (Bounded,Enum,Show)
-- >>> enumList @Letters
-- [A,B,C,D]
enumList :: forall a. (Bounded a,Enum a) => [a]
enumList = enumFrom minBound

-- | Zip left with something extracted from each value
--
-- >>> zipLeftWith odd [0..5]
-- [(False,0),(True,1),(False,2),(True,3),(False,4),(True,5)]
zipLeftWith :: (a -> b) -> [a] -> [(b,a)]
zipLeftWith f xs = fmap f xs `zip` xs

-- | Zip right with something extracted from each value
--
-- >>> zipRightWith odd [0..5]
-- [(0,False),(1,True),(2,False),(3,True),(4,False),(5,True)]
zipRightWith :: (a -> b) -> [a] -> [(a,b)]
zipRightWith f xs = xs `zip` fmap f xs
