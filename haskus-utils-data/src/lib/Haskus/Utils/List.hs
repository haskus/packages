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
   , nubOn
   , L.nubBy
   , L.sortOn
   , L.sortBy
   , groupOn
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
   , L.elem
   , L.notElem
   -- * Split
   , splitAt
   , split
   , splitOn
   , breakOn
   )
where

import Prelude hiding (replicate, length, drop, take,splitAt)

import Data.Bifunctor
import Data.Function (on)
import qualified Data.List as L

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

-- | Apply some operation repeatedly, producing an element of output
--   and the remainder of the list.
repeatedly :: ([a] -> (b, [a])) -> [a] -> [b]
repeatedly _ [] = []
repeatedly f as = b : repeatedly f as'
    where (b, as') = f as

-- | Split a list into chunks of a given size. The last chunk may contain fewer
-- than n elements.
--
-- >>> chunksOf 3 "my test"
-- ["my ","tes","t"]
--
-- >>> chunksOf 3 "mytest"
-- ["myt","est"]
--
-- >>> chunksOf 8 ""
-- []
--
-- >> chunksOf 0 "test"
-- undefined
chunksOf :: Word -> [a] -> [[a]]
chunksOf i xs = repeatedly (splitAt i) xs

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


-- | A version of 'nub' where the equality is done on some extracted value.
--   @nubOn f@ is equivalent to @nubBy ((==) `on` f)@, but has the
--   performance advantage of only evaluating @f@ once for each element in the
--   input list.
nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = map snd . L.nubBy ((==) `on` fst) . map (\x -> let y = f x in y `seq` (y, x))


-- | A version of 'group' where the equality is done on some extracted value.
groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = L.groupBy ((==) `on2` f)
    -- redefine on so we avoid duplicate computation for most values.
    where (.*.) `on2` g = \x -> let fx = g x in \y -> fx .*. g y

---------------------------------------
-- Split
---------------------------------------

-- | 'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
-- length @n@ and second element is the remainder of the list:
--
-- > splitAt 6 "Hello World!" == ("Hello ","World!")
-- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
-- > splitAt 1 [1,2,3] == ([1],[2,3])
-- > splitAt 3 [1,2,3] == ([1,2,3],[])
-- > splitAt 4 [1,2,3] == ([1,2,3],[])
-- > splitAt 0 [1,2,3] == ([],[1,2,3])
-- > splitAt (-1) [1,2,3] == ([],[1,2,3])
--
-- It is equivalent to @('take' n xs, 'drop' n xs)@ when @n@ is not @_|_@
-- (@splitAt _|_ xs = _|_@).
splitAt :: Integral n => n -> [a] -> ([a],[a])
splitAt n xs = L.genericSplitAt n xs

-- | Break a list into pieces separated by the first
-- list argument, consuming the delimiter. An empty delimiter is
-- invalid, and will cause an error to be raised.
--
-- > splitOn "\r\n" "a\r\nb\r\nd\r\ne" == ["a","b","d","e"]
-- > splitOn "aaa"  "aaaXaaaXaaaXaaa"  == ["","X","X","X",""]
-- > splitOn "x"    "x"                == ["",""]
-- > splitOn "x"    ""                 == [""]
-- > \s x -> s /= "" ==> intercalate s (splitOn s x) == x
-- > \c x -> splitOn [c] x                           == split (==c) x
splitOn :: (Eq a) => [a] -> [a] -> [[a]]
splitOn [] _ = error "splitOn, needle may not be empty"
splitOn _ [] = [[]]
splitOn needle haystack = a : if null b then [] else splitOn needle $ drop (length needle) b
    where (a,b) = breakOn needle haystack


-- | Splits a list into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.
--
-- > split (== 'a') "aabbaca" == ["","","bb","c",""]
-- > split (== 'a') ""        == [""]
-- > split (== ':') "::xyz:abc::123::" == ["","","xyz","abc","","123","",""]
-- > split (== ',') "my,list,here" == ["my","list","here"]
split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = [[]]
split f (x:xs) | f x                   = [] : split f xs
split f (x:xs) | ~(y:ys) <- split f xs = (x:y) : ys

-- | Find the first instance of @needle@ in @haystack@.
-- The first element of the returned tuple
-- is the prefix of @haystack@ before @needle@ is matched.  The second
-- is the remainder of @haystack@, starting with the match.
-- If you want the remainder /without/ the match, use 'stripInfix'.
--
-- > breakOn "::" "a::b::c" == ("a", "::b::c")
-- > breakOn "/" "foobar"   == ("foobar", "")
-- > \needle haystack -> let (prefix,match) = breakOn needle haystack in prefix ++ match == haystack
breakOn :: Eq a => [a] -> [a] -> ([a], [a])
breakOn needle haystack
   | needle `L.isPrefixOf` haystack = ([], haystack)
breakOn _      []                   = ([], [])
breakOn needle (x:xs)               = first (x:) $ breakOn needle xs
