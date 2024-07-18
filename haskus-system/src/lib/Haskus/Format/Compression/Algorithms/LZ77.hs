-- | Implementation of LZ77
--
-- The idea is to use a sliding window to compress and decompress. The window w
-- contains the n last tokens you have (de)compressed. It is initialized with a
-- default element (ini :: a). The size of the window is a parameter.
--
-- You have a sequence of tokens s :: [a] to compress. You seek for the longest
-- proper prefix p of s that is present in (w ++ dropLast p).  The recursive
-- definition allows for multiple repetition of the same pattern. 
-- E.g. w = xxxxx102, s = 1021021023 -> p has length 9 and position (n-3)
--
-- The maximum length of p is parametric. p can't be equal to s, it is a
-- *proper* prefix, meaning that at least the last token of s is not in p.
-- 
-- Each compression step returns a (Code pos len c :: Code a) where pos and len
-- are respectively the position and length of the prefix in the window. "c" is
-- the token just after the prefix in s. When len is 0, only one character
-- of s is encoded. 
--
-- The final result is a sequence of (Code a). It can be later transformed into
-- a new sequence of [a] by encoding positions and lengths into "a" values
-- (e.g. imagine "a" is a byte (Word8)).
--
-- The decompression algorithm is similar. It uses the same kind of window. It
-- decompresses an input s :: [Code a] into a [a].
--
-- From the paper:
--    "A Universal Algorithm for Sequential Data Compression"
--       Ziv, Jacob; Lempel, Abraham (May 1977)
--       IEEE Transactions on Information Theory 23 (3): 337–343
module Haskus.Format.Compression.Algorithms.LZ77
   ( Code(..)
   , compress
   , decompress
   )
where

import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Data.Sequence (Seq,viewl,ViewL(..),(|>),(><))
import qualified Data.Sequence as Seq

-- | A code (prefix + single value)
data Code a = Code 
   { codePosition :: Int   -- ^ Position of the prefix in the window
   , codeLength   :: Int   -- ^ Length of the prefix
   , codeElem     :: a     -- ^ Value after the prefix
   } deriving (Show)

-- | Compress a sequence, using LZ77
--
-- `ls` is the maximal word length
-- `n` is the buffer length
-- `ini` is the value filling the buffer initially
compress :: (Eq a) => Int -> Int -> a -> [a] -> [Code a]
compress ls n ini = rec (Seq.replicate (n-ls) ini)
   where
      -- return the length and the value of the longest prefix
      prefixLen :: Eq a => [a] -> Seq a -> Int
      prefixLen = prefixLen' 0

      prefixLen' len u v = case (u, viewl v) of
         (s:ss,b:<bs) | s == b -> prefixLen' (len+1) ss (bs |> s) 
         _                     -> len

      rec _ [] = []
      rec b s  = Code pos len k : rec newb ks
         where
            -- current word (max length = ls-1) of s
            w = take (ls-1) s

            -- prefix lengths and their position [(len,pos)]
            prefixes = fmap (prefixLen w) (Seq.tails b) `Seq.zip` Seq.fromList [0..Seq.length b]

            -- longest prefix length and its position
            (len,pos) = maximumBy (comparing fst) prefixes

            -- last char and remaining sequence
            (k:ks) = drop len s

            -- new buffer
            newb = Seq.drop (len+1) b >< Seq.fromList (take (len+1) s)


-- | Decompress a sequence, using LZ77
--
-- `ls` is the maximal word length
-- `n` is the buffer length
-- `ini` is the value filling the buffer initially
decompress :: Int -> Int -> a -> [Code a] -> [a]
decompress ls n ini = rec (replicate (n-ls) ini)
   where
      rec _ [] = []
      rec b (Code pos len c:ss) = w ++ rec newb ss
         where 
            b'   = b ++ drop pos b'
            w    = take len (drop pos b') ++ [c]
            newb = drop (len+1) b ++ w
