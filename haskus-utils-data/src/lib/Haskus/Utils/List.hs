module Haskus.Utils.List
   ( checkLength
   , (++)
   , replicate
   , drop
   , length
   , take
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
   )
where

import Prelude hiding (replicate, length, drop, take)

import qualified Data.List as L
import qualified Data.List.Extra as L

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
