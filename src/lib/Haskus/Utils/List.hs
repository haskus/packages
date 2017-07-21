module Haskus.Utils.List
   ( checkLength
   , module Data.List
   , module Data.List.Extra
   )
where

import Data.List
import Data.List.Extra

-- | Check that a list has the given length (support infinite lists)
checkLength :: Word -> [a] -> Bool
checkLength 0 []     = True
checkLength 0 _      = False
checkLength _ []     = False
checkLength i (_:xs) = checkLength (i-1) xs
