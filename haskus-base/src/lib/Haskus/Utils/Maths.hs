module Haskus.Utils.Maths
   ( gcds
   , lcms
   )
where

-- | Return the GCD of a list of integrals
--
-- >>> gcds [2,4,8]
-- 2
gcds :: Integral a => [a] -> a
gcds []     = 1
gcds [0]    = 1
gcds [x]    = x
gcds (x:xs) = foldr gcd x xs

-- | Return the LCM of a list of integrals
--
-- >>> lcms [2,3,5]
-- 30
lcms :: Integral a => [a] -> a
lcms []     = 0
lcms [x]    = x
lcms (x:xs) = foldr lcm x xs
