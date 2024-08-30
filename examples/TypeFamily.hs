{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Haskus.Data.Variant

import Data.Char

class C a where
   type R a :: *
   foo :: a -> R a

instance C Int where
   type R Int = Char
   foo = chr

instance C [Int] where
   type R [Int] = String
   foo = fmap chr

-- boilerplate instance
instance C (V '[]) where
   type R (V '[]) = V '[]
   {-# INLINABLE foo #-}
   foo = undefined

-- Variant instance
instance
   ( V (MapR xs) ~ R (V xs)
   , C (Variant xs)
   , C x
   ) => C (V (x ': xs)) where

   type R (V (x ': xs)) = V (MapR (x ': xs))

   {-# INLINABLE foo #-}
   foo v = case popVariantHead v of
      Right a -> toVariantHead (foo a)
      Left as -> toVariantTail (foo as)

type family MapR xs where
   MapR '[]       = '[]
   MapR (x ': xs) = R x ': MapR xs


v1,v2 :: V '[Int,[Int]]
v1 = V @Int 52
v2 = V @[Int] [52,88,67]

main :: IO ()
main = do
   print (foo v1)
   print (foo v2)

-- > foo v1
-- '4'
-- > foo v2
-- "4XC"
-- > :t (foo v1)
-- (foo v1) :: V '[Char, String]
-- > :t (foo v2)
-- (foo v2) :: V '[Char, String]
