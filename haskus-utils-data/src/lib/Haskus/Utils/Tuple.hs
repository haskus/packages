{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- | Tuple helpers
module Haskus.Utils.Tuple
   ( uncurry3
   , uncurry4
   , uncurry5
   , uncurry6
   , uncurry7
   , take4
   , fromTuple4
   , module Data.Tuple
   , Unit (..)
   , Tuple
   , ExtractTuple (..)
   , TupleCon (..)
   , tupleHead
   , TupleTail (..)
   , TupleCons (..)
   , ReorderTuple (..)
   )
where

import GHC.Tuple
import GHC.Exts
import Data.Tuple
import Haskus.Utils.Types

-- | Uncurry3
uncurry3 :: (a -> b -> c -> r) -> (a,b,c) -> r
{-# INLINABLE uncurry3 #-}
uncurry3 fn (a,b,c) = fn a b c

-- | Uncurry4
uncurry4 :: (a -> b -> c -> d -> r) -> (a,b,c,d) -> r
{-# INLINABLE uncurry4 #-}
uncurry4 fn (a,b,c,d) = fn a b c d

-- | Uncurry5
uncurry5 :: (a -> b -> c -> d -> e -> r) -> (a,b,c,d,e) -> r
{-# INLINABLE uncurry5 #-}
uncurry5 fn (a,b,c,d,e) = fn a b c d e

-- | Uncurry6
uncurry6 :: (a -> b -> c -> d -> e -> f -> r) -> (a,b,c,d,e,f) -> r
{-# INLINABLE uncurry6 #-}
uncurry6 fn (a,b,c,d,e,f) = fn a b c d e f

-- | Uncurry7
uncurry7 :: (a -> b -> c -> d -> e -> f -> g -> r) -> (a,b,c,d,e,f,g) -> r
{-# INLINABLE uncurry7 #-}
uncurry7 fn (a,b,c,d,e,f,g) = fn a b c d e f g


-- | Take specialised for quadruple
take4 :: [a] -> (a,a,a,a)
{-# INLINABLE take4 #-}
take4 [a,b,c,d] = (a,b,c,d)
take4 _         = error "take4: invalid list (exactly 4 elements required)"


-- | toList for quadruple
fromTuple4 :: (a,a,a,a) -> [a]
{-# INLINABLE fromTuple4 #-}
fromTuple4 (a,b,c,d) = [a,b,c,d]

-- | Extract a tuple value statically
class ExtractTuple (n :: Nat) xs where
   -- | Extract a tuple value by type-level index
   tupleN :: Tuple xs -> Index n xs

instance ExtractTuple 0 '[a] where
   {-# INLINABLE tupleN #-}
   tupleN (Unit t) = t

instance ExtractTuple 0 '[e0,e1] where
   {-# INLINABLE tupleN #-}
   tupleN (t,_) = t

instance ExtractTuple 1 '[e0,e1] where
   {-# INLINABLE tupleN #-}
   tupleN (_,t) = t

instance ExtractTuple 0 '[e0,e1,e2] where
   {-# INLINABLE tupleN #-}
   tupleN (t,_,_) = t

instance ExtractTuple 1 '[e0,e1,e2] where
   {-# INLINABLE tupleN #-}
   tupleN (_,t,_) = t

instance ExtractTuple 2 '[e0,e1,e2] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,t) = t

instance ExtractTuple 0 '[e0,e1,e2,e3] where
   {-# INLINABLE tupleN #-}
   tupleN (t,_,_,_) = t

instance ExtractTuple 1 '[e0,e1,e2,e3] where
   {-# INLINABLE tupleN #-}
   tupleN (_,t,_,_) = t

instance ExtractTuple 2 '[e0,e1,e2,e3] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,t,_) = t

instance ExtractTuple 3 '[e0,e1,e2,e3] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,_,t) = t


instance ExtractTuple 0 '[e0,e1,e2,e3,e4] where
   {-# INLINABLE tupleN #-}
   tupleN (t,_,_,_,_) = t

instance ExtractTuple 1 '[e0,e1,e2,e3,e4] where
   {-# INLINABLE tupleN #-}
   tupleN (_,t,_,_,_) = t

instance ExtractTuple 2 '[e0,e1,e2,e3,e4] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,t,_,_) = t

instance ExtractTuple 3 '[e0,e1,e2,e3,e4] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,_,t,_) = t

instance ExtractTuple 4 '[e0,e1,e2,e3,e4] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,_,_,t) = t


instance ExtractTuple 0 '[e0,e1,e2,e3,e4,e5] where
   {-# INLINABLE tupleN #-}
   tupleN (t,_,_,_,_,_) = t

instance ExtractTuple 1 '[e0,e1,e2,e3,e4,e5] where
   {-# INLINABLE tupleN #-}
   tupleN (_,t,_,_,_,_) = t

instance ExtractTuple 2 '[e0,e1,e2,e3,e4,e5] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,t,_,_,_) = t

instance ExtractTuple 3 '[e0,e1,e2,e3,e4,e5] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,_,t,_,_) = t

instance ExtractTuple 4 '[e0,e1,e2,e3,e4,e5] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,_,_,t,_) = t

instance ExtractTuple 5 '[e0,e1,e2,e3,e4,e5] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,_,_,_,t) = t


instance ExtractTuple 0 '[e0,e1,e2,e3,e4,e5,e6] where
   {-# INLINABLE tupleN #-}
   tupleN (t,_,_,_,_,_,_) = t

instance ExtractTuple 1 '[e0,e1,e2,e3,e4,e5,e6] where
   {-# INLINABLE tupleN #-}
   tupleN (_,t,_,_,_,_,_) = t

instance ExtractTuple 2 '[e0,e1,e2,e3,e4,e5,e6] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,t,_,_,_,_) = t

instance ExtractTuple 3 '[e0,e1,e2,e3,e4,e5,e6] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,_,t,_,_,_) = t

instance ExtractTuple 4 '[e0,e1,e2,e3,e4,e5,e6] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,_,_,t,_,_) = t

instance ExtractTuple 5 '[e0,e1,e2,e3,e4,e5,e6] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,_,_,_,t,_) = t

instance ExtractTuple 6 '[e0,e1,e2,e3,e4,e5,e6] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,_,_,_,_,t) = t


instance ExtractTuple 0 '[e0,e1,e2,e3,e4,e5,e6,e7] where
   {-# INLINABLE tupleN #-}
   tupleN (t,_,_,_,_,_,_,_) = t

instance ExtractTuple 1 '[e0,e1,e2,e3,e4,e5,e6,e7] where
   {-# INLINABLE tupleN #-}
   tupleN (_,t,_,_,_,_,_,_) = t

instance ExtractTuple 2 '[e0,e1,e2,e3,e4,e5,e6,e7] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,t,_,_,_,_,_) = t

instance ExtractTuple 3 '[e0,e1,e2,e3,e4,e5,e6,e7] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,_,t,_,_,_,_) = t

instance ExtractTuple 4 '[e0,e1,e2,e3,e4,e5,e6,e7] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,_,_,t,_,_,_) = t

instance ExtractTuple 5 '[e0,e1,e2,e3,e4,e5,e6,e7] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,_,_,_,t,_,_) = t

instance ExtractTuple 6 '[e0,e1,e2,e3,e4,e5,e6,e7] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,_,_,_,_,t,_) = t

instance ExtractTuple 7 '[e0,e1,e2,e3,e4,e5,e6,e7] where
   {-# INLINABLE tupleN #-}
   tupleN (_,_,_,_,_,_,_,t) = t

-- | Get first element of the tuple
tupleHead :: forall xs. ExtractTuple 0 xs => Tuple xs -> Index 0 xs
tupleHead = tupleN @0

class TupleTail ts ts' | ts -> ts' where
   tupleTail :: ts -> ts'

instance TupleTail (a,b) (Unit b) where
   {-# INLINABLE tupleTail #-}
   tupleTail (_,b) = Unit b

instance TupleTail (a,b,c) (b,c) where
   {-# INLINABLE tupleTail #-}
   tupleTail (_,b,c) = (b,c)

instance TupleTail (a,b,c,d) (b,c,d) where
   {-# INLINABLE tupleTail #-}
   tupleTail (_,b,c,d) = (b,c,d)

instance TupleTail (a,b,c,d,e) (b,c,d,e) where
   {-# INLINABLE tupleTail #-}
   tupleTail (_,b,c,d,e) = (b,c,d,e)

instance TupleTail (a,b,c,d,e,f) (b,c,d,e,f) where
   {-# INLINABLE tupleTail #-}
   tupleTail (_,b,c,d,e,f) = (b,c,d,e,f)



class TupleCons t ts ts' | t ts -> ts' where
   tupleCons :: t -> ts -> ts'

instance TupleCons a (Unit b) (a,b) where
   {-# INLINABLE tupleCons #-}
   tupleCons a (Unit b) = (a,b)

instance TupleCons a (b,c) (a,b,c) where
   {-# INLINABLE tupleCons #-}
   tupleCons a (b,c) = (a,b,c)

instance TupleCons a (b,c,d) (a,b,c,d) where
   {-# INLINABLE tupleCons #-}
   tupleCons a (b,c,d) = (a,b,c,d)

instance TupleCons a (b,c,d,e) (a,b,c,d,e) where
   {-# INLINABLE tupleCons #-}
   tupleCons a (b,c,d,e) = (a,b,c,d,e)

instance TupleCons a (b,c,d,e,f) (a,b,c,d,e,f) where
   {-# INLINABLE tupleCons #-}
   tupleCons a (b,c,d,e,f) = (a,b,c,d,e,f)


-- | Reorder tuple elements
class ReorderTuple t1 t2 where
   -- | Reorder tuple elements
   tupleReorder :: t1 -> t2


instance ReorderTuple (Unit a) (Unit a) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder = id

instance ReorderTuple (a,b) (a,b) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder = id

instance ReorderTuple (a,b,c) (a,b,c) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder = id

instance ReorderTuple (a,b,c,d) (a,b,c,d) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder = id

instance ReorderTuple (a,b,c,d,e) (a,b,c,d,e) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder = id

instance ReorderTuple (a,b,c,d,e,f) (a,b,c,d,e,f) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder = id

instance ReorderTuple (a,b,c,d,e,f,g) (a,b,c,d,e,f,g) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder = id

instance ReorderTuple (a,b,c,d,e,f,g,h) (a,b,c,d,e,f,g,h) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder = id

instance ReorderTuple (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g,h,i) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder = id

instance ReorderTuple (a,b,c,d,e,f,g,h,i,j) (a,b,c,d,e,f,g,h,i,j) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder = id


instance ReorderTuple (a,b) (b,a) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b) = (b,a)

instance ReorderTuple (a,b,c) (a,c,b) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c) = (a,c,b)

instance ReorderTuple (a,b,c) (b,a,c) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c) = (b,a,c)

instance ReorderTuple (a,b,c) (b,c,a) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c) = (b,c,a)

instance ReorderTuple (a,b,c) (c,a,b) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c) = (c,a,b)

instance ReorderTuple (a,b,c) (c,b,a) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c) = (c,b,a)

instance ReorderTuple (b,c,d) (x,y,z) => ReorderTuple (a,b,c,d) (a,x,y,z) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d) = let (x,y,z) = tupleReorder (b,c,d) in (a,x,y,z)

instance ReorderTuple (a,c,d) (x,y,z) => ReorderTuple (a,b,c,d) (x,b,y,z) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d) = let (x,y,z) = tupleReorder (a,c,d) in (x,b,y,z)

instance ReorderTuple (a,b,d) (x,y,z) => ReorderTuple (a,b,c,d) (x,y,c,z) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d) = let (x,y,z) = tupleReorder (a,b,d) in (x,y,c,z)

instance ReorderTuple (a,b,c) (x,y,z) => ReorderTuple (a,b,c,d) (x,y,z,d) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d) = let (x,y,z) = tupleReorder (a,b,c) in (x,y,z,d)

instance ReorderTuple (b,c,d,e) (x,y,z,w) => ReorderTuple (a,b,c,d,e) (a,x,y,z,w) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e) = let (x,y,z,w) = tupleReorder (b,c,d,e) in (a,x,y,z,w)

instance ReorderTuple (a,c,d,e) (x,y,z,w) => ReorderTuple (a,b,c,d,e) (x,b,y,z,w) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e) = let (x,y,z,w) = tupleReorder (a,c,d,e) in (x,b,y,z,w)

instance ReorderTuple (a,b,d,e) (x,y,z,w) => ReorderTuple (a,b,c,d,e) (x,y,c,z,w) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e) = let (x,y,z,w) = tupleReorder (a,b,d,e) in (x,y,c,z,w)

instance ReorderTuple (a,b,c,e) (x,y,z,w) => ReorderTuple (a,b,c,d,e) (x,y,z,d,w) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e) = let (x,y,z,w) = tupleReorder (a,b,c,e) in (x,y,z,d,w)

instance ReorderTuple (a,b,c,d) (x,y,z,w) => ReorderTuple (a,b,c,d,e) (x,y,z,w,e) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e) = let (x,y,z,w) = tupleReorder (a,b,c,d) in (x,y,z,w,e)

instance ReorderTuple (b,c,d,e,f) (x,y,z,w,v) => ReorderTuple (a,b,c,d,e,f) (a,x,y,z,w,v) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e,f) = let (x,y,z,w,v) = tupleReorder (b,c,d,e,f) in (a,x,y,z,w,v)

instance ReorderTuple (a,c,d,e,f) (x,y,z,w,v) => ReorderTuple (a,b,c,d,e,f) (x,b,y,z,w,v) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e,f) = let (x,y,z,w,v) = tupleReorder (a,c,d,e,f) in (x,b,y,z,w,v)

instance ReorderTuple (a,b,d,e,f) (x,y,z,w,v) => ReorderTuple (a,b,c,d,e,f) (x,y,c,z,w,v) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e,f) = let (x,y,z,w,v) = tupleReorder (a,b,d,e,f) in (x,y,c,z,w,v)

instance ReorderTuple (a,b,c,e,f) (x,y,z,w,v) => ReorderTuple (a,b,c,d,e,f) (x,y,z,d,w,v) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e,f) = let (x,y,z,w,v) = tupleReorder (a,b,c,e,f) in (x,y,z,d,w,v)

instance ReorderTuple (a,b,c,d,f) (x,y,z,w,v) => ReorderTuple (a,b,c,d,e,f) (x,y,z,w,e,v) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e,f) = let (x,y,z,w,v) = tupleReorder (a,b,c,d,f) in (x,y,z,w,e,v)

instance ReorderTuple (a,b,c,d,e) (x,y,z,w,v) => ReorderTuple (a,b,c,d,e,f) (x,y,z,w,v,f) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e,f) = let (x,y,z,w,v) = tupleReorder (a,b,c,d,e) in (x,y,z,w,v,f)


instance ReorderTuple (b,c,d,e,f,g) (x,y,z,w,v,u) => ReorderTuple (a,b,c,d,e,f,g) (a,x,y,z,w,v,u) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e,f,g) = let (x,y,z,w,v,u) = tupleReorder (b,c,d,e,f,g) in (a,x,y,z,w,v,u)

instance ReorderTuple (a,c,d,e,f,g) (x,y,z,w,v,u) => ReorderTuple (a,b,c,d,e,f,g) (x,b,y,z,w,v,u) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e,f,g) = let (x,y,z,w,v,u) = tupleReorder (a,c,d,e,f,g) in (x,b,y,z,w,v,u)

instance ReorderTuple (a,b,d,e,f,g) (x,y,z,w,v,u) => ReorderTuple (a,b,c,d,e,f,g) (x,y,c,z,w,v,u) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e,f,g) = let (x,y,z,w,v,u) = tupleReorder (a,b,d,e,f,g) in (x,y,c,z,w,v,u)

instance ReorderTuple (a,b,c,e,f,g) (x,y,z,w,v,u) => ReorderTuple (a,b,c,d,e,f,g) (x,y,z,d,w,v,u) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e,f,g) = let (x,y,z,w,v,u) = tupleReorder (a,b,c,e,f,g) in (x,y,z,d,w,v,u)

instance ReorderTuple (a,b,c,d,f,g) (x,y,z,w,v,u) => ReorderTuple (a,b,c,d,e,f,g) (x,y,z,w,e,v,u) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e,f,g) = let (x,y,z,w,v,u) = tupleReorder (a,b,c,d,f,g) in (x,y,z,w,e,v,u)

instance ReorderTuple (a,b,c,d,e,g) (x,y,z,w,v,u) => ReorderTuple (a,b,c,d,e,f,g) (x,y,z,w,v,f,u) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e,f,g) = let (x,y,z,w,v,u) = tupleReorder (a,b,c,d,e,g) in (x,y,z,w,v,f,u)

instance ReorderTuple (a,b,c,d,e,f) (x,y,z,w,v,u) => ReorderTuple (a,b,c,d,e,f,g) (x,y,z,w,v,u,g) where
   {-# INLINABLE tupleReorder #-}
   tupleReorder (a,b,c,d,e,f,g) = let (x,y,z,w,v,u) = tupleReorder (a,b,c,d,e,f) in (x,y,z,w,v,u,g)

type family TupleFun r xs where
   TupleFun r '[]      = r
   TupleFun r (x:xs)   = x -> (TupleFun r xs)

-- | Create a Tuple
class TupleCon xs where
   -- | Create a Tuple
   tupleCon :: TupleFun (Tuple xs) xs

instance TupleCon '[] where
   tupleCon = ()

instance TupleCon '[a] where
   tupleCon = Unit

instance TupleCon '[a,b] where
   tupleCon = (,)

instance TupleCon '[a,b,c] where
   tupleCon = (,,)

instance TupleCon '[a,b,c,d] where
   tupleCon = (,,,)

instance TupleCon '[a,b,c,d,e] where
   tupleCon = (,,,,)

instance TupleCon '[a,b,c,d,e,f] where
   tupleCon = (,,,,,)

-- | Boxed tuple
--
-- TODO: put this family into GHC
type family Tuple xs = t | t -> xs where
   Tuple '[]                                                    = ()
   Tuple '[a]                                                   = Unit a
   Tuple '[a,b]                                                 = (a,b)
   Tuple '[a,b,c]                                               = (a,b,c)
   Tuple '[a,b,c,d]                                             = (a,b,c,d)
   Tuple '[a,b,c,d,e]                                           = (a,b,c,d,e)
   Tuple '[a,b,c,d,e,f]                                         = (a,b,c,d,e,f)
   Tuple '[a,b,c,d,e,f,g]                                       = (a,b,c,d,e,f,g)
   Tuple '[a,b,c,d,e,f,g,h]                                     = (a,b,c,d,e,f,g,h)
   Tuple '[a,b,c,d,e,f,g,h,i]                                   = (a,b,c,d,e,f,g,h,i)
   Tuple '[a,b,c,d,e,f,g,h,i,j]                                 = (a,b,c,d,e,f,g,h,i,j)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k]                               = (a,b,c,d,e,f,g,h,i,j,k)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k,l]                             = (a,b,c,d,e,f,g,h,i,j,k,l)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k,l,m]                           = (a,b,c,d,e,f,g,h,i,j,k,l,m)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n]                         = (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o]                       = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p]                     = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q]                   = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r]                 = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s]               = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t]             = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u]           = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v]         = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w]       = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x]     = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y]   = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y)
   Tuple '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z] = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z)


type family TypeReps xs where
   TypeReps '[]                 = '[]
   TypeReps ((a::TYPE k) ': as) = k ': TypeReps as

-- | Unboxed tuple
--
-- TODO: put this family into GHC
type family Tuple# xs = (t :: TYPE ('TupleRep (TypeReps xs))) | t -> xs where
   Tuple# '[]                  = (##)
   Tuple# '[a]                 = (# a #)
   Tuple# '[a,b]               = (# a,b #)
   Tuple# '[a,b,c]             = (# a,b,c #)
   Tuple# '[a,b,c,d]           = (# a,b,c,d #)
   Tuple# '[a,b,c,d,e]         = (# a,b,c,d,e #)
   Tuple# '[a,b,c,d,e,f]       = (# a,b,c,d,e,f #)
   Tuple# '[a,b,c,d,e,f,g]     = (# a,b,c,d,e,f,g #)
   Tuple# '[a,b,c,d,e,f,g,h]   = (# a,b,c,d,e,f,g,h #)
   Tuple# '[a,b,c,d,e,f,g,h,i] = (# a,b,c,d,e,f,g,h,i #)
