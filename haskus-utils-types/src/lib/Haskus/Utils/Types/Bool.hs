{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Type level booleans
module Haskus.Utils.Types.Bool
   ( If
   , NotB
   , OrB
   , AndB
   , XorB
   , KnownBool (..)
   -- * Generic
   , Not
   , And
   , Or
   , Xor
   , AndMany
   , OrMany
   , XorMany
   , AllFalse
   , AllTrue
   )
where

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> import Haskus.Utils.Types


-- | If-then-else
type family If (c :: Bool) (t :: k) (e :: k) where
   If 'True  t e = t
   If 'False t e = e


-- | Type-level Bool known at compile time
class KnownBool a where
   -- | Get a bool value from a Bool at type level
   --
   -- >>> boolValue @'True
   -- True
   -- >>> boolValue @(AndB 'True 'False)
   -- False
   boolValue :: Bool

instance KnownBool 'True where
   {-# INLINE boolValue #-}
   boolValue = True

instance KnownBool 'False where
   {-# INLINE boolValue #-}
   boolValue = False


-- | Boolean Not
--
-- >>> boolValue @(NotB 'True)
-- False
-- >>> boolValue @(NotB 'False)
-- True
type family NotB x where
   NotB 'True  = 'False
   NotB 'False = 'True

-- | Boolean And
--
-- >>> boolValue @(AndB 'True 'False)
-- False
-- >>> boolValue @(AndB 'True 'True)
-- True
type family AndB x y where
   AndB 'True 'True   = 'True
   AndB 'True 'False  = 'False
   AndB 'False 'True  = 'False
   AndB 'False 'False = 'False

-- | Boolean Or
--
-- >>> boolValue @(OrB 'True 'False)
-- True
-- >>> boolValue @(OrB 'False 'False)
-- False
type family OrB x y where
   OrB 'True 'True   = 'True
   OrB 'False 'True  = 'True
   OrB 'True 'False  = 'True
   OrB 'False 'False = 'False

-- | Boolean Xor
--
-- >>> boolValue @(XorB 'True 'False)
-- True
-- >>> boolValue @(XorB 'False 'False)
-- False
-- >>> boolValue @(XorB 'True 'True)
-- False
type family XorB x y where
   XorB 'True 'True   = 'False
   XorB 'False 'True  = 'True
   XorB 'True 'False  = 'True
   XorB 'False 'False = 'False

---------------------------------------
-- Generic
---------------------------------------

-- | Generic boolean Not
--
-- >>> natValue' @(Not 1 0 1 :: Nat)
-- 0
-- >>> natValue' @(Not 1 0 0 :: Nat)
-- 1
type family Not (t :: b) (f :: b) (x :: b) :: b where
   Not t f t = f
   Not t f f = t

-- | Generic boolean And
--
-- >>> natValue' @(And 1 0 1 0 :: Nat)
-- 0
-- >>> natValue' @(And 1 0 1 1 :: Nat)
-- 1
type family And (t :: b) (f :: b) (x :: b) (y :: b) :: b where
   And t f t t = t
   And t f t f = f
   And t f f t = f
   And t f f f = f

-- | Generic boolean Or
--
-- >>> natValue' @(Or 1 0 1 0 :: Nat)
-- 1
-- >>> natValue' @(Or 1 0 0 0 :: Nat)
-- 0
type family Or (t :: b) (f :: b) (x :: b) (y :: b) :: b where
   Or t f t t = t
   Or t f f t = t
   Or t f t f = t
   Or t f f f = f

-- | Generic boolean Xor
--
-- >>> natValue' @(Xor 1 0 1 0 :: Nat)
-- 1
-- >>> natValue' @(Xor 1 0 0 0 :: Nat)
-- 0
-- >>> natValue' @(Xor 1 0 1 1 :: Nat)
-- 0
type family Xor (t :: b) (f :: b) (x :: b) (y :: b) :: b where
   Xor t f t t = f
   Xor t f f t = t
   Xor t f t f = t
   Xor t f f f = f


-- | Generic boolean And on a list
--
-- >>> natValue' @(AndMany 1 0 '[1,0,1] :: Nat)
-- 0
-- >>> natValue' @(AndMany 1 0 '[1,1,1] :: Nat)
-- 1
type family AndMany (t :: b) (f :: b) (xs :: [b]) :: b where
   AndMany t f '[t]      = t
   AndMany t f '[f]      = f
   AndMany t f (f ': xs) = f
   AndMany t f (t ': xs) = AndMany t f xs

-- | Generic boolean Or on a list
--
-- >>> natValue' @(OrMany 1 0 '[1,0,1] :: Nat)
-- 1
-- >>> natValue' @(OrMany 1 0 '[1,1,1] :: Nat)
-- 1
-- >>> natValue' @(OrMany 1 0 '[0,0,0] :: Nat)
-- 0
type family OrMany (t :: b) (f :: b) (xs :: [b]) :: b where
   OrMany t f '[t]      = t
   OrMany t f '[f]      = f
   OrMany t f (t ': xs) = t
   OrMany t f (f ': xs) = OrMany t f xs

-- | Generic boolean Xor on a list (i.e. check if there is a single true element
-- in the list)
--
-- >>> natValue' @(XorMany 1 0 '[0,0,1] :: Nat)
-- 1
-- >>> natValue' @(XorMany 1 0 '[1,0,1] :: Nat)
-- 0
-- >>> natValue' @(XorMany 1 0 '[0,0,0] :: Nat)
-- 0
type family XorMany (t :: b) (f :: b) (xs :: [b]) :: b where
   XorMany t f '[t]      = t
   XorMany t f '[f]      = f
   XorMany t f (t ': xs) = AllFalse t f xs
   XorMany t f (f ': xs) = XorMany t f xs

-- | Check if all the elements are false
--
-- >>> natValue' @(AllFalse 1 0 '[0,0,1] :: Nat)
-- 0
-- >>> natValue' @(AllFalse 1 0 '[0,0,0] :: Nat)
-- 1
type family AllFalse (t :: b) (f :: b) (xs :: [b]) :: b where
   AllFalse t f '[t]      = f
   AllFalse t f '[f]      = t
   AllFalse t f (t ': xs) = f
   AllFalse t f (f ': xs) = AllFalse t f xs

-- | Check if all the elements are true
--
-- >>> natValue' @(AllTrue 1 0 '[0,0,1] :: Nat)
-- 0
-- >>> natValue' @(AllTrue 1 0 '[1,1,1] :: Nat)
-- 1
type family AllTrue (t :: b) (f :: b) (xs :: [b]) :: b where
   AllTrue t f '[t]      = t
   AllTrue t f '[f]      = f
   AllTrue t f (f ': xs) = f
   AllTrue t f (t ': xs) = AllTrue t f xs
