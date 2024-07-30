{-# LANGUAGE BangPatterns #-}

-- | Control-flow
module Haskus.Utils.Flow
   ( MonadIO (..)
   , MonadInIO (..)
   -- * Basic operators
   , (>.>)
   , (<.<)
   , (|>)
   , (<|)
   , (||>)
   , (<||)
   , (|||>)
   , (<|||)
   -- * Monadic/applicative operators
   , when
   , unless
   , whenM
   , unlessM
   , ifM
   , guard
   , void
   , forever
   , foldM
   , foldM_
   , forM
   , forM_
   , forMaybeM
   , mapM
   , mapM_
   , sequence
   , replicateM
   , replicateM_
   , filterM
   , join
   , (<=<)
   , (>=>)
   , loopM
   , whileM
   , intersperseM_
   , forLoopM_
   , forLoop
   -- * Variant based operators
   , module Haskus.Utils.Variant.Excepts
   -- * Monad transformers
   , lift
   )
where

import Haskus.Utils.Variant
import Haskus.Utils.Variant.Excepts
import Haskus.Utils.Monad
import Haskus.Utils.Maybe

-- $setup
-- >>> :seti -XDataKinds
-- >>> :seti -XTypeApplications
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeFamilies

-- | Compose functions
--
-- >>> (+1) >.> (*7) <| 1
-- 14
(>.>) :: (a -> b) -> (b -> c) -> a -> c
f >.> g = \x -> g (f x)

infixl 9 >.>

-- | Compose functions
--
-- >>> (+1) <.< (*7) <| 1
-- 8
(<.<) :: (b -> c) -> (a -> b) -> a -> c
f <.< g = \x -> f (g x)

infixr 9 <.<


-- | Apply a function
--
-- >>> 5 |> (*2)
-- 10
(|>) :: a -> (a -> b) -> b
{-# INLINABLE (|>) #-}
x |> f = f x

infixl 0 |>

-- | Apply a function
--
-- >>> (*2) <| 5
-- 10
(<|) :: (a -> b) -> a -> b
{-# INLINABLE (<|) #-}
f <| x = f x

infixr 0 <|

-- | Apply a function in a Functor
--
-- >>> Just 5 ||> (*2)
-- Just 10
(||>) :: Functor f => f a -> (a -> b) -> f b
{-# INLINABLE (||>) #-}
x ||> f = fmap f x

infixl 0 ||>

-- | Apply a function in a Functor
--
-- >>> (*2) <|| Just 5
-- Just 10
(<||) :: Functor f => (a -> b) -> f a -> f b
{-# INLINABLE (<||) #-}
f <|| x = fmap f x

infixr 0 <||

-- | Apply a function in a Functor
--
-- >>> Just [5] |||> (*2)
-- Just [10]
(|||>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
{-# INLINABLE (|||>) #-}
x |||> f = fmap (fmap f) x

infixl 0 |||>

-- | Apply a function in a Functor
--
-- >>> (*2) <||| Just [5]
-- Just [10]
--
(<|||) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
{-# INLINABLE (<|||) #-}
f <||| x = fmap (fmap f) x

infixr 0 <|||

-- | Composition of catMaybes and forM
-- 
-- >>> let f x = if x > 3 then putStrLn "OK" >> return (Just x) else return Nothing
-- >>> forMaybeM [0..5] f
-- OK
-- OK
-- [4,5]
forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM xs f = catMaybes <|| forM xs f

-- | forM_ with interspersed action
--
-- >>> intersperseM_ (putStr ", ") ["1","2","3","4"] putStr
-- 1, 2, 3, 4
intersperseM_ :: Monad m => m () -> [a] -> (a -> m ()) -> m ()
intersperseM_ f as g = go as
   where
      go []     = pure ()
      go [x]    = g x
      go (x:xs) = g x >> f >> go xs

-- | Fast for-loop in a Monad (more efficient than forM_ [0..n] for instance).
--
-- >>> forLoopM_ (0::Word) (<5) (+1) print
-- 0
-- 1
-- 2
-- 3
-- 4
forLoopM_ :: (Monad m) => a -> (a -> Bool) -> (a -> a) -> (a -> m ()) -> m ()
{-# INLINABLE forLoopM_ #-}
forLoopM_ start cond inc f = go start
   where
      go !x | cond x    = f x >> go (inc x)
            | otherwise = return ()


-- | Fast fort-loop with an accumulated result
--
-- >>> let f acc n = acc ++ (if n == 0 then "" else ", ") ++ show n
-- >>> forLoop (0::Word) (<5) (+1) "" f
-- "0, 1, 2, 3, 4"
forLoop :: a -> (a -> Bool) -> (a -> a) -> acc -> (acc -> a -> acc) -> acc
{-# INLINABLE forLoop #-}
forLoop start cond inc acc0 f = go acc0 start
   where
      go acc !x
         | cond x    = let acc' = f acc x
                       in acc' `seq` go acc' (inc x)
         | otherwise = acc
