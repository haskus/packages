{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- | Utils for Monads
module Haskus.Utils.Monad
   ( MonadInIO (..)
   , module Control.Monad
   , module Control.Monad.IO.Class
   , module Control.Monad.Trans.Class
   , whileM
   , loop
   , loopM
   , whenM
   , unlessM
   , ifM
   , notM
   , anyM
   , allM
   , orM
   , andM
   )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad
import Control.Monad.State

class MonadIO m => MonadInIO m where
   -- | Lift with*-like functions into IO (alloca, etc.)
   liftWith :: (forall c. (a -> IO c) -> IO c) -> (a -> m b) -> m b

   -- | Lift with*-like functions into IO (alloca, etc.)
   liftWith2 :: (forall c. (a -> b -> IO c) -> IO c) -> (a -> b -> m e) -> m e

instance MonadInIO IO where
   {-# INLINABLE liftWith #-}
   liftWith wth f = wth f

   {-# INLINABLE liftWith2 #-}
   liftWith2 wth f = wth f

instance MonadInIO m => MonadInIO (StateT s m) where
   {-# INLINABLE liftWith #-}
   liftWith wth f =
      StateT $ \s -> do
         liftWith wth (\a -> runStateT (f a) s)

   {-# INLINABLE liftWith2 #-}
   liftWith2 wth f =
      StateT $ \s ->
         liftWith2 wth (\a b -> runStateT (f a b) s)

-- | Keep running an operation until it becomes 'False'. As an example:
--
-- @
-- whileM $ do sleep 0.1; notM $ doesFileExist "foo.txt"
-- readFile "foo.txt"
-- @
--
--   If you need some state persisted between each test, use 'loopM'.
whileM :: Monad m => m Bool -> m ()
whileM act = do
    b <- act
    when b $ whileM act

-- Looping

-- | A looping operation, where the predicate returns 'Left' as a seed for the next loop
--   or 'Right' to abort the loop.
--
-- > loop (\x -> if x < 10 then Left $ x * 2 else Right $ show x) 1 == "16"
loop :: (a -> Either a b) -> a -> b
loop act x = case act x of
    Left x' -> loop act x'
    Right v -> v

-- | A monadic version of 'loop', where the predicate returns 'Left' as a seed for the next loop
--   or 'Right' to abort the loop.
loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = act x >>= \case
   Left x' -> loopM act x'
   Right v -> return v


-- | Like 'when', but where the test can be monadic.
whenM :: Monad m => m Bool -> m () -> m ()
whenM b t = ifM b t (return ())

-- | Like 'unless', but where the test can be monadic.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b f = ifM b (return ()) f

-- | Like @if@, but where the test can be monadic.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb t f = do
   b <- mb
   if b then t else f

-- | Like 'not', but where the test can be monadic.
notM :: Functor m => m Bool -> m Bool
notM = fmap not

-- | A version of 'any' lifted to a monad. Retains the short-circuiting behaviour.
--
-- > anyM Just [False,True ,undefined] == Just True
-- > anyM Just [False,False,undefined] == undefined
-- > \(f :: Int -> Maybe Bool) xs -> anyM f xs == orM (map f xs)
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []     = return False
anyM p (x:xs) = ifM (p x) (return True) (anyM p xs)

-- | A version of 'all' lifted to a monad. Retains the short-circuiting behaviour.
--
-- > allM Just [True,False,undefined] == Just False
-- > allM Just [True,True ,undefined] == undefined
-- > \(f :: Int -> Maybe Bool) xs -> anyM f xs == orM (map f xs)
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ []     = return True
allM p (x:xs) = ifM (p x) (allM p xs) (return False)

-- | A version of 'or' lifted to a monad. Retains the short-circuiting behaviour.
--
-- > orM [Just False,Just True ,undefined] == Just True
-- > orM [Just False,Just False,undefined] == undefined
-- > \xs -> Just (or xs) == orM (map Just xs)
orM :: Monad m => [m Bool] -> m Bool
orM = anyM id

-- | A version of 'and' lifted to a monad. Retains the short-circuiting behaviour.
--
-- > andM [Just True,Just False,undefined] == Just False
-- > andM [Just True,Just True ,undefined] == undefined
-- > \xs -> Just (and xs) == andM (map Just xs)
andM :: Monad m => [m Bool] -> m Bool
andM = allM id
