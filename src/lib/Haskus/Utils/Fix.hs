{-# LANGUAGE UndecidableInstances #-}

-- | Fix-point type.
--
-- > Fix f = f (Fix f)
--
module Haskus.Utils.Fix
   ( Fix (..)
   -- * Simple recursion
   , cata
   , ana
   , hylo
   , (~>)
   -- * Monadic recursion
   , cataM
   , anaM
   , hyloM
   )
where

import Data.Function (on)

-- | Fix-point type
--
-- > Fix f = f (Fix f)
--
newtype Fix f = Fix { unFix :: (f (Fix f)) }

instance Show (f (Fix f)) => Show (Fix f) where
    showsPrec n x = showParen (n > 10) $ \s ->
        "Fix " ++ showsPrec 11 (unFix x) s

instance Read (f (Fix f)) => Read (Fix f) where
    readsPrec d = readParen (d > 10) $ \r ->
        [(Fix m, t) | ("Fix", s) <- lex r, (m, t) <- readsPrec 11 s]

instance Eq (f (Fix f)) => Eq (Fix f) where
    (==) = (==) `on` unFix

instance Ord (f (Fix f)) => Ord (Fix f) where
    compare = compare `on` unFix


-- | Catamorphism or generic function fold.
cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata f = f . fmap (cata f) . unFix

-- | Anamorphism or generic function unfold.
ana :: Functor f => (a -> f a) -> (a -> Fix f)
ana f = Fix . fmap (ana f) . f

-- | Hylomorphism is anamorphism followed by catamorphism.
hylo :: Functor f => (f b -> b) -> (a -> f a) -> (a -> b)
hylo phi psi = cata phi . ana psi

-- | Infix version of @hylo@.
(~>) :: Functor f => (a -> f a) -> (f b -> b) -> (a -> b)
psi ~> phi = phi . fmap (hylo phi psi) . psi

-- monadic recursion

-- | Monadic catamorphism.
cataM :: (Applicative m, Monad m, Traversable t)
    => (t a -> m a) -> Fix t -> m a
cataM f = (f =<<) . traverse (cataM f) . unFix

-- | Monadic anamorphism.
anaM :: (Applicative m, Monad m, Traversable t)
    => (a -> m (t a)) -> (a -> m (Fix t))
anaM f = fmap Fix . (traverse (anaM f) =<<) . f

-- | Monadic hylomorphism.
hyloM :: (Applicative m, Monad m, Traversable t)
    => (t b -> m b) -> (a -> m (t a)) -> (a -> m b)
hyloM phi psi = (cataM phi =<<) . anaM psi

