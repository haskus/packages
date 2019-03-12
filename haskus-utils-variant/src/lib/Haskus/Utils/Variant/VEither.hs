{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Variant biased towards one type
--
-- This allows definition of common type classes (Functor, etc.) that can't  be
-- provided for Variant
module Haskus.Utils.Variant.VEither
   ( VEither
   , pattern VLeft
   , pattern VRight
   , veitherFromVariant
   , veitherToVariant
   )
where

import Haskus.Utils.Variant

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies


-- | Variant biased towards one type
newtype VEither es a
   = VEither (V (a ': es))


----------------------
-- Patterns
----------------------

-- | Left value
--
-- >>> VLeft (V "failed" :: V '[String,Int]) :: VEither '[String,Int] Bool
-- VLeft (V @[Char] "failed" :: V '[[Char], Int])
--
pattern VLeft :: forall x xs. V xs -> VEither xs x
pattern VLeft xs <- ((popVariantHead . veitherToVariant) -> Left xs)
   where
      VLeft xs = VEither (toVariantTail xs)

-- | Right value
--
-- >>> VRight True :: VEither '[String,Int] Bool
-- VRight True
pattern VRight :: forall x xs. x -> VEither xs x
pattern VRight x <- ((popVariantHead . veitherToVariant) -> Right x)
   where
      VRight x = VEither (toVariantHead x)

{-# COMPLETE VLeft,VRight #-}


----------------------
-- Show instance
----------------------

instance
   ( Show a
   , Show (V es)
   ) => Show (VEither es a) where
   showsPrec d v = showParen (d /= 0) $ case v of
      VLeft xs -> showString "VLeft "
                  . showsPrec 10 xs
      VRight x -> showString "VRight "
                  . showsPrec 10 x


-- | Convert a Variant into a VEither
--
-- >>> let x = V "Test" :: V '[Int,String,Double]
-- >>> veitherFromVariant x
-- VLeft (V @[Char] "Test" :: V '[[Char], Double])
--
veitherFromVariant :: V (a ': es) -> VEither es a
veitherFromVariant = VEither

-- | Convert a VEither into a Variant
--
-- >>> let x = VRight True :: VEither '[Int,Float] Bool
-- >>> veitherToVariant x
-- V @Bool True :: V '[Bool, Int, Float]
--
veitherToVariant :: VEither es a -> V (a ': es)
veitherToVariant (VEither x) = x

-- | Functor instance for VEither
--
-- >>> let x = VRight True :: VEither '[Int,Float] Bool
-- >>> fmap (\b -> if b then "Success" else "Failure") x
-- VRight "Success"
--
instance Functor (VEither es) where
   {-# INLINABLE fmap #-}
   fmap f (VEither v) = VEither (mapVariantAt @0 f v)
