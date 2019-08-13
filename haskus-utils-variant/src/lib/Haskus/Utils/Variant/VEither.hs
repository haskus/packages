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
{-# LANGUAGE LambdaCase #-}

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
   , veitherToValue
   , veitherBimap
   , VEitherLift
   , veitherLift
   , veitherAppend
   , veitherPrepend
   , veitherCont
   , veitherToEither
   , veitherProduct
   , module Haskus.Utils.Variant
   )
where

import Haskus.Utils.Variant
import Haskus.Utils.Types
import Data.Coerce

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> import Data.Foldable


-- | Variant biased towards one type
newtype VEither es a
   = VEither (V (a ': es))


----------------------
-- Patterns
----------------------

-- | Left value
--
-- >>> VLeft (V "failed" :: V '[String,Int]) :: VEither '[String,Int] Bool
-- VLeft "failed"
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
-- VLeft "Test"
--
veitherFromVariant :: V (a ': es) -> VEither es a
{-# INLINABLE veitherFromVariant #-}
veitherFromVariant = VEither

-- | Convert a VEither into a Variant
--
-- >>> let x = VRight True :: VEither '[Int,Float] Bool
-- >>> veitherToVariant x
-- True
--
veitherToVariant :: VEither es a -> V (a ': es)
{-# INLINABLE veitherToVariant #-}
veitherToVariant (VEither x) = x

-- | Convert a VEither into an Either
--
-- >>> let x = VRight True :: VEither '[Int,Float] Bool
-- >>> veitherToEither x
-- Right True
--
veitherToEither :: VEither es a -> Either (V es) a
{-# INLINABLE veitherToEither #-}
veitherToEither = \case
   VLeft xs -> Left xs
   VRight x -> Right x

-- | Extract from a VEither without left types
--
-- >>> let x = VRight True :: VEither '[] Bool
-- >>> veitherToValue x
-- True
veitherToValue :: forall a. VEither '[] a -> a
{-# INLINABLE veitherToValue #-}
veitherToValue = coerce (variantToValue @a)

-- | Bimap for VEither
--
-- >>> let x = VRight True :: VEither '[Int,Float] Bool
-- >>> veitherBimap id not x
-- VRight False
--
veitherBimap :: (V es -> V fs) -> (a -> b) ->  VEither es a -> VEither fs b
{-# INLINABLE veitherBimap #-}
veitherBimap f g v = case v of
   VLeft xs -> VLeft (f xs)
   VRight x -> VRight (g x)


type VEitherLift es es' =
   ( LiftVariant es es'
   )

-- | Lift a VEither into another
veitherLift :: forall es' es a.
   ( VEitherLift es es'
   ) => VEither es a -> VEither es' a
{-# INLINABLE veitherLift #-}
veitherLift = veitherBimap liftVariant id

-- | Prepend errors to VEither
veitherPrepend :: forall ns es a.
   ( KnownNat (Length ns)
   ) => VEither es a -> VEither (Concat ns es) a
{-# INLINABLE veitherPrepend #-}
veitherPrepend = veitherBimap (prependVariant @ns) id

-- | Append errors to VEither
veitherAppend :: forall ns es a.
   VEither es a -> VEither (Concat es ns) a
{-# INLINABLE veitherAppend #-}
veitherAppend = veitherBimap (appendVariant @ns) id

-- | VEither continuations
veitherCont :: (V es -> u) -> (a -> u) -> VEither es a -> u
{-# INLINABLE veitherCont #-}
veitherCont f g v = case v of
   VLeft xs -> f xs
   VRight x -> g x

-- | Product of two VEither
veitherProduct :: KnownNat (Length (b:e2)) => VEither e1 a -> VEither e2 b -> VEither (Tail (Product (a:e1) (b:e2))) (a,b)
veitherProduct (VEither x) (VEither y) = VEither (productVariant x y)

-- | Functor instance for VEither
--
-- >>> let x = VRight True :: VEither '[Int,Float] Bool
-- >>> fmap (\b -> if b then "Success" else "Failure") x
-- VRight "Success"
--
instance Functor (VEither es) where
   {-# INLINABLE fmap #-}
   fmap f (VEither v) = VEither (mapVariantAt @0 f v)

-- | Applicative instance for VEither
--
-- >>> let x = VRight True  :: VEither '[Int,Float] Bool
-- >>> let y = VRight False :: VEither '[Int,Float] Bool
-- >>> (&&) <$> x <*> y
-- VRight False
-- >>> (||) <$> x <*> y
-- VRight True
--
instance Applicative (VEither es) where
   pure = VRight

   VRight f <*> VRight a = VRight (f a)
   VLeft v  <*> _        = VLeft v
   _        <*> VLeft v  = VLeft v

-- | Monad instance for VEither
--
-- >>> let x   = VRight True    :: VEither '[Int,Float] Bool
-- >>> let f v = VRight (not v) :: VEither '[Int,Float] Bool
-- >>> x >>= f
-- VRight False
--
instance Monad (VEither es) where
   VRight a >>= f = f a
   VLeft v  >>= _ = VLeft v

-- | Foldable instance for VEither
--
-- >>> let x   = VRight True    :: VEither '[Int,Float] Bool
-- >>> let y   = VLeft (V "failed" :: V '[String,Int]) :: VEither '[String,Int] Bool
-- >>> forM_ x print
-- True
-- >>> forM_ y print
--
instance Foldable (VEither es) where
   foldMap f (VRight a) = f a
   foldMap _ (VLeft _)  = mempty

instance Traversable (VEither es) where
   traverse f (VRight a) = VRight <$> f a
   traverse _ (VLeft xs) = pure (VLeft xs)
