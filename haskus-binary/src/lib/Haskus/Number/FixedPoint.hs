{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Fixed-point numbers
module Haskus.Number.FixedPoint
   ( FixedPoint (..)
   , getFixedPointBase
   , fromFixedPointBase
   , toFixedPoint
   , fromFixedPoint
   )
where

import Haskus.Binary.BitField
import Haskus.Binary.Bits
import Haskus.Binary.Storable
import Haskus.Utils.Types
import Data.Coerce
import Data.Ratio

-- | Unsigned fixed-point number
-- * `w` is the backing type
-- * `i` is the number of bits for the integer part (before the radix point)
-- * `f` is the number of bits for the fractional part (after the radix point)
--
-- >>> :seti -XDataKinds
-- >>> import Data.Word
-- >>> fromIntegral 0 :: FixedPoint Word32 16 16
-- 0 % 1
--
-- >>> fromIntegral 10 :: FixedPoint Word32 16 16
-- 10 % 1
newtype FixedPoint w (i :: Nat) (f :: Nat) = FixedPoint (BitFields w
   '[ BitField i "integer"    w
    , BitField f "fractional" w
    ])
   deriving (Storable)

-- | Get base value
getFixedPointBase :: FixedPoint w i f -> w
getFixedPointBase (FixedPoint (BitFields w)) = w

-- | Set base value
fromFixedPointBase :: forall w i f. w -> FixedPoint w i f
fromFixedPointBase w = FixedPoint @w @i @f (BitFields w)

instance
   ( BitSize w ~ (i + f)
   , Num w
   , FiniteBits w
   , Bits w
   , KnownNat i
   , KnownNat f
   , Field w
   , Integral w
   ) => Num (FixedPoint w i f) where
   (+)    = coerce ((+) :: w -> w -> w)
   (-)    = coerce ((-) :: w -> w -> w)
   negate = error "Can't negate unsigned fixed-point nubmer"
   abs    = id
   signum = error "Can't call signum on unsigned fixed-point number"
   (*)    = error "Fixed-point number multiplication not implemented yet"
   fromInteger x = toFixedPoint (toRational x)


instance
   ( BitSize w ~ (i + f)
   , Integral w
   , FiniteBits w
   , Bits w
   , Field w
   , KnownNat i
   , KnownNat f
   ) => Real (FixedPoint w i f) where
   toRational fp = fromIntegral (getFixedPointBase fp) % (2^(natValue' @f))

deriving instance forall w n d.
   ( Integral w
   , Bits w
   , Field w
   , BitSize w ~ (n + d)
   , KnownNat n
   , KnownNat d
   ) => Eq (FixedPoint w n d)

instance forall w n d.
   ( Integral w
   , Bits w
   , Field w
   , BitSize w ~ (n + d)
   , KnownNat n
   , KnownNat d
   ) => Ord (FixedPoint w n d) where

   compare x y = compare (getFixedPointBase x) (getFixedPointBase y)
   x > y       = getFixedPointBase x >  getFixedPointBase y
   x >= y      = getFixedPointBase x >= getFixedPointBase y
   x < y       = getFixedPointBase x <  getFixedPointBase y
   x <= y      = getFixedPointBase x <= getFixedPointBase y

instance forall w n d.
   ( Integral w
   , Bits w
   , Field w
   , BitSize w ~ (n + d)
   , KnownNat n
   , KnownNat d
   , Show w
   ) => Show (FixedPoint w n d) where

   show w = show (toRational w)

-- | Convert to a fixed point value
toFixedPoint :: forall a w (n :: Nat) (d :: Nat).
   ( RealFrac a
   , BitSize w ~ (n + d)
   , KnownNat n
   , KnownNat d
   , Bits w
   , Field w
   , Num w
   , Integral w
   ) => a -> FixedPoint w n d
toFixedPoint a = FixedPoint $ BitFields (round (a * 2^natValue' @d))

-- | Convert from a fixed-point value
fromFixedPoint :: forall a w (n :: Nat) (d :: Nat).
   ( RealFrac a
   , BitSize w ~ (n + d)
   , KnownNat n
   , KnownNat d
   , Bits w
   , Field w
   , Num w
   , Integral w
   ) => FixedPoint w n d -> a
fromFixedPoint (FixedPoint bf) = w / 2^(natValue' @d)
   where
      w = fromIntegral (bitFieldsBits bf)
