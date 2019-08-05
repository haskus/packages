{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Haskus.Binary.Bits.Mask
   ( MaskBits (..)
   , makeMaskFinite
   , makeMask
   , maskDyn
   , Maskable
   , mask
   )
where

import Haskus.Binary.Bits.Finite
import Haskus.Binary.Bits.Shift
import Haskus.Binary.Bits.Bitwise
import Haskus.Number.Word
import Haskus.Number.Int
import Haskus.Utils.Types
import GHC.Natural

-- | makeMaskFinite 3 = 00000111
makeMaskFinite :: forall a.
   ( ShiftableBits a
   , FiniteBits a
   , KnownNat (BitSize a)
   , Bitwise a
   ) => Word -> a
{-# INLINABLE makeMaskFinite #-}
makeMaskFinite n = complement zeroBits `shiftR` off
   where
      off = natValue' @(BitSize a) - n

{-# SPECIALIZE makeMaskFinite :: Word -> Int #-}
{-# SPECIALIZE makeMaskFinite :: Word -> Int8 #-}
{-# SPECIALIZE makeMaskFinite :: Word -> Int16 #-}
{-# SPECIALIZE makeMaskFinite :: Word -> Int32 #-}
{-# SPECIALIZE makeMaskFinite :: Word -> Int64 #-}
{-# SPECIALIZE makeMaskFinite :: Word -> Word #-}
{-# SPECIALIZE makeMaskFinite :: Word -> Word8 #-}
{-# SPECIALIZE makeMaskFinite :: Word -> Word16 #-}
{-# SPECIALIZE makeMaskFinite :: Word -> Word32 #-}
{-# SPECIALIZE makeMaskFinite :: Word -> Word64 #-}

class MaskBits a where
   -- | Make a mask dynamically
   makeMaskDyn :: Word -> a

instance MaskBits Natural where
   makeMaskDyn n = mkNatural (replicate (fromIntegral q) c ++ [makeMaskFinite r])
      where
         c = complement zeroBits
         (q,r) = n `quotRem` 32

instance MaskBits Word   where makeMaskDyn = makeMaskFinite
instance MaskBits Word8  where makeMaskDyn = makeMaskFinite
instance MaskBits Word16 where makeMaskDyn = makeMaskFinite
instance MaskBits Word32 where makeMaskDyn = makeMaskFinite
instance MaskBits Word64 where makeMaskDyn = makeMaskFinite
instance MaskBits Int    where makeMaskDyn = makeMaskFinite
instance MaskBits Int8   where makeMaskDyn = makeMaskFinite
instance MaskBits Int16  where makeMaskDyn = makeMaskFinite
instance MaskBits Int32  where makeMaskDyn = makeMaskFinite
instance MaskBits Int64  where makeMaskDyn = makeMaskFinite

-- | Make a mask statically
makeMask :: forall n a.
   ( KnownNat n
   , MaskBits a
   ) => a
{-# INLINABLE makeMask #-}
makeMask = makeMaskDyn (natValue' @n)

-- | Keep only the n least-significant bits of the given value
maskDyn ::
   ( MaskBits a
   , Bitwise a
   ) => Word -> a -> a
{-# INLINABLE maskDyn #-}
maskDyn n v = v .&. makeMaskDyn n

-- | Keep only the n least-significant bits of the given value
mask :: forall n a. Maskable n a => a -> a
{-# INLINABLE mask #-}
mask v = v .&. makeMask @n

type Maskable n a =
   ( MaskBits a
   , Bitwise a
   , KnownNat n
   )
