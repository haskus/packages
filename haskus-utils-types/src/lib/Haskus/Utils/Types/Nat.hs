{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE CPP #-}

-- We need this otherwise GHC chokes on the export of
-- "type (*)"
#if MIN_VERSION_GLASGOW_HASKELL (8,6,0,0)
{-# LANGUAGE NoStarIsType #-}
#endif

-- | Type-level Nat
module Haskus.Utils.Types.Nat
   ( Nat
   , natValue
   , natValue'
   , KnownNat
   , SomeNat (..)
   , someNatVal
   , sameNat
   -- * Comparisons
   , CmpNat
   , type (<=?)
   , type (<=)
   , Same
   , Max
   , Min
   -- * Operations
   , type (+)
   , type (-)
   , type (*)
   , type (^)
   , Mod
   , Log2
   , Div
   )
where

import GHC.TypeNats
import Haskus.Utils.Types.Bool
import Data.Proxy

-- | Get a Nat value
natValue :: forall (n :: Nat) a. (KnownNat n, Num a) => a
{-# INLINABLE natValue #-}
natValue = fromIntegral (natVal (Proxy :: Proxy n))

-- | Get a Nat value as a Word
natValue' :: forall (n :: Nat). KnownNat n => Word
{-# INLINABLE natValue' #-}
natValue' = natValue @n


-- | Modulo
type family Modulo (a :: Nat) (b :: Nat) where
   Modulo a b = Modulo' (a <=? b) a b

-- | Helper for Modulo
type family Modulo' c a b where
   Modulo' 'True  a b = a
   Modulo' 'False a b = Modulo' ((a-b) <=? b) (a-b) b

-- | Type equality to Nat
type family Same a b :: Nat where
   Same a a = 1
   Same a b = 0

-- | Max of two naturals
type family Max (a :: Nat) (b :: Nat) where
   Max a b = If (a <=? b) b a

-- | Min of two naturals
type family Min (a :: Nat) (b :: Nat) where
   Min a b = If (a <=? b) a b
