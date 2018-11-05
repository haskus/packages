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

-- | Common type functions
module Haskus.Utils.Types
   ( Nat
   , Symbol
   , natValue
   , natValue'
   , symbolValue
   , KnownNat
   , KnownSymbol
   , CmpNat
   , CmpSymbol
   , type (<=?)
   , type (<=)
   , type (+)
   , type (-)
   , type (*)
   , type (^)
   , Assert
   , If
   , Modulo
   , Same
   , Proxy (..)
   , TypeError
   , ErrorMessage (..)
   )
where

import GHC.TypeLits
import Data.Proxy

-- | Get a Nat value
natValue :: forall (n :: Nat) a. (KnownNat n, Num a) => a
{-# INLINE natValue #-}
natValue = fromIntegral (natVal (Proxy :: Proxy n))

-- | Get a Nat value
natValue' :: forall (n :: Nat). KnownNat n => Word
{-# INLINE natValue' #-}
natValue' = natValue @n

-- | Get a Symbol value
symbolValue :: forall (s :: Symbol). (KnownSymbol s) => String
{-# INLINE symbolValue #-}
symbolValue = symbolVal (Proxy :: Proxy s)

-- | If-then-else
type family If (c :: Bool) (t :: k) (e :: k) where
   If 'True  t e = t
   If 'False t e = e


-- | Like: If cond t (TypeError msg)
--
-- The difference is that the TypeError doesn't appear in the RHS of the type
-- which leads to better error messages (see GHC #14771).
--
-- For instance:
--    type family F n where
--       F n = If (n <=? 8) Int8 (TypeError (Text "ERROR"))
--
--    type family G n where
--       G n = Assert (n <=? 8) Int8 (Text "ERROR")
--
--    If GHC cannot solve `F n ~ Word`, it shows: ERROR
--    If GHC cannot solve `G n ~ Word`, it shows:
--       can't match `Assert...` with `Word`
--
type family Assert (prop :: Bool) (val :: k) (msg :: ErrorMessage) where
   Assert 'True  val msg = val
   Assert 'False val msg = TypeError msg

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
