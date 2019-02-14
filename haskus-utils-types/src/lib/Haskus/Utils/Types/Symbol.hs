{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

-- | Type-level Symbol (i.e. string)
module Haskus.Utils.Types.Symbol
   ( Symbol
   , symbolValue
   , KnownSymbol
   , CmpSymbol
   , SomeSymbol (..)
   , sameSymbol
   , someSymbolVal
   )
where

import GHC.TypeLits
import Data.Proxy

--- | Get a Symbol value
symbolValue :: forall (s :: Symbol). (KnownSymbol s) => String
{-# INLINABLE symbolValue #-}
symbolValue = symbolVal (Proxy :: Proxy s)
