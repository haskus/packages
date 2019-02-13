{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Haskus.Utils.Types.Proxy
   ( module Data.Proxy
   , NatVal (..)
   )
where

import Data.Proxy
import Haskus.Utils.Types

-- | A natural value Proxy
data NatVal (t :: Nat) = NatVal

instance KnownNat t => Show (NatVal t) where
   show _ = show (natValue' @t)

