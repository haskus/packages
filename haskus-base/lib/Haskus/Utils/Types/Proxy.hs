{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

module Haskus.Utils.Types.Proxy
   ( module Data.Proxy
   , NatVal (..)
   , Proxy#
   )
where

import Data.Proxy
import Haskus.Utils.Types.Nat
import GHC.Exts (Proxy#)

-- | A natural value Proxy
data NatVal (t :: Nat) = NatVal

instance KnownNat t => Show (NatVal t) where
   show _ = show (natValue' @t)

