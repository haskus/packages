{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}

-- | Signed primitive integers
module Haskus.Number.Int
   ( IntAtLeast
   , IntN
   -- * Unlifted
   , module GHC.Int
   , Int#
   , (+#)
   , (-#)
   , (==#)
   , (>#)
   , (<#)
   , (>=#)
   , (<=#)
   , isTrue#
   )
where

import Data.Int
import GHC.Int
import GHC.Exts

import Haskus.Utils.Types

-- | Return a Int with at least 'n' bits
type family IntAtLeast (n :: Nat) where
   IntAtLeast n =
       If (n <=? 8) Int8
      (If (n <=? 16) Int16
      (If (n <=? 32) Int32
      (Assert (n <=? 64) Int64
      ('Text "Cannot find Int with size " ':<>: 'ShowType n)
      )))

-- | Return a Int with exactly 'n' bits
type family IntN (n :: Nat) where
   IntN 8  = Int8
   IntN 16 = Int16
   IntN 32 = Int32
   IntN 64 = Int64
   IntN n  = TypeError ('Text "Cannot find Int with size " ':<>: 'ShowType n)
