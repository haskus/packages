{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}

-- | Unsigned primitive words
module Haskus.Number.Word
   ( WordAtLeast
   , WordN
   -- * Unlifted
   , module GHC.Word
   , Word#
   , plusWord#
   , minusWord#
   , ltWord#
   , leWord#
   , gtWord#
   , geWord#
   , eqWord#
   )
where

import Data.Word
import GHC.Word
import GHC.Exts

import Haskus.Utils.Types

-- | Return a Word with at least 'n' bits
type family WordAtLeast (n :: Nat) where
   WordAtLeast n =
       If (n <=? 8) Word8
      (If (n <=? 16) Word16
      (If (n <=? 32) Word32
      (Assert (n <=? 64) Word64
      ('Text "Cannot find Word with size " ':<>: 'ShowType n)
      )))

-- | Return a Word with exactly 'n' bits
type family WordN (n :: Nat) where
   WordN 8  = Word8
   WordN 16 = Word16
   WordN 32 = Word32
   WordN 64 = Word64
   WordN n  = TypeError ('Text "Cannot find Word with size " ':<>: 'ShowType n)
