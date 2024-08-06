-- | Unsigned primitive types
module Haskus.Binary.Word
  ( U#
  , U8#
  , U16#
  , U32#
  , U64#
  , U
  , U8
  , U16
  , U32
  , U64
  , pattern U
  , pattern U8
  , pattern U16
  , pattern U32
  , pattern U64
  )
where

import GHC.Exts
import GHC.Word

type U#   = Word#
type U8#  = Word8#
type U16# = Word16#
type U32# = Word32#
type U64# = Word64#

type U   = Word
type U8  = Word8
type U16 = Word16
type U32 = Word32
type U64 = Word64

{-# COMPLETE U #-}
pattern U :: U# -> U
pattern U w = W# w

{-# COMPLETE U8 #-}
pattern U8 :: U8# -> U8
pattern U8 w = W8# w

{-# COMPLETE U16 #-}
pattern U16 :: U16# -> U16
pattern U16 w = W16# w

{-# COMPLETE U32 #-}
pattern U32 :: U32# -> U32
pattern U32 w = W32# w

{-# COMPLETE U64 #-}
pattern U64 :: U64# -> U64
pattern U64 w = W64# w
