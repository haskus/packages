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
