-- | Signed primitive types
module Haskus.Binary.Int
  ( I#
  , I8#
  , I16#
  , I32#
  , I64#
  , I
  , I8
  , I16
  , I32
  , I64
  )
where

import GHC.Exts
import GHC.Int

type I#   = Int#
type I8#  = Int8#
type I16# = Int16#
type I32# = Int32#
type I64# = Int64#

type I   = Int
type I8  = Int8
type I16 = Int16
type I32 = Int32
type I64 = Int64
