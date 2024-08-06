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
  , pattern I
  , pattern I8
  , pattern I16
  , pattern I32
  , pattern I64
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

{-# COMPLETE I #-}
pattern I :: I# -> I
pattern I   w = I#   w

{-# COMPLETE I8 #-}
pattern I8 :: I8# -> I8
pattern I8  w = I8#  w

{-# COMPLETE I16 #-}
pattern I16 :: I16# -> I16
pattern I16 w = I16# w

{-# COMPLETE I32 #-}
pattern I32 :: I32# -> I32
pattern I32 w = I32# w

{-# COMPLETE I64 #-}
pattern I64 :: I64# -> I64
pattern I64 w = I64# w
