module SizedWriter where

import Haskus.Memory.Writer.SizedWriter

foo :: SizedWriter s
foo = mconcat
  [ writeU8 8
  , writeU16 16
  , writeU32 32
  ]
