module Writer where

import Haskus.Memory.Writer

foo :: Writer s
foo = mconcat
  [ writeU8 8
  , writeU16 16
  , writeU32 32
  ]
