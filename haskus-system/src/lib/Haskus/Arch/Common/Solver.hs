{-# LANGUAGE TypeFamilies #-}

-- | Solver utils
module Haskus.Arch.Common.Solver
   ( Q
   , T
   , NT
   )
where

import Haskus.Utils.Solver
import Data.Kind

data T      -- terminal
data NT p e -- non-terminal

type family Q t a :: Type where
   Q (NT p e) a = Rule e p a
   Q T        a = a
