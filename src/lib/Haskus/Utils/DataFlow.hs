module Haskus.Utils.DataFlow
   (
   )
where

import Haskus.Utils.STM

-- | One-way dataflow constraint
data OneWay a = OneWay
   { oneWayValue :: TVar a
   }
