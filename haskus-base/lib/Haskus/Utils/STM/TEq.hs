-- | Equality in a STM context
module Haskus.Utils.STM.TEq
   ( TEq (..)
   )
where

import Haskus.Utils.STM

class TEq a where
   teq :: a -> a -> STM Bool


instance Eq a => TEq (TVar a) where
   teq a b = do
      a' <- readTVar a
      b' <- readTVar b
      return (a' == b')
