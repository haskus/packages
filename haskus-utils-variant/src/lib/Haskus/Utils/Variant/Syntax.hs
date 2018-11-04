{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Rebindable syntax for Variant
module Haskus.Utils.Variant.Syntax
   ( (>>=)
   , (>>)
   , return
   )
where

import Haskus.Utils.Variant
import Haskus.Utils.Types
import Haskus.Utils.Types.List

import Prelude hiding ((>>=),(>>),return)

(>>=) :: forall x xs ys. 
   ( KnownNat (Length ys)
   ) => V (x ': xs) -> (x -> V ys) -> V (Concat ys xs)
(>>=) = bindVariant

(>>) :: V xs -> V ys -> V (Concat ys xs)
(>>) = constBindVariant

return :: x -> V '[x]
return = variantFromValue
