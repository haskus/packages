{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

-- | Rebindable syntax for Variant
module Haskus.Data.Variant.Syntax
   ( (>>=)
   , (>>)
   , return
   )
where

import Haskus.Data.Variant
import Haskus.Utils.Types

import Prelude hiding ((>>=),(>>),return)

(>>=) :: forall x xs ys. 
   ( KnownNat (Length ys)
   ) => V (x ': xs) -> (x -> V ys) -> V (Concat ys xs)
(>>=) = bindVariant

(>>) :: V xs -> V ys -> V (Concat ys xs)
(>>) = constBindVariant

return :: x -> V '[x]
return = variantFromValue
