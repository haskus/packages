{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

-- | Continuation based control-flow
module Haskus.Utils.ContFlow
   ( ContFlow (..)
   , (>::>)
   , (>:-:>)
   , (>:%:>)
   , ContListToTuple
   , ContTupleToList
   , StripR
   , AddR
   )
where

import Haskus.Utils.Tuple
import Haskus.Utils.Types

-- | A continuation based control-flow
newtype ContFlow (xs :: [*]) r = ContFlow (ContListToTuple xs r -> r)

-- | Convert a list of types into the actual data type representing the
-- continuations.
type family ContListToTuple (xs :: [*]) r where
   ContListToTuple xs r = ListToTuple (AddR xs r)

-- | Convert a tuple of continuations into a list of types
type family ContTupleToList t r :: [*] where
   ContTupleToList t r = StripR (TupleToList t) r

type family AddR f r where
   AddR '[] r       = '[]
   AddR (x ': xs) r = (x -> r) ': AddR xs r

type family StripR f r where
   StripR '[] r              = '[]
   StripR ((x -> r) ': xs) r = x ': StripR xs r
   StripR ((x -> w) ': xs) r =
      TypeError ( 'Text "Invalid continuation return type `"
                  ':<>: 'ShowType w ':<>: 'Text "', expecting `"
                  ':<>: 'ShowType r ':<>: 'Text "'")

-- | Bind a flow to a tuple of continuations
(>::>) :: ContFlow xs r -> ContListToTuple xs r -> r
{-# INLINE (>::>) #-}
(>::>) (ContFlow f) !cs = f cs

infixl 0 >::>

-- | Bind a flow to a 1-tuple of continuations
(>:-:>) :: ContFlow '[a] r -> (a -> r) -> r
{-# INLINE (>:-:>) #-}
(>:-:>) (ContFlow f) c = f (Single c)

infixl 0 >:-:>

-- | Bind a flow to a tuple of continuations and
-- reorder fields if necessary
(>:%:>) :: forall ts xs r.
   ( ReorderTuple ts (ContListToTuple xs r)
   ) => ContFlow xs r -> ts -> r
{-# INLINE (>:%:>) #-}
(>:%:>) (ContFlow f) !cs = f (tupleReorder cs)

infixl 0 >:%:>


