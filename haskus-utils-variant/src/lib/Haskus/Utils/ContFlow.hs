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
   , (>:>)
   , (>-:>)
   , (>%:>)
   , (>::>)
   , (>:-:>)
   , (>:%:>)
   , ContTuple
   , AddR
   , MultiCont (..)
   )
where

import Haskus.Utils.Tuple

-- | A continuation based control-flow
newtype ContFlow (xs :: [*]) r = ContFlow (ContTuple xs r -> r)

-- | Convert a list of types into the actual data type representing the
-- continuations.
type family ContTuple (xs :: [*]) r where
   ContTuple xs r = Tuple (AddR xs r)

type family AddR f r where
   AddR '[] r       = '[]
   AddR (x ': xs) r = (x -> r) ': AddR xs r

-- | A multi-continuable type
class MultiCont a where
   type MultiContTypes a :: [*]

   -- | Convert a data into a multi-continuation
   toCont :: a -> ContFlow (MultiContTypes a) r

   -- | Convert a data into a multi-continuation (monadic)
   toContM :: Monad m => m a -> ContFlow (MultiContTypes a) (m r)


-- | Bind a multi-continuable type to a tuple of continuations
(>:>) :: MultiCont a => a -> ContTuple (MultiContTypes a) r -> r
{-# INLINABLE (>:>) #-}
(>:>) a !cs = toCont a >::> cs

infixl 0 >:>

-- | Bind a single-continuable type to a 1-tuple of continuations
(>-:>) :: (MultiCont a, MultiContTypes a ~ '[b]) => a -> (b -> r) -> r
{-# INLINABLE (>-:>) #-}
(>-:>) a c = toCont a >:-:> c

infixl 0 >-:>

-- | Bind a multi-continuable type to a tuple of continuations and
-- reorder fields if necessary
(>%:>) ::
   ( MultiCont a
   , ReorderTuple ts (ContTuple (MultiContTypes a) r)
   ) => a -> ts -> r
{-# INLINABLE (>%:>) #-}
(>%:>) a !cs = toCont a >:%:> cs

infixl 0 >%:>


-- | Bind a flow to a tuple of continuations
(>::>) :: ContFlow xs r -> ContTuple xs r -> r
{-# INLINABLE (>::>) #-}
(>::>) (ContFlow f) !cs = f cs

infixl 0 >::>

-- | Bind a flow to a 1-tuple of continuations
(>:-:>) :: ContFlow '[a] r -> (a -> r) -> r
{-# INLINABLE (>:-:>) #-}
(>:-:>) (ContFlow f) c = f (Unit c)

infixl 0 >:-:>

-- | Bind a flow to a tuple of continuations and
-- reorder fields if necessary
(>:%:>) :: forall ts xs r.
   ( ReorderTuple ts (ContTuple xs r)
   ) => ContFlow xs r -> ts -> r
{-# INLINABLE (>:%:>) #-}
(>:%:>) (ContFlow f) !cs = f (tupleReorder cs)

infixl 0 >:%:>
