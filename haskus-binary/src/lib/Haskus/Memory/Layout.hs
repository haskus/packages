{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Memory layout
--
-- Describe a memory region
module Haskus.Format.Binary.Layout
   ( LPath (..)
   , PathElem (..)
   , lPath
   , LPathType
   , LPathOffset
   , LRoot
   , (:->)
   , (:#>)
   )
where

import Haskus.Utils.Types

-- | Path in a layout
data LPath (path :: [PathElem])   = LPath

-- | Layout path element
data PathElem
   = LIndex Nat      -- ^ Addressing via a numeric index
   | LSymbol Symbol  -- ^ Addressing via a symbol

-- | Layout path root
type LRoot = LPath '[]

-- | Index in the layout path
--
-- Helper for ``ptr --> lPath @p``
-- until
lPath :: forall e. LPath '[e]
lPath = LPath

-- | Type obtained when following path p
type family LPathType p l :: *
type instance LPathType (LPath '[]) l  = l

-- | Offset obtained when following path p
type family LPathOffset p l :: Nat
type instance LPathOffset (LPath '[]) l  = 0


type family (:->) p (s :: Symbol) where
   (:->) (LPath xs) s = LPath (Snoc xs ('LSymbol s))

type family (:#>) p (n :: Nat) where
   (:#>) (LPath xs) n = LPath (Snoc xs ('LIndex n))
