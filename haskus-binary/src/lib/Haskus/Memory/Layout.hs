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
module Haskus.Memory.Layout
   ( LPath (..)
   , PathElem (..)
   , lPath
   , LPathType
   , LPathOffset
   , LRoot
   , (:->)
   , (:#>)
   -- * Padding
   , RequiredPadding
   , Padding
   , PaddingEx
   -- * Layouts
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

---------------------------
-- Padding
---------------------------

-- | Compute the required padding between a and b to respect b's alignment
type family RequiredPadding a b where
   RequiredPadding a b = Padding (SizeOf a) b

-- | Compute the required padding between the size sz and b to respect b's alignment
type family Padding (sz :: Nat) b where
   Padding sz b = PaddingEx (Mod sz (Alignment b)) (Alignment b)

type family PaddingEx (m :: Nat) (a :: Nat) where
   PaddingEx 0 a = 0
   PaddingEx m a = a - m

---------------------------
-- Layouts
---------------------------

-- | A storable data in constant space whose size is known at compile time
class HasLayout a where
   -- | Size of the stored data (in bytes)
   type SizeOf a    :: Nat

   -- | Alignment requirement (in bytes)
   type Alignment a :: Nat

   -- | Memory layout
   type Layout a    :: MemLayout


-- | Memory layout
data MemLayout
   = Primitive Type           -- ^ Primitive
   | Array Nat Type           -- ^ Bounded array
   | UnboundedArray Type      -- ^ Unbounded array
   | Struct [Field]           -- ^ Structure
   | PackedStruct [Field]     -- ^ Packed structure
   | Overlay [Field]          -- ^ Union
   | BitFields [BitField]     -- ^ Bit fields

-- | Structure field
data Field = Field Symbol Type

-- | Bit field
data BitField = BitField Symbol Nat Type

class MemLayoutable a where
   type MemLayoutType a :: [PathElem] -> Type

   -- | Index into a memory layout
   indexLayout :: a -> LPath p -> MemLayoutType a p
