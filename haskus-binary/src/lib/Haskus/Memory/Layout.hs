{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}

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
   -- * Layouts
   , CPrimitive (..)
   , CArray (..)
   , CUArray (..)
   , CStruct (..)
   , CUnion (..)
   )
where

import Haskus.Utils.Types

-- $setup
-- >>> import Numeric.Natural

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
type family LPathType p l :: Type
type instance LPathType (LPath '[]) l  = l

-- | Offset obtained when following path p
type family LPathOffset p l :: Nat
type instance LPathOffset (LPath '[]) l  = 0


type family (:->) p (s :: Symbol) where
   (:->) (LPath xs) s = LPath (Snoc xs ('LSymbol s))

type family (:#>) p (n :: Nat) where
   (:#>) (LPath xs) n = LPath (Snoc xs ('LIndex n))

---------------------------
-- Layouts
---------------------------

type family CSizeOf a    :: Nat
type family CAlignment a :: Nat

-- | Primitives
--
-- >>> :kind! CSizeOf (CPrimitive 8 1)
-- CSizeOf (CPrimitive 8 1) :: Natural
-- = 8
--
-- >>> :kind! CAlignment (CPrimitive 8 2)
-- CAlignment (CPrimitive 8 2) :: Natural
-- = 2
--
data CPrimitive (size :: Nat) (align :: Nat)     = CPrimitive
type instance CSizeOf (CPrimitive size align)    = size
type instance CAlignment (CPrimitive size align) = align

-- | Array
--
-- >>> type S = CArray 10 (CPrimitive 8 8)
-- >>> :kind! CSizeOf S
-- CSizeOf S :: Natural
-- = 80
--
-- >>> :kind! CAlignment S
-- CAlignment S :: Natural
-- = 8
data CArray (n :: Nat) (a :: k)       = CArray
type instance CSizeOf (CArray n a)    = n * (CSizeOf a)
type instance CAlignment (CArray n a) = CAlignment a

-- | Unbounded array
--
-- >>> type S = CUArray (CPrimitive 8 8)
-- >>> :kind! CSizeOf S
-- CSizeOf S :: Natural
-- = (TypeError ...)
--
-- >>> :kind! CAlignment S
-- CAlignment S :: Natural
-- = 8
data CUArray (a :: k)                = CUArray
type instance CSizeOf (CUArray a)    = TypeError ('Text "Cannot apply SizeOf to an unbounded array")
type instance CAlignment (CUArray a) = CAlignment a

-- | Struct
--
-- >>> type S = CStruct ['Field "i8" (CPrimitive 1 1), 'Field "i32" (CPrimitive 4 4)]
-- >>> :kind! CSizeOf S
-- CSizeOf S :: Natural
-- = 8
--
-- >>> :kind! CAlignment S
-- CAlignment S :: Natural
-- = 4
data CStruct (fs :: [Field])           = CStruct
type instance CSizeOf (CStruct fs)     = CStructSize fs (CMaxAlignment fs 1) 0
type instance CAlignment (CStruct fs)  = CMaxAlignment fs 1

type family CStructSize (xs :: [Field]) al sz where
   CStructSize '[] al sz               =
      sz + PaddingEx (sz `Mod` al) al
   CStructSize ('Field s t : fs) al sz = CStructSize fs al
      (sz + CSizeOf t + PaddingEx (sz `Mod` CAlignment t) (CAlignment t))

-- | Union
--
-- >>> type S = CUnion ['Field "i8" (CPrimitive 1 1), 'Field "i32" (CPrimitive 4 4)]
-- >>> :kind! CSizeOf S
-- CSizeOf S :: Natural
-- = 4
--
-- >>> :kind! CAlignment S
-- CAlignment S :: Natural
-- = 4
data CUnion (fs :: [Field])           = CUnion
type instance CSizeOf (CUnion fs)     = CUnionSize fs (CMaxAlignment fs 1) 0
type instance CAlignment (CUnion fs)  = CMaxAlignment fs 1

type family CUnionSize (xs :: [Field]) al sz where
   CUnionSize '[] al sz               =
      sz + PaddingEx (sz `Mod` al) al
   CUnionSize ('Field s t : fs) al sz = CUnionSize fs al (Max (CSizeOf t) sz)

-- | Structure field
data Field = Field Symbol Type

type family PaddingEx (m :: Nat) (a :: Nat) where
   PaddingEx 0 a = 0
   PaddingEx m a = a - m

type family CMaxAlignment (xs :: [Field]) al where
   CMaxAlignment '[] al               = al
   CMaxAlignment ('Field s t : fs) al =
      CMaxAlignment fs (Max al (CAlignment t))

