{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}

-- | Store an Enum in the given backing word type
module Haskus.Binary.Enum
   ( EnumField
   , CEnum (..)
   , fromEnumField
   , toEnumField
   , makeEnum
   , makeEnumMaybe
   , makeEnumWithCustom
   , dataToTag
   )
where

import Haskus.Binary.Storable

import Foreign.Ptr
import Data.Data
import GHC.Int

#if MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
import GHC.Magic (DataToTag,dataToTag#)
#else
import GHC.Prim
#endif

-----------------------------------------------------------------------------
-- EnumField b a: directly store the value of enum "a" as a "b"
-----------------------------------------------------------------------------

-- | Store enum `a` as a `b`
newtype EnumField b a
   = EnumField b
   deriving (Show,Eq,Storable)

instance
      ( Integral b
      , StaticStorable b
      , CEnum a
      ) => StaticStorable (EnumField b a)
   where
      type SizeOf (EnumField b a)    = SizeOf b
      type Alignment (EnumField b a) = Alignment b
      staticPeekIO p                 = EnumField  <$> staticPeek (castPtr p :: Ptr b)
      staticPokeIO p (EnumField v)   = staticPoke (castPtr p :: Ptr b) v

-- | Read an enum field
fromEnumField :: (CEnum a, Integral b) => EnumField b a -> a
{-# INLINABLE fromEnumField #-}
fromEnumField (EnumField b) = toCEnum b

-- | Create an enum field
toEnumField :: (CEnum a, Integral b) => a -> EnumField b a
{-# INLINABLE toEnumField #-}
toEnumField = EnumField . fromCEnum


-----------------------------------------------------------------------------
-- Extended Enum
-----------------------------------------------------------------------------

-- | Extended Enum
--
-- By default, use dataToTag and toEnum to convert from and to an Integral.
--
-- But it can be overloaded to perform transformation before using
-- fromEnum/toEnum. E.g. if values are shifted by 1 compared to Enum values,
-- define fromCEnum = (+1) . fromIntegral . dataToTag
--
class CEnum a where
   fromCEnum       :: Integral b => a -> b
#if MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
   default fromCEnum :: (DataToTag a, Integral b) => a -> b
#endif
   fromCEnum       = fromIntegral . dataToTag

   toCEnum         :: Integral b => b -> a
   default toCEnum :: (Enum a, Integral b) => b -> a
   toCEnum         = toEnum . fromIntegral

-- | Make an enum with the last constructor taking a parameter for the rest of
-- the range
--
-- @
-- data T = A | B | C | D Word8
--
-- makeEnumWithCustom :: Int -> T
-- makeEnumWithCustom x = case x of
--    0 -> A
--    1 -> B
--    2 -> C
--    n -> D (n - 3)
-- @
--
makeEnumWithCustom :: forall a i. (Data a,Integral i) => i -> a
{-# INLINABLE makeEnumWithCustom #-}
makeEnumWithCustom x =
   if x' < maxConstrIndex t
      then fromConstr (indexConstr t x')
      else fromConstrB (fromConstr (toConstr (x' - m)))
               (indexConstr t m)
   where
      m   = maxConstrIndex t
      x'  = fromIntegral x + 1
      t   = dataTypeOf (undefined :: a)

-- | Make an enum with the last constructor taking a parameter for the rest of
-- the range, but don't build the last constructor
--
-- @
-- data T = A | B | C | D Word8
--
-- makeEnumMaybe :: Int -> T
-- makeEnumMaybe x = case x of
--    0 -> Just A
--    1 -> Just B
--    2 -> Just C
--    n -> Nothing
-- @
--
makeEnumMaybe :: forall a i. (Data a,Integral i) => i -> Maybe a
{-# INLINABLE makeEnumMaybe #-}
makeEnumMaybe x =
   if x' < maxConstrIndex t
      then Just (fromConstr (indexConstr t x'))
      else Nothing
   where
      x'  = fromIntegral x + 1
      t   = dataTypeOf (undefined :: a)

-- | Make an enum from a number (0 indexed)
makeEnum :: forall a i. (Data a,Integral i) => i -> a
{-# INLINABLE makeEnum #-}
makeEnum x =fromConstr (indexConstr t x')
   where
      x'  = fromIntegral x + 1
      t   = dataTypeOf (undefined :: a)


-- | Retrieve data tag
--
-- >>> data D = A | B | C
-- >>> dataToTag B
-- 1
#if MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
dataToTag :: DataToTag a => a -> Int
#else
dataToTag :: a -> Int
#endif
dataToTag a = I# (dataToTag# a)
