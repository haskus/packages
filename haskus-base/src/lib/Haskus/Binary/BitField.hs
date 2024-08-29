{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

-- | Bit fields (as in C)
--
-- This module allows you to define bit fields over words. For instance, you can
-- have a Word16 split into 3 fields X, Y and Z composed of 5, 9 and 2 bits
-- respectively.
--
-- @                  X             Y          Z   @
-- @ w :: Word16 |0 0 0 0 0|0 0 0 0 0 0 0 0 0|0 0| @
-- 
-- You define it as follows:
--
-- @
-- {-# LANGUAGE DataKinds #-}
--
-- w :: BitFields Word16 '[ BitField 5 "X" Word8 
--                        , BitField 9 "Y" Word16
--                        , BitField 2 "Z" Word8
--                        ]
-- w = BitFields 0x0102
-- @
--
-- Note that each field has its own associated type (e.g. Word8 for X and Z)
-- that must be large enough to hold the number of bits for the field.
--
-- Operations on BitFields expect that the cumulated size of the fields is equal
-- to the whole word size: use a padding field if necessary. Otherwise you can
-- use unsafe versions of the functions: extractField', updateField',
-- withField'.
-- 
-- You can extract and update the value of a field by its name:
--
-- @
-- x = extractField @"X" w
-- z = extractField @"Z" w
-- w' = updateField @"Y" 0x16 w
-- @
--
-- Fields can also be 'BitSet' or 'EnumField':
--
-- @
-- {-# LANGUAGE DataKinds #-}
--
-- data A = A0 | A1 | A2 | A3 deriving (Enum,CEnum)
--
-- data B = B0 | B1 deriving (Enum,BitOffset)
--
-- w :: BitFields Word16 '[ BitField 5 "X" (EnumField Word8 A)
--                        , BitField 9 "Y" Word16
--                        , BitField 2 "Z" (BitSet Word8 B)
--                        ]
-- w = BitFields 0x0102
-- @
--
module Haskus.Binary.BitField where

import Haskus.Binary.BitSet as BitSet
import Haskus.Binary.Enum
import Haskus.Number.Word
import Haskus.Number.Int
import Haskus.Binary.Bits
import Haskus.Binary.Storable
import Haskus.Utils.HList
import Haskus.Utils.Types
import Haskus.Utils.Tuple

import Data.Typeable

-- | Bit fields on a base type b
newtype BitFields b (f :: [Type]) = BitFields b deriving (Storable)

-- | Get backing word
bitFieldsBits :: BitFields b f -> b
{-# INLINABLE bitFieldsBits #-}
bitFieldsBits (BitFields b) = b


-- | A field of n bits
newtype BitField (n :: Nat) (name :: nk) s = BitField s deriving (Storable)

-- | Get the bit offset of a field from its name
type family Offset name fs :: Nat where
   Offset name (BitField n name  s ': xs) = AddOffset xs
   Offset name (BitField n name2 s ': xs) = Offset name xs

type family AddOffset fs :: Nat where
   AddOffset '[]                        = 0
   AddOffset (BitField n name s ': xs)  = n + AddOffset xs

-- | Get the type of a field from its name
type family Output name fs :: Type where
   Output name (BitField n name  s ': xs) = s
   Output name (BitField n name2 s ': xs) = Output name xs

-- | Get the size of a field from it name
type family Size name fs :: Nat where
   Size name (BitField n name  s ': xs) = n
   Size name (BitField n name2 s ': xs) = Size name xs

-- | Get the whole size of a BitFields
type family WholeSize fs :: Nat where
   WholeSize '[]                        = 0
   WholeSize (BitField n name s ': xs)  = n + WholeSize xs

type family BitFieldTypes xs where
   BitFieldTypes '[]                       = '[]
   BitFieldTypes (BitField n name s ': xs) = s ': BitFieldTypes xs

class Field f where
   fromField :: Integral b => f -> b
   toField   :: Integral b => b -> f

instance Field Bool where
   fromField True  = 1
   fromField False = 0
   toField 0  = False
   toField _  = True

instance Field Word where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Word8 where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Word16 where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Word32 where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Word64 where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Int where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Int8 where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Int16 where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Int32 where
   fromField = fromIntegral
   toField   = fromIntegral

instance Field Int64 where
   fromField = fromIntegral
   toField   = fromIntegral

instance (FiniteBits b, Integral b, BitOffset a) => Field (BitSet b a) where
   fromField = fromIntegral . BitSet.toBits
   toField   = BitSet.fromBits . fromIntegral

instance (Integral b, CEnum a) => Field (EnumField b a) where
   fromField = fromCEnum . fromEnumField
   toField   = toEnumField . toCEnum

-- | Get the value of a field
extractField :: forall name fields b .
   ( KnownNat (Offset name fields)
   , KnownNat (Size name fields)
   , WholeSize fields ~ BitSize b
   , Bits b, Integral b
   , Field (Output name fields)
   ) => BitFields b fields -> Output name fields
{-# INLINABLE extractField #-}
extractField = extractField' @name

-- | Get the value of a field (without checking sizes)
extractField' :: forall name fields b .
   ( KnownNat (Offset name fields)
   , KnownNat (Size name fields)
   , Bits b, Integral b
   , Field (Output name fields)
   ) => BitFields b fields -> Output name fields
{-# INLINABLE extractField' #-}
extractField' (BitFields w) = toField ((w `shiftR` off) .&. ((1 `shiftL` sz) - 1))
   where
      off = natValue @(Offset name fields)
      sz  = natValue @(Size name fields)


-- | Set the value of a field
updateField :: forall name fields b .
   ( KnownNat (Offset name fields)
   , KnownNat (Size name fields)
   , WholeSize fields ~ BitSize b
   , Bits b, Integral b
   , Field (Output name fields)
   ) => Output name fields -> BitFields b fields -> BitFields b fields
{-# INLINABLE updateField #-}
updateField = updateField' @name

-- | Set the value of a field (without checking sizes)
updateField' :: forall name fields b .
   ( KnownNat (Offset name fields)
   , KnownNat (Size name fields)
   , Bits b, Integral b
   , Field (Output name fields)
   ) => Output name fields -> BitFields b fields -> BitFields b fields
{-# INLINABLE updateField' #-}
updateField' value (BitFields w) = BitFields $ ((fromField value `shiftL` off) .&. mask') .|. (w .&. complement mask')
   where
      off   = natValue @(Offset name fields)
      sz    = natValue @(Size name fields)
      mask' = ((1 `shiftL` sz) - 1) `shiftL` off


-- | Modify the value of a field
withField :: forall name fields b f .
   ( KnownNat (Offset name fields)
   , KnownNat (Size name fields)
   , WholeSize fields ~ BitSize b
   , Bits b, Integral b
   , f ~ Output name fields
   , Field f
   ) => (f -> f) -> BitFields b fields -> BitFields b fields
{-# INLINABLE withField #-}
withField = withField' @name

-- | Modify the value of a field (without checking sizes)
withField' :: forall name fields b f .
   ( KnownNat (Offset name fields)
   , KnownNat (Size name fields)
   , Bits b, Integral b
   , f ~ Output name fields
   , Field f
   ) => (f -> f) -> BitFields b fields -> BitFields b fields
{-# INLINABLE withField' #-}
withField' f bs = updateField' @name (f v) bs
   where
      v = extractField' @name bs


-------------------------------------------------------------------------------------
-- We use HFoldr' to extract each component and create a HList from it. Then we
-- convert it into a Tuple
-------------------------------------------------------------------------------------
data Extract = Extract
data Name    = Name

instance forall name bs b l l2 i (n :: Nat) s r w .
   ( bs ~ BitFields w l                    -- the bitfields
   , b ~ BitField n name s                 -- the current field
   , i ~ (bs, HList l2)                    -- input type
   , r ~ (bs, HList (Output name l ': l2)) -- result type
   , BitSize w ~ WholeSize l
   , Integral w, Bits w
   , KnownNat (Offset name l)
   , KnownNat (Size name l)
   , Field (Output name l)
   ) => Apply Extract (b, i) r where
      apply _ (_, (bs,xs)) =
         (bs, HCons (extractField @name bs) xs)

instance forall name bs b l l2 i (n :: Nat) s r w .
   ( bs ~ BitFields w l       -- the bitfields
   , b ~ BitField n name s    -- the current field
   , i ~ HList l2             -- input type
   , r ~ HList (String ': l2) -- result type
   , Typeable name
   ) => Apply Name (b, i) r where
      apply _ (_, xs) = HCons (show (typeRep (Proxy :: Proxy name))) xs

fieldValues :: forall l l2 w bs .
   ( bs ~ BitFields w l
   , HFoldr' Extract (bs, HList '[]) l (bs, HList l2)
   ) => bs -> HList l2
fieldValues bs = snd res
   where
      res :: (bs, HList l2)
      res = hFoldr' Extract ((bs, HNil) :: (bs, HList '[])) (undefined :: HList l)

fieldNames :: forall l l2 w bs .
   ( bs ~ BitFields w l
   , HFoldr' Name (HList '[]) l (HList l2)
   ) => bs -> HList l2
fieldNames _ = hFoldr' Name (HNil :: HList '[]) (undefined :: HList l)

-- | Get values in a tuple
matchFields :: forall l l2 w bs t .
   ( bs ~ BitFields w l
   , HFoldr' Extract (bs, HList '[]) l (bs, HList l2)
   , HTuple l2
   , t ~ Tuple l2
   ) => bs -> t
matchFields = hToTuple @l2 . fieldValues


-- | Get field names and values in a tuple
matchNamedFields ::forall lt lv ln lnv w bs t .
   ( bs ~ BitFields w lt
   , HFoldr' Extract (bs, HList '[]) lt (bs, HList lv)
   , HFoldr' Name (HList '[]) lt (HList ln)
   , HZipList ln lv lnv
   , HTuple lnv
   , t ~ Tuple lnv
   ) => bs -> t
matchNamedFields = hToTuple @lnv . matchNamedFields'

-- | Get field names and values in a tuple
matchNamedFields' ::forall lt lv ln lnv w bs .
   ( bs ~ BitFields w lt
   , HFoldr' Extract (bs, HList '[]) lt (bs, HList lv)
   , HFoldr' Name (HList '[]) lt (HList ln)
   , HZipList ln lv lnv
   ) => bs -> HList lnv
matchNamedFields' bs = hZipList names values
   where
      names  = fieldNames bs
      values = fieldValues bs

-- | Get field names and values in a tuple
instance forall lt ln lnv w bs.
   ( bs ~ BitFields w lt
   , ln ~ Replicate (Length lt) String
   , HFoldr' Extract (bs, HList '[]) lt (bs, HList (BitFieldTypes lt))
   , HFoldr' Name (HList '[]) lt (HList ln)
   , HZipList ln (BitFieldTypes lt) lnv
   , Show (HList lnv)
   ) => Show (BitFields w lt) where
      show bs = show (matchNamedFields' bs :: HList lnv)


instance forall lt lt2 w bs.
   ( bs ~ BitFields w lt
   , HFoldr' Extract (bs, HList '[]) lt (bs, HList lt2)
   , Eq (HList lt2)
   , lt2 ~ BitFieldTypes lt
   ) => Eq (BitFields w lt) where
   (==) x y = x' == y'
      where
         x' :: HList lt2
         x' = fieldValues x
         y' :: HList lt2
         y' = fieldValues y
