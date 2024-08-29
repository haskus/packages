{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Union (as in C)
--
-- Unions are storable and can contain any storable data.
-- 
-- Use 'fromUnion' to read an alternative:
--
-- @
-- {-# LANGUAGE DataKinds #-}
--
-- getUnion :: IO (Union '[Word16, Word32, Word64])
-- getUnion = ...
--
-- test = do
--    u <- getUnion
--
--    -- to get one of the member
--    let v = fromUnion u :: Word16
--    let v = fromUnion u :: Word32
--    let v = fromUnion u :: Word64
--
--    -- This won't compile (Word8 is not a member of the union)
--    let v = fromUnion u :: Word8
-- @
--
-- Use 'toUnion' to create a new union:
--
-- @
-- let
--    u2 :: Union '[Word32, Vector 4 Word8]
--    u2 = toUnion (0x12345678 :: Word32)
-- @
--
module Haskus.Binary.Union
   ( Union
   , fromUnion
   , toUnion
   , toUnionZero
   )
where

import Haskus.Utils.Types hiding (Union)
import Haskus.Utils.HList
import Haskus.Utils.Flow (when)
import Haskus.Binary.Storable
import Haskus.Memory.Utils (memCopy, memSet)

import System.IO.Unsafe (unsafePerformIO)

import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Foreign.Storable as FS


-- TODO: rewrite rules
-- poke p (toUnion x) = poke (castPtr p) x
--
-- (fromUnion <$> peek p) :: IO a  = peek (castPtr p) :: IO a



-- | An union 
--
-- We use a list of types as a parameter.
--
-- The union is just a pointer to a buffer containing the value(s). The size of
-- the buffer is implicitly known from the types in the list.
newtype Union (x :: [Type]) = Union (ForeignPtr ()) deriving (Show)

-- | Retrieve a union member from its type
fromUnion :: (Storable a, Member a l) => Union l -> a
fromUnion (Union fp) = unsafePerformIO $ withForeignPtr fp (peek . castPtr)

-- | Create a new union from one of the union types
toUnion :: forall a l . (Storable (Union l), Storable a, Member a l) => a -> Union l
toUnion = toUnion' False

-- | Like 'toUnion' but set the remaining bytes to 0
toUnionZero :: forall a l . (Storable (Union l), Storable a, Member a l) => a -> Union l
toUnionZero = toUnion' True


-- | Create a new union from one of the union types
toUnion' :: forall a l . (Storable (Union l), Storable a, Member a l) => Bool -> a -> Union l
toUnion' zero v = unsafePerformIO $ do
   let sz = sizeOfT @(Union l)
   fp <- mallocForeignPtrBytes (fromIntegral sz)
   withForeignPtr fp $ \p -> do
      -- set bytes after the object to 0
      when zero $ do
         let psz = sizeOfT @a
         memSet (p `plusPtr` fromIntegral psz) (fromIntegral (sz - psz)) 0
      poke (castPtr p) v
   return $ Union fp

type family MapSizeOf fs where
   MapSizeOf '[]       = '[]
   MapSizeOf (x ': xs) = SizeOf x ': MapSizeOf xs

type family MapAlignment fs where
   MapAlignment '[]       = '[]
   MapAlignment (x ': xs) = Alignment x ': MapAlignment xs


instance forall fs.
      ( KnownNat (ListMax (MapSizeOf fs))
      , KnownNat (ListMax (MapAlignment fs))
      )
      => StaticStorable (Union fs)
   where
      type SizeOf (Union fs)    = ListMax (MapSizeOf fs)
      type Alignment (Union fs) = ListMax (MapAlignment fs)

      staticPeekIO ptr = do
         let sz = natValue @(SizeOf (Union fs))
         fp <- mallocForeignPtrBytes sz
         withForeignPtr fp $ \p -> 
            memCopy p (castPtr ptr) (fromIntegral sz)
         return (Union fp)

      staticPokeIO ptr (Union fp) = do
         withForeignPtr fp $ \p ->
            memCopy (castPtr ptr) p (natValue @(SizeOf (Union fs)))

-------------------------------------------------------------------------------------
-- We use HFoldr' to get the maximum size and alignment of the types in the union
-------------------------------------------------------------------------------------

data FoldSizeOf    = FoldSizeOf
data FoldAlignment = FoldAlignment

instance (r ~ Word, Storable a) => Apply FoldSizeOf (a, Word) r where
   apply _ (_,r) = max r (sizeOfT @a)

instance (r ~ Word, Storable a) => Apply FoldAlignment (a, Word) r where
   apply _ (_,r) = max r (alignmentT @a)

-- | Get the union size (i.e. the maximum of the types in the union)
unionSize :: forall l . HFoldr' FoldSizeOf Word l Word => Union l -> Word
unionSize _ = hFoldr' FoldSizeOf (0 :: Word) (undefined :: HList l)

-- | Get the union alignment (i.e. the maximum of the types in the union)
unionAlignment :: forall l . HFoldr' FoldAlignment Word l Word => Union l -> Word
unionAlignment _ = hFoldr' FoldAlignment (0 :: Word) (undefined :: HList l)


-------------------------------------------------------------------------------------
-- Finally we can write the Storable instance
-------------------------------------------------------------------------------------

instance
   ( HFoldr' FoldSizeOf Word l Word
   , HFoldr' FoldAlignment Word l Word
   ) => Storable (Union l) where
   sizeOf     = unionSize
   alignment  = unionAlignment
   peekIO ptr = do
      let sz = sizeOfT' @(Union l)
      fp <- mallocForeignPtrBytes sz
      withForeignPtr fp $ \p -> 
         memCopy p (castPtr ptr) (fromIntegral sz)
      return (Union fp)

   pokeIO ptr (Union fp) = withForeignPtr fp $ \p ->
      memCopy (castPtr ptr) p (sizeOfT' @(Union l))


-- compatibility instance with Foreign.Storable
instance
   ( HFoldr' FoldSizeOf Word l Word
   , HFoldr' FoldAlignment Word l Word
   ) => FS.Storable (Union l) where
   sizeOf     = fromIntegral . unionSize
   alignment  = fromIntegral . unionAlignment
   peek       = peekIO
   poke       = pokeIO
