{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}

module Haskus.Format.Text
   ( Text
   , ToText (..)
   , toTextShow
   -- * Text
   , TextFormat (..)
   , TypedText (..)
   , ShowText (..)
   )
where

import qualified Data.Text as T
import Data.Char

import Haskus.Format.Binary.Word
import Haskus.Memory.Buffer
import Haskus.Utils.Flow

-- | UTF-16 encoded strict text
type Text = T.Text

-- | Value convertible into text
class ToText a where
   toText :: a -> Text


instance ToText String where
   toText x = T.pack x 

instance ToText Text where
   toText x = x

instance ToText Float where
   {-# INLINE toText #-}
   toText = toTextShow

instance ToText Double where
   {-# INLINE toText #-}
   toText = toTextShow


instance ToText Int where
   {-# INLINE toText #-}
   toText = toTextShow

instance ToText Int8 where
   {-# INLINE toText #-}
   toText = toTextShow

instance ToText Int16 where
   {-# INLINE toText #-}
   toText = toTextShow

instance ToText Int32 where
   {-# INLINE toText #-}
   toText = toTextShow

instance ToText Int64 where
   {-# INLINE toText #-}
   toText = toTextShow


instance ToText Word where
   {-# INLINE toText #-}
   toText = toTextShow

instance ToText Word8 where
   {-# INLINE toText #-}
   toText = toTextShow

instance ToText Word16 where
   {-# INLINE toText #-}
   toText = toTextShow

instance ToText Word32 where
   {-# INLINE toText #-}
   toText = toTextShow

instance ToText Word64 where
   {-# INLINE toText #-}
   toText = toTextShow


-- | Convert a Showable value into a Text
toTextShow :: Show a => a -> Text
{-# INLINE toTextShow #-}
toTextShow x = T.pack (show x)


-- | Supported text formats
data TextFormat
   = ASCII
   | UTF8
   | UTF16_BOM
   | UTF16_BE
   | UTF16_LE
   | UTF32_BOM
   | UTF32_BE
   | UTF32_LE
   deriving (Show,Eq)

newtype TypedText (t :: k) mut pin gc
   = TypedText (TypedBuffer t mut pin gc )

class ShowText a where
   showText :: a -> IO String

instance ShowText (TypedText 'ASCII mut 'Pinned gc) where
   showText (TypedText (TypedBuffer b)) = do
      bs <- bufferToList b
      return (bs ||> fromIntegral ||> chr)
   
