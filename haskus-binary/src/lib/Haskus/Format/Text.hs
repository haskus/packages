{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Haskus.Format.Text
   ( Text
   , ToText (..)
   , toTextShow
   )
where

import qualified Data.Text as T
import Haskus.Format.Binary.Word

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
