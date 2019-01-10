{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Haskus.Format.Text
   ( Text
   , ToText (..)
   , toTextShow
   -- * Text
   , TextFormat (..)
   , TextBuffer (..)
   , TextI
   , TextB
   , ShowText (..)
   )
where

import qualified Data.Text as T
import Data.Char
import Data.String
import GHC.Exts (IsList(..))

import Haskus.Format.Binary.Word
import Haskus.Memory.Buffer
import Haskus.Utils.Flow

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies

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

-- | Text buffer
--
-- The buffer contains a text encoded according to phantom type t
newtype TextBuffer (t :: k) b
   = TextBuffer b

-- type aliases
type TextI t            = TextBuffer t BufferI
type TextB t mut pin gc = TextBuffer t (Buffer mut pin gc)


class ShowText t b where
   -- | Show text in IO
   showTextIO :: MonadIO m => TextBuffer t b -> m String

   -- | Pure show text
   showText   :: BufferToList b => TextBuffer t b -> String

-- | Instance for ASCII text
--
-- >>> :set -XOverloadedLists
-- >>> let b = [72,69,76,76,79] :: BufferI
-- >>> showText (TextBuffer @ASCII b)
-- "HELLO"
--
instance ShowText 'ASCII (Buffer mut pin gc) where
   showTextIO (TextBuffer b) = do
      bs <- bufferToListIO b
      return (bs ||> fromIntegral ||> chr)

   showText (TextBuffer b) =
      bufferToList b ||> fromIntegral ||> chr

-- | Support ASCII text with OverloadedStrings
--
-- >>> :set -XOverloadedStrings
-- >>> let t = "HELLO" :: TextI 'ASCII
-- >>> showText t
-- "HELLO"
--
instance (IsList b, Item b ~ Word8) => IsString (TextBuffer 'ASCII b) where
   fromString s = TextBuffer (fromList (fmap (fromIntegral . ord) s))
