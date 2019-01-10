{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Haskus.Format.Text
   ( TextFormat (..)
   , TextBuffer (..)
   , TextI
   , TextB
   , ShowText (..)
   )
where

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
-- >>> let badt = "José" :: TextI 'ASCII
-- >>> showText badt
-- "*** Exception: Invalid ASCII character: é
--
instance (IsList b, Item b ~ Word8) => IsString (TextBuffer 'ASCII b) where
   fromString s = TextBuffer (fromList (fmap (toWord8 . ord) s))
      where
         toWord8 x
            | x >= 128  = errorWithoutStackTrace ("Invalid ASCII character: " ++ [chr x])
            | otherwise = fromIntegral x
