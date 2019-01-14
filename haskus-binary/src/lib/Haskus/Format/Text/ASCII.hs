{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | ASCII character map and encoding
module Haskus.Format.Text.ASCII
   ( ASCII (..)
   )
where

import Data.Char
import Data.String
import GHC.Exts (IsList(..))

import Haskus.Format.Binary.Word
import Haskus.Format.Text
import Haskus.Memory.Buffer
import Haskus.Utils.Flow

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies

-- | ASCII character map and encoding
data ASCII = ASCII

-- | Instance for ASCII text
--
-- >>> :set -XOverloadedLists
-- >>> let b = [72,69,76,76,79] :: BufferI
-- >>> showText (TextBuffer @ASCII b)
-- "HELLO"
--
instance ShowText ASCII (Buffer mut pin gc heap) where
   showTextIO (TextBuffer b) = do
      bs <- bufferToListIO b
      return (bs ||> fromIntegral ||> chr)

   showText (TextBuffer b) =
      bufferToList b ||> fromIntegral ||> chr

-- | Support ASCII text with OverloadedStrings
--
-- >>> :set -XOverloadedStrings
-- >>> let t = "HELLO" :: TextI ASCII
-- >>> showText t
-- "HELLO"
--
-- >>> let badt = "José" :: TextI ASCII
-- >>> showText badt
-- "*** Exception: Invalid ASCII character: é
--
instance (IsList b, Item b ~ Word8) => IsString (TextBuffer ASCII b) where
   fromString s = TextBuffer (fromList (fmap (toWord8 . ord) s))
      where
         toWord8 x
            | x >= 128  = errorWithoutStackTrace ("Invalid ASCII character: " ++ [chr x])
            | otherwise = fromIntegral x
