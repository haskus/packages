{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | ASCII character map and encoding
module Haskus.Text.ASCII
   ( ASCII (..)
   )
where

import Data.Char
import Data.String
import GHC.Exts (IsList(..))

import Haskus.Text
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
-- >>> let b = [72,69,76,76,79] :: Buffer
-- >>> showTextIO (Text b :: Text ASCII)
-- "HELLO"
--
instance ShowText ASCII where
   showTextIO (Text b) = do
      bs <- bufferToList b
      return (bs ||> fromIntegral ||> chr)

-- | Support ASCII text with OverloadedStrings
--
-- >>> :set -XOverloadedStrings
-- >>> let t = "HELLO" :: Text ASCII
-- >>> showTextIO t
-- "HELLO"
--
-- >>> let badt = "José" :: Text ASCII
-- >>> showTextIO badt
-- *** Exception: Invalid ASCII character: é
--
instance IsString (Text ASCII) where
   fromString s = Text (fromList (fmap (toWord8 . ord) s))
      where
         toWord8 x
            | x >= 128  = errorWithoutStackTrace ("Invalid ASCII character: " ++ [chr x])
            | otherwise = fromIntegral x
