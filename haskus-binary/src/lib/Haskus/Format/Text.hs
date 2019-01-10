{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Haskus.Format.Text
   ( TextBuffer (..)
   , TextI
   , TextB
   , ShowText (..)
   )
where

import Data.Char
import Data.String
import GHC.Exts (IsList(..))

import Haskus.Format.Binary.Word
import Haskus.Data.Buffer
import Haskus.Utils.Flow

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