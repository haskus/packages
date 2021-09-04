{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Haskus.Text
   ( Text (..)
   , ShowText (..)
   )
where

import Haskus.Memory.Buffer

-- | Text
--
-- The buffer contains a text encoded according to phantom type t
newtype Text (t :: k)
   = Text Buffer

class ShowText (t :: k) where
   -- | Show text in IO
   showTextIO :: Text t -> IO String
