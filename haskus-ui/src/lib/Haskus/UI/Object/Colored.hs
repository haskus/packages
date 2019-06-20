{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Haskus.UI.Object.Colored
   ( ColoredF (..)
   , pattern Colored
   )
where

import Haskus.Utils.Flow
import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH

import Haskus.UI.Object
import Haskus.UI.Common

-- | Add default color to inner object
data ColoredF e = ColoredF Color e deriving (Functor)

eadtPattern 'ColoredF "Colored"

instance Object e => Object (ColoredF e) where
   hit r (ColoredF c o) = hit r o ||> (\x -> x { hitColor = Just c })

