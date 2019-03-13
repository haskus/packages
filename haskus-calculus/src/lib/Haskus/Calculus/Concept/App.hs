{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Haskus.Calculus.Concept.App
   ( AppF (..)
   , pattern App
   )
where

import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH

import Haskus.Calculus.PrettyPrint


-- | Application
data AppF e = AppF e e deriving (Functor)

$(eadtPattern 'AppF "App")

instance PrettyPrintF AppF where
   prettyPrintF (AppF (b1,e1) (b2,e2)) = (True,withParen b1 e1 ++ " "++ withParen b2 e2)
