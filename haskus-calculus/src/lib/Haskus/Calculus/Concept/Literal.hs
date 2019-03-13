{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Haskus.Calculus.Concept.Literal
   ( LiteralF (..)
   , pattern LitInt
   , pattern LitString
   )
where

import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH

import Haskus.Calculus.PrettyPrint
import Haskus.Calculus.FreeVars

import Data.Set as Set

-- | Literal
data LiteralF e
   = LitIntF Integer
   | LitStringF String
   deriving (Functor)

$(eadtPattern 'LitIntF    "LitInt")
$(eadtPattern 'LitStringF "LitString")

instance PrettyPrintF LiteralF where
   prettyPrintF (LitIntF n)    = (False,show n)
   prettyPrintF (LitStringF n) = (False,show n)

instance FreeVarsF n LiteralF where
   freeVarsF _ = Set.empty
