{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Haskus.Calculus.Concept.Var
   ( VarF (..)
   , pattern Var
   )
where

import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH

import Haskus.Calculus.PrettyPrint

-- | Variable
data VarF n e = VarF n deriving (Functor)

$(eadtPattern 'VarF "Var")

instance Show n => PrettyPrintF (VarF n) where
   prettyPrintF (VarF n) = (False,show n)
