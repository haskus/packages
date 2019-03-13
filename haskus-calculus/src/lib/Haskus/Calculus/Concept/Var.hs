{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Haskus.Calculus.Concept.Var
   ( VarF (..)
   , pattern Var
   )
where

import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH

import Haskus.Calculus.PrettyPrint
import Haskus.Calculus.FreeVars

import Data.Set as Set


-- | Variable
data VarF n e = VarF n deriving (Functor)

$(eadtPattern 'VarF "Var")

instance Show n => PrettyPrintF (VarF n) where
   prettyPrintF (VarF n) = (False,show n)

instance FreeVarsF n (VarF n) where
   freeVarsF (VarF n) = Set.singleton n
