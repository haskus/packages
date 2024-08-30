{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Haskus.Calculus.Concept.Var
   ( VarF (..)
   , pattern Var
   )
where

import Haskus.Data.Variant.EADT
import Haskus.Data.Variant.EADT.TH

import Haskus.Calculus.PrettyPrint
import Haskus.Calculus.FreeVars
import Haskus.Calculus.ReplaceVar

import Data.Set as Set


-- | Variable
data VarF n e = VarF n deriving (Functor)

$(eadtPattern 'VarF "Var")

instance Show n => PrettyPrintF (VarF n) where
   prettyPrintF (VarF n) = (False,show n)

instance FreeVarsF n (VarF n) where
   freeVarsF (VarF n) = Set.singleton n

instance {-# OVERLAPPING #-}
   (Eq n
   ) => ReplaceVarF n fs (VarF n)
   where
      replaceVarF n e v@(VarF n')
         | n == n'   = Right e
         | otherwise = Left v
