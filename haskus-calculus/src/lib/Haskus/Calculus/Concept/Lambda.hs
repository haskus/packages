{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Haskus.Calculus.Concept.Lambda
   ( LambdaF (..)
   , pattern Lambda
   )
where

import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH

import Haskus.Calculus.PrettyPrint
import Haskus.Calculus.FreeVars

import Data.Set as Set

-- | Abstraction
data LambdaF n e = LambdaF n e deriving (Functor)

$(eadtPattern 'LambdaF "Lambda")

instance Show n => PrettyPrintF (LambdaF n) where
   prettyPrintF (LambdaF n (b,e)) = (True,"\\"++show n++"."++withParen b e)

instance Ord n => FreeVarsF n (LambdaF n) where
   freeVarsF (LambdaF n s) = Set.delete n s
