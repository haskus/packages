{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskus.Calculus.Concept.Lambda
   ( LambdaF (..)
   , pattern Lambda
   )
where

import Haskus.Data.Variant.EADT
import Haskus.Data.Variant.EADT.TH

import Haskus.Calculus.PrettyPrint
import Haskus.Calculus.FreeVars
import Haskus.Calculus.ReplaceVar

import Data.Set as Set

-- | Abstraction
data LambdaF n e = LambdaF n e deriving (Functor)

$(eadtPattern 'LambdaF "Lambda")

instance Show n => PrettyPrintF (LambdaF n) where
   prettyPrintF (LambdaF n (b,e)) = (True,"\\"++show n++"."++withParen b e)

instance Ord n => FreeVarsF n (LambdaF n) where
   freeVarsF (LambdaF n s) = Set.delete n s

instance {-# OVERLAPPING #-}
   ( Eq n
   , LambdaF n :<: fs
   ) => ReplaceVarF n fs (LambdaF n)
   where
      replaceVarF n _e v@(LambdaF n' _e')
         -- n is bound into e', we must stop here
         | n == n'   = Right (VF v)
         | otherwise = Left v
