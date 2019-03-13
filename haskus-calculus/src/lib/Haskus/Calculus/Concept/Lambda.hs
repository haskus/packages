{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Haskus.Calculus.Concept.Lambda
   ( LambdaF (..)
   , pattern Lambda
   )
where

import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH

import Haskus.Calculus.PrettyPrint

-- | Abstraction
data LambdaF n e = LambdaF n e deriving (Functor)

$(eadtPattern 'LambdaF "Lambda")

instance Show n => PrettyPrintF (LambdaF n) where
   prettyPrintF (LambdaF n (b,e)) = (True,"\\"++show n++"."++withParen b e)
