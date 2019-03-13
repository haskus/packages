{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}

-- | Untyped lambda-calculus
module Haskus.Calculus.Lang.LC
   ( LC
   , LCL
   )
where

import Haskus.Utils.EADT

import Haskus.Calculus.Concept.App
import Haskus.Calculus.Concept.Var
import Haskus.Calculus.Concept.Lambda
import Haskus.Calculus.Concept.Literal

-- | Untyped lambda-calculus
type LC n = EADT '[VarF n, LambdaF n, AppF]

-- | Untyped lambda-calculus with literals
type LCL n = EADT '[VarF n, LambdaF n, AppF, LiteralF]
