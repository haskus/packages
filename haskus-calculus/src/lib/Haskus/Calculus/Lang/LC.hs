{-# LANGUAGE PatternSynonyms #-}

-- | Untyped lambda-calculus
module Haskus.Calculus.Lang.LC
   ( LC
   , sampleLC1
   , sampleLC2
   , sampleLC3
   , LCL
   , sampleLCL1
   )
where

import Haskus.Utils.EADT

import Haskus.Calculus.Concept.App
import Haskus.Calculus.Concept.Var
import Haskus.Calculus.Concept.Lambda
import Haskus.Calculus.Concept.Literal

-- | Untyped lambda-calculus
type LC n = EADT '[VarF n, LambdaF n, AppF]

sampleLC1 :: LC String
sampleLC1 = Var "inc" `App` Var "1"

sampleLC2 :: LC String
sampleLC2 = Var "+" `App` Var "1" `App` Var "10"

sampleLC3 :: LC String
sampleLC3 = Var "*" `App` sampleLC2 `App` sampleLC1

-- | Untyped lambda-calculus with literals
type LCL n = EADT '[VarF n, LambdaF n, AppF, LiteralF]

sampleLCL1 :: LCL String
sampleLCL1 = Var "+" `App` LitInt 1 `App` LitInt 10

