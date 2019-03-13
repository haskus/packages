{-# LANGUAGE PatternSynonyms #-}

module Haskus.Calculus.Lang.LC
   ( LC
   , sample1
   , sample2
   , sample3
   )
where

import Haskus.Utils.EADT

import Haskus.Calculus.App
import Haskus.Calculus.Var
import Haskus.Calculus.Lambda

-- | Untyped lambda-calculus
type LC n = EADT '[VarF n, LambdaF n, AppF]

sample1 :: LC String
sample1 = App (Var "inc") (Var "1")

sample2 :: LC String
sample2 = (Var "+") `App` (Var "1") `App` (Var "10")

sample3 :: LC String
sample3 = (Var "*") `App` sample2 `App` sample1
