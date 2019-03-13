module Haskus.Calculus.Samples where

import Haskus.Calculus.Concept.App
import Haskus.Calculus.Concept.Var
import Haskus.Calculus.Concept.Lambda
import Haskus.Calculus.Concept.Literal
import Haskus.Calculus.Lang.LC

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> :set -XScopedTypeVariables
-- >>> import Haskus.Calculus.FreeVars

sampleLC1 :: LC String
sampleLC1 = Var "inc" `App` Var "1"

sampleLC2 :: LC String
sampleLC2 = Var "+" `App` Var "1" `App` Var "10"

-- | Untyped lambda calculus - Sample 3
--
-- >>> freeVars @String sampleLC3
-- fromList ["*","+","1","10","inc"]
--
sampleLC3 :: LC String
sampleLC3 = Var "*" `App` sampleLC2 `App` sampleLC1

-- | Untyped lambda calculus - Sample 4
--
-- >>> freeVars @String sampleLC4
-- fromList ["*","y"]
--
sampleLC4 :: LC String
sampleLC4 = Lambda "x" $
   Var "*" `App` Var "x" `App` Var "y"


sampleLCL1 :: LCL String
sampleLCL1 = Var "+" `App` LitInt 1 `App` LitInt 10

