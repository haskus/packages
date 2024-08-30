{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Haskus.Calculus.PrettyPrint
   ( PrettyPrintF (..)
   , prettyPrint
   , withParen
   )
where

import Haskus.Utils.Types
import Haskus.Data.Variant.EADT

withParen :: Bool -> String -> String
withParen True s  = "("++s++")"
withParen False s = s

class PrettyPrintF (f :: Type -> Type) where
   -- | Bool to indicate if parentheses are required
   prettyPrintF :: f (Bool,String) -> (Bool,String)

-- | Pretty print an expression
prettyPrint :: forall t xs.
   ( Base t ~ VariantF xs
   , Recursive t
   , BottomUpF PrettyPrintF xs
   ) => t -> String
prettyPrint x = snd (bottomUp (toBottomUp @PrettyPrintF prettyPrintF) x)
