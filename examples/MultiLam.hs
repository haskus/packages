{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module MultiLam where

import Haskus.Data.Variant.EADT
import Haskus.Data.Variant.EADT.TH
import Haskus.Utils.ContFlow

import qualified Data.List as List


data SingleLamF r = SingleLamF String r  deriving Functor
data MultiLamF r  = MultiLamF [String] r deriving Functor
data AppF r       = AppF r r             deriving Functor
data VarF r       = VarF String          deriving Functor

eadtPattern 'SingleLamF "SingleLam"
eadtPattern 'MultiLamF  "MultiLam"
eadtPattern 'AppF       "App"
eadtPattern 'VarF       "Var"

type Expr1 = EADT '[SingleLamF, AppF, VarF]
type Expr2 = EADT '[MultiLamF, AppF, VarF]

oneToTwo :: Expr1 -> Expr2
oneToTwo e = e >:>
  ( \lam        -> let (xs,r) = binders (VF lam)
                   in MultiLam xs (oneToTwo r)
  , \(AppF a b) -> App (oneToTwo a) (oneToTwo b)
  , \(VarF s)   -> Var s
  )
-- Note that ontToTwo uses what I've dubbed "safe pattern-matching" in the
-- documentation. See https://docs.haskus.org/eadt/safe_pattern_matching.html
--
-- As we only modify a single constructor, it would be better (but more verbose)
-- to use a generic approach that works for any Expr1/Expr2 as long as Expr1 has
-- a SingleLamF constructor and Expr2 a MultiLamF constructor.
-- This is documented in https://docs.haskus.org/eadt/constructor_removal.html

binders :: Expr1 -> ([String],Expr1)
binders = go []
  where
    go :: [String] -> Expr1 -> ([String],Expr1)
    go xs e = e >:>
      ( \(SingleLamF x r) -> go (x:xs) r
      , \v                -> (reverse xs, VF v)
      , \v                -> (reverse xs, VF v)
      )


---------------
-- example

instance EADTShow AppF where
  eadtShow' (AppF e1 e2) = "(" ++ e1 ++ " " ++ e2 ++ ")"

instance EADTShow VarF where
  eadtShow' (VarF s) = s

instance EADTShow SingleLamF where
  eadtShow' (SingleLamF b e) = "(\\" ++ b ++ ". " ++ e ++ ")"

instance EADTShow MultiLamF where
  eadtShow' (MultiLamF bs e) = "(\\" ++ concat (List.intersperse " " bs) ++ ". " ++ e ++ ")"

expr1 :: Expr1
expr1 = SingleLam "a" $ SingleLam "b"  $ SingleLam "c" $ App (Var "a") $ App (Var "b") (Var "c")

expr2 :: Expr2
expr2 = oneToTwo expr1

main :: IO ()
main = do
  putStrLn (eadtShow expr1)
  putStrLn (eadtShow expr2)

  -- > main
  -- (\a. (\b. (\c. (a (b c)))))
  -- (\a b c. (a (b c)))
