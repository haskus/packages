{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Lambda where

import Haskus.Data.Variant.EADT
import Haskus.Data.Variant.EADT.TH

data AppF   a e = AppF a e e deriving (Functor)
data LamF n a e = LamF a n e deriving (Functor)
data VarF n a e = VarF a n   deriving (Functor)

eadtPattern 'AppF "App"
eadtPattern 'LamF "Lam"
eadtPattern 'VarF "Var"

type Expr n app lam var = EADT '[AppF app, LamF n lam, VarF n var]

type Id = String
type VanillaExpr = Expr Id () () ()
type MyPassExpr  = Expr Id () () Int


class MyPass xs f where
   myPass :: f (EADT xs) -> EADT xs

instance (Foldable n, VarF (n a) Int :<: xs) => MyPass xs (VarF (n a) var) where
   myPass (VarF _ nam) = Var (length nam) nam

instance {-# OVERLAPS #-} f :<: xs => MyPass xs f where
   myPass = VF

annotate
   :: forall xs ys.
   ( BottomUpF (MyPass ys) xs
   ) => EADT xs -> EADT ys
annotate = bottomUp (toBottomUp @(MyPass ys) myPass)


input :: VanillaExpr
input = App () (App () (Var () "myFunction") (Var () "x")) (Var () "mylist")

output :: MyPassExpr
output = annotate input

-- >>> putStrLn (eadtShow input)
-- (("myFunction" "x") "mylist")
--
-- >>> putStrLn (eadtShow output)
-- (("myFunction"{10} "x"{1}) "mylist"{6})

class ShowExt x where
   showExt :: x -> String

instance ShowExt () where
   showExt _ = ""

instance {-# OVERLAPS #-} Show a => ShowExt a where
   showExt x = "{" ++ show x ++ "}"

instance (ShowExt x,Show n) => EADTShow (VarF n x) where
   eadtShow' (VarF x name) = show name ++ showExt x
   
instance (ShowExt x, Show n) => EADTShow (LamF n x) where
   eadtShow' (LamF x name e) = "\\" ++ show name ++ showExt x ++ " => " ++ e

instance ShowExt x => EADTShow (AppF x) where
   eadtShow' (AppF x e1 e2) = "(" ++ e1 ++ " " ++ e2 ++ ")" ++ showExt x
