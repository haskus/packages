{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}

module Main where


import Haskus.Utils.Variant
import Haskus.Utils.ContFlow

import Criterion
import Criterion.Main (defaultMain)
import Test.QuickCheck
import Control.Monad


main :: IO ()
main = do
   let
      evalAt n = do
         !tree1 <- generate (resize n (arbitrary :: Gen (Node Int)))
         let !tree2 = nodeToStrictNode tree1
         let !tree3 = nodeToVariantNode tree1
         return 
            [ bgroup ("Tree Eval at size=" ++ show n)
               [ bench "ADT"                      $ whnf evalNode tree1
               , bench "Strict ADT"               $ whnf evalStrictNode tree2
               , bench "Variant ADT - V"          $ whnf evalVariantNode tree3
               , bench "Variant ADT - Safe match" $ whnf evalVariantNodeSafe tree3
               ]
            ]

   evals <- forM [10] evalAt

   defaultMain (mconcat evals)




data Plus  a    = Plus a a
data Minus a    = Minus a a
newtype Value a = Value a

instance Arbitrary a => Arbitrary (Plus a) where
   arbitrary = Plus <$> arbitrary <*> arbitrary
instance Arbitrary a => Arbitrary (Minus a) where
   arbitrary = Minus <$> arbitrary <*> arbitrary
instance Arbitrary a => Arbitrary (Value a) where
   arbitrary = Value <$> arbitrary


data Node a
   = NValue (Value a)
   | NPlus  (Plus (Node a))
   | NMinus (Minus (Node a))

instance Arbitrary a => Arbitrary (Node a) where
   arbitrary = do
      n <- getSize
      if n == 0
         then (NValue <$> arbitrary)
         else oneof
            [ resize (n-1) (NPlus  <$> arbitrary)
            , resize (n-1) (NMinus <$> arbitrary)
            ]

evalNode :: Num a => Node a -> a
evalNode (NValue (Value v))   = v
evalNode (NPlus (Plus a b))   = evalNode a + evalNode b
evalNode (NMinus (Minus a b)) = evalNode a - evalNode b

data StrictNode a
   = SNValue                !(Value a)
   | SNPlus  {-# UNPACK #-} !(Plus (StrictNode a))
   | SNMinus {-# UNPACK #-} !(Minus (StrictNode a))

evalStrictNode :: Num a => StrictNode a -> a
evalStrictNode (SNValue (Value v))   = v
evalStrictNode (SNPlus (Plus a b))   = evalStrictNode a + evalStrictNode b
evalStrictNode (SNMinus (Minus a b)) = evalStrictNode a - evalStrictNode b

nodeToStrictNode :: Node a -> StrictNode a
nodeToStrictNode !(NValue a)           = SNValue a
nodeToStrictNode !(NPlus (Plus a b))   = SNPlus (Plus (nodeToStrictNode a) (nodeToStrictNode b))
nodeToStrictNode !(NMinus (Minus a b)) = SNMinus (Minus (nodeToStrictNode a) (nodeToStrictNode b))


newtype VariantNode a = VariantNode (V '[Value a, Plus (VariantNode a), Minus (VariantNode a)])

evalVariantNode :: Num a => VariantNode a -> a
evalVariantNode (VariantNode (V (Value v)))   = v
evalVariantNode (VariantNode (V (Plus a b)))  = evalVariantNode a + evalVariantNode b
evalVariantNode (VariantNode (V (Minus a b))) = evalVariantNode a - evalVariantNode b
evalVariantNode _                             = undefined

evalVariantNodeSafe :: Num a => VariantNode a -> a
evalVariantNodeSafe (VariantNode v) = toCont v >::>
   ( \(Value x)   -> x
   , \(Plus a b)  -> evalVariantNodeSafe a + evalVariantNodeSafe b
   , \(Minus a b) -> evalVariantNodeSafe a - evalVariantNodeSafe b
   )

nodeToVariantNode :: Node a -> VariantNode a
nodeToVariantNode !(NValue a)           = VariantNode (toVariantAt @0 a)
nodeToVariantNode !(NPlus (Plus a b))   = VariantNode (toVariantAt @1 (Plus (nodeToVariantNode a) (nodeToVariantNode b)))
nodeToVariantNode !(NMinus (Minus a b)) = VariantNode (toVariantAt @2 (Minus (nodeToVariantNode a) (nodeToVariantNode b)))
