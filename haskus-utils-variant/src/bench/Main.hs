{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Main where


import Haskus.Utils.Variant
import Haskus.Utils.ContFlow

import Criterion
import Criterion.Main (defaultMain)
import Test.QuickCheck
import Control.DeepSeq
import GHC.Generics


main :: IO ()
main = do
   let
      evalEnv n = do
         !tree1 <- generate (resize n (arbitrary :: Gen (Node Int)))
         let !tree2 = nodeToVariantNode tree1
         return  (n,tree1,tree2)

      evalTest ~(n,tree1,tree2) = bgroup ("Tree Eval at size=" ++ show n)
         [ bench "ADT"                      $ whnf evalNode tree1
         , bench "Variant ADT - V"          $ whnf evalVariantNode tree2
         , bench "Variant ADT - Safe match" $ whnf evalVariantNodeSafe tree2
         ]


   defaultMain
      [ env (evalEnv 10) evalTest
      ]




data Node a
   = NValue a
   | NPlus  (Node a) (Node a)
   | NMinus (Node a) (Node a)
   deriving (Generic,NFData)

instance Arbitrary a => Arbitrary (Node a) where
   arbitrary = do
      n <- getSize
      if n == 0
         then (NValue <$> arbitrary)
         else oneof
            [ resize (n-1) (NPlus  <$> arbitrary <*> arbitrary)
            , resize (n-1) (NMinus <$> arbitrary <*> arbitrary)
            ]

evalNode :: Num a => Node a -> a
evalNode (NValue v)   = v
evalNode (NPlus a b)  = evalNode a + evalNode b
evalNode (NMinus a b) = evalNode a - evalNode b



data Plus a     = Plus a a  deriving (Generic,NFData)
data Minus a    = Minus a a deriving (Generic,NFData)
newtype Value a = Value a   deriving newtype (NFData)

newtype VariantNode a
   = VariantNode (V '[Value a, Plus (VariantNode a), Minus (VariantNode a)])

deriving newtype instance (NFData a) => NFData (VariantNode a)

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
nodeToVariantNode !(NValue a)   = VariantNode (toVariantAt @0 (Value a))
nodeToVariantNode !(NPlus a b)  = VariantNode (toVariantAt @1 (Plus (nodeToVariantNode a) (nodeToVariantNode b)))
nodeToVariantNode !(NMinus a b) = VariantNode (toVariantAt @2 (Minus (nodeToVariantNode a) (nodeToVariantNode b)))
