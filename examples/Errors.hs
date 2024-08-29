{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RebindableSyntax #-}

import Haskus.Utils.Variant
import Haskus.Utils.Variant.Syntax

import Prelude hiding (head,lookup,(>>=),(>>),return)
import qualified Prelude
import Text.Read

data HeadError = ListWasEmpty deriving Show

head :: [a] -> V '[a,HeadError]
head []    = toVariantAt @1 ListWasEmpty
head (x:_) = toVariantAt @0 x

data LookupError k = KeyWasNotPresent k deriving Show

lookup :: Eq k => k -> [(k,v)] -> V '[v,LookupError k]
lookup k vs = case Prelude.lookup k vs of
   Just v  -> toVariantAt @0 v
   Nothing -> toVariantAt @1 (KeyWasNotPresent k)

data ParseError = ParseError deriving Show

parse :: String -> V '[Integer,ParseError]
parse s = case readMaybe s of
   Just i  -> V @Integer i
   Nothing -> V ParseError


foo :: String -> V '[Integer, ParseError, LookupError Char, HeadError]
foo str = do
   c <- head str
   r <- lookup c codeMap
   parse (r ++ tail str)

   where
      codeMap :: [(Char, String)]
      codeMap = [ ('x', "0x")
                , ('d', "")
                ]
