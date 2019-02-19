{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.FileEmbed
import qualified Data.ByteString as B

main :: IO ()
main = do
   let b = $(embedFile "bench/data.bin")
   print (B.length b)
