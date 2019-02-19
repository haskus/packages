{-# LANGUAGE TemplateHaskell #-}

module Main where

import Haskus.Memory.Buffer
import Haskus.Memory.Embed

main :: IO ()
main = do
   let b = $(embedFile "bench/data.bin" False Nothing Nothing Nothing)
   print (bufferSize b)
