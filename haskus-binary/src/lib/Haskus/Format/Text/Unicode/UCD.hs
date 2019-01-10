{-# LANGUAGE TemplateHaskell #-}

-- | Unicode character database
module Haskus.Format.Text.Unicode.UCD
   ( blocks
   )
where

import Haskus.Format.Text.Unicode.UCDParser
import Haskus.Format.Text.Unicode

-- | Plane blocks
--
-- >>> blocks !! 1
-- (U+0080,U+00FF,"Latin-1 Supplement")
--
blocks :: [(CodePoint,CodePoint,String)]
blocks = $(parseFile "src/data/ucd/Blocks.txt" parseBlocks)
