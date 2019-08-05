{-# LANGUAGE TemplateHaskell #-}

-- | Unicode character database
module Haskus.Text.Unicode.UCD
   ( blocks
   --, names
   )
where

import Haskus.Text.Unicode.UCDParser
import Haskus.Text.Unicode.CodePoint

-- | Plane blocks
--
-- >>> blocks !! 1
-- (U+0080..U+00FF,"Latin-1 Supplement")
--
blocks :: [(CodePointRange,String)]
blocks = $(parseFile "src/data/ucd/Blocks.txt" parseBlocks)

-- | Names
--
-- >> names !! 1
-- (Left U+0021,"EXCLAMATION MARK")
--
-- >> names !! 41630
-- (Right U+2B740..U+2B81D,"CJK UNIFIED IDEOGRAPH-*")
--
--names :: [(Either CodePoint CodePointRange,String)]
--names = $(parseFile "src/data/ucd/extracted/DerivedName.txt" parseDerivedName)
