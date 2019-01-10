{-# LANGUAGE TemplateHaskell #-}

-- | Unicode character database parsers
module Haskus.Format.Text.Unicode.UCDParser
   ( parseCodePointValue
   , parseCodePoint
   , parseCommentLine
   , skipCommentLines
   , parseFile
   , parseBlocks
   )
where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Haskus.Format.Text.Unicode
import Haskus.Utils.Flow

type Parser = Parsec () String

----------------------------------------------------------------
-- Common
----------------------------------------------------------------

-- | Parse a code-point value without the "U+" prefix
parseCodePointValue :: Parser CodePoint
parseCodePointValue = do
   v <- L.hexadecimal
   let c = CodePoint v
   when (v > 0x10FFFF) $ do
      error ("Parsed invalid CodePoint: " ++ show c)
   return c

-- | Parse a code-point value with the "U+" prefix
parseCodePoint :: Parser CodePoint
parseCodePoint = do
   void <| string "U+"
   parseCodePointValue

-- | Parse a comment line ("^# ...<eol>")
parseCommentLine :: Parser String
parseCommentLine = do
   void <| string "#"
   anySingle `manyTill` eol

-- | Parse valid line with the given parser, skipping comment lines
skipCommentLines :: Parser a -> Parser [a]
skipCommentLines p = do
   mr <- skipManyTill
            (eol <|> parseCommentLine)
            (fmap Just p <|> fmap (const Nothing) eof)
   case mr of
      Just r  -> fmap (r:) (skipCommentLines p)
      Nothing -> return []

-- | Parse a file and lift the result into a TH expression
parseFile :: Lift a => FilePath -> Parser a -> ExpQ
parseFile fp p = do
   addDependentFile fp
   str <- liftIO (readFile fp)
   case runParser p fp str of
      Right e   -> [| e |]
      Left err  -> fail (show err)

----------------------------------------------------------------
-- Blocks.txt parser
----------------------------------------------------------------

-- | Parse Blocks.txt file
parseBlocks :: Parser [(CodePoint,CodePoint,String)]
parseBlocks = skipCommentLines parseBlockLine
   where
      parseBlockLine = do
         r1 <- parseCodePointValue
         void <| string ".."
         r2 <- parseCodePointValue
         void <| string "; "
         n <- anySingle `manyTill` eol
         return (r1,r2,n)
