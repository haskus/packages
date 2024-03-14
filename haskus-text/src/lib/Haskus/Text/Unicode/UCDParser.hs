{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

-- | Unicode character database parsers
module Haskus.Text.Unicode.UCDParser
   ( parseCodePointValue
   , parseCodePoint
   , parseCodePointRange
   , parseCodePointValueOrRange
   , parseCommentLine
   , skipCommentLines
   , parseFile
   , stripComments
   -- * File specific
   , parseBlocks
   , parseDerivedName
   )
where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath

import Haskus.Text.Unicode.CodePoint
import Haskus.Utils.Flow

-- $setup
-- >>> import Text.Megaparsec


type Parser = Parsec () String

----------------------------------------------------------------
-- Common
----------------------------------------------------------------

-- | Parse a code-point value without the "U+" prefix
--
-- >>> runParser parseCodePointValue "" "1234"
-- Right U+1234
--
parseCodePointValue :: Parser CodePoint
parseCodePointValue = do
   v <- L.hexadecimal
   let c = CodePoint v
   when (v > 0x10FFFF) $ do
      error ("Parsed invalid CodePoint: " ++ show c)
   return c

-- | Parse a range of code-points separated by ".."
--
-- >>> runParser parseCodePointRange "" "1234..5678"
-- Right U+1234..U+5678
--
parseCodePointRange :: Parser CodePointRange
parseCodePointRange = do
   r1 <- parseCodePointValue
   void <| string ".."
   r2 <- parseCodePointValue
   return (CodePointRange r1 r2)

-- | Parse either a range of code-points or a single code-point
--
-- >>> runParser parseCodePointValueOrRange "" "1234..5678"
-- Right (Right U+1234..U+5678)
--
-- >>> runParser parseCodePointValueOrRange "" "1234"
-- Right (Left U+1234)
--
parseCodePointValueOrRange :: Parser (Either CodePoint CodePointRange)
parseCodePointValueOrRange = do
   (Right <$> try parseCodePointRange)
      <|> (Left <$> parseCodePointValue)

-- | Parse a code-point value with the "U+" prefix
--
-- >>> runParser parseCodePoint "" "U+1234"
-- Right U+1234
--
parseCodePoint :: Parser CodePoint
parseCodePoint = do
   void <| string "U+"
   parseCodePointValue

-- | Parse a comment line ("^# ...<eol>")
--
-- >>> runParser parseCommentLine "" "# comment"
-- Right " comment"
--
parseCommentLine :: Parser String
parseCommentLine = do
   void <| string "#"
   anySingle `manyTill` (void eol <|> try eof)

-- | Parse valid line with the given parser, skipping comment lines
skipCommentLines :: Parser a -> Parser [a]
skipCommentLines p = do
   skipMany (eol <|> parseCommentLine)
   atEnd >>= \case
      True  -> return []
      False -> do
         x  <- p
         xs <- skipCommentLines p
         return (x:xs)

-- | Parse a file and lift the result into a TH expression
parseFile :: Lift a => FilePath -> Parser a -> ExpQ
parseFile fp' p = do
   -- small hack because "stack build" and "stack repl" in the multi-package
   -- project have different CWD
   fp <- liftIO (doesFileExist fp') >>= \case
            True  -> return fp'
            False -> return ("haskus-text" </> fp')
   addDependentFile fp
   str <- liftIO (readFile fp)
   case runParser p fp str of
      Right e   -> [| e |]
      Left err  -> fail (show err)

-- | Strip comments
stripComments :: Parser [String]
stripComments = skipCommentLines (anySingle `manyTill` eol)

----------------------------------------------------------------
-- File specific
----------------------------------------------------------------

-- | Parse Blocks.txt file
parseBlocks :: Parser [(CodePointRange,String)]
parseBlocks = skipCommentLines parseLine
   where
      parseLine = do
         r <- parseCodePointRange
         void <| string "; "
         n <- anySingle `manyTill` eol
         return (r,n)

-- | Parse DerivedName.txt file
parseDerivedName :: Parser [(Either CodePoint CodePointRange,String)]
parseDerivedName = skipCommentLines parseLine
   where
      parseLine = do
         e <- parseCodePointValueOrRange
         space
         void <| string "; "
         n <- anySingle `manyTill` (void eol <|> try eof)
         return (e,n)
