-- | Embed data into the executable binary
module Haskus.Utils.Embed
   ( embedBytes
   , qCabalProjectPath
   , qRelativeToCabalProjectPath
   , quoteRelativeFile
   -- | Raw text quasiquoter
   , raw
   , rawQ
   )
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (qLocation, addDependentFile)
import Data.Word
import System.Directory (getDirectoryContents, canonicalizePath)
import System.FilePath (isRelative, takeDirectory, takeExtension, (</>))

-- | Embed bytes in a C array, return an Addr#
embedBytes :: [Word8] -> Q Exp
embedBytes bs = pure $ LitE (StringPrimL bs)


-- | Get cabal project path
--
-- This use useful with Template Haskell to read files using paths relative to
-- the project top-level directory. Otherwise, if the current working directory
-- is used, it may vary depending on where `cabal build` is called (e.g. in the
-- `cabal.project` directory or in one of the subidrs).
qCabalProjectPath :: Q FilePath
qCabalProjectPath = do
  loc <- qLocation
  runIO $ do
    let
      go :: FilePath -> IO (Maybe FilePath)
      go p = do
          let dir = takeDirectory p -- move up one level
          if dir == p
            then pure Nothing -- reached the root directory without finding .cabal file
            else do
              contents <- getDirectoryContents dir
              if any ((==) ".cabal" . takeExtension) contents
                then pure (Just dir)
                else go dir
    -- make path absolute and "simpler" (remove indirections, etc.)
    abs_path <- canonicalizePath (loc_filename loc)
    go abs_path >>= \case
      Nothing  -> error "qCabalProjectPath: couldn't find `.cabal` file"
      Just dir -> pure dir

-- | Convert a FilePath to cabal project path into an absolute path
qRelativeToCabalProjectPath :: FilePath -> Q FilePath
qRelativeToCabalProjectPath fp =
  if isRelative fp
    then do
      p <- qCabalProjectPath
      runIO $ canonicalizePath (p </> fp)
    else pure fp

-- | 'quoteRelativeFile' takes a 'QuasiQuoter' and lifts it into one that read
-- the data out of a file.  For example, suppose @asmq@ is an
-- assembly-language quoter, so that you can write [asmq| ld r1, r2 |]
-- as an expression. Then if you define @asmq_f = quoteRelativeFile asmq@, then
-- the quote [asmq_f|foo.s|] will take input from file @"foo.s"@ instead
-- of the inline text
--
-- The difference with 'quoteFile' is that the file is looked up relative to the
-- project directory containing the ".cabal" file (using 'qCabalProjectPath').
quoteRelativeFile :: QuasiQuoter -> QuasiQuoter
quoteRelativeFile q = QuasiQuoter
  { quoteExp  = get (quoteExp q)
  , quotePat  = get (quotePat q)
  , quoteType = get (quoteType q)
  , quoteDec  = get (quoteDec q)
  }
  where
   get :: (String -> Q a) -> String -> Q a
   get old_quoter fp = do
    file_name <- qRelativeToCabalProjectPath fp
    file_cts <- runIO (readFile file_name)
    addDependentFile file_name
    old_quoter file_cts


----------------------------------------------------------------------
-- Raw text quasiquoter (adapted from raw-strings-qq package (BSD3)
----------------------------------------------------------------------


-- |
--
-- A quasiquoter for raw string literals - that is, string literals that don't
-- recognise the standard escape sequences (such as @\'\\n\'@). Basically, they
-- make your code more readable by freeing you from the responsibility to escape
-- backslashes. They are useful when working with regular expressions, DOS/Windows
-- paths and markup languages (such as XML).
--
-- Don't forget the @LANGUAGE QuasiQuotes@ pragma if you're using this
-- module in your code.
--
-- Usage:
--
-- > :seti -XQuasiQuotes
-- > import Haskus.Utils.Embed
-- > let s = [raw|\\w+\@[a-zA-Z_]+?\\.[a-zA-Z]{2,3}|]
-- > s
-- \"\\\\w+\@[a-zA-Z_]+?\\\\.[a-zA-Z]{2,3}\"
-- > [raw|C:\\Windows\\SYSTEM|] ++ [raw|\\user32.dll|]
-- \"C:\\\\Windows\\\\SYSTEM\\\\user32.dll\"
--
-- Multiline raw string literals are also supported:
--
-- @
--     multiline :: String
--     multiline = [raw|\<HTML\>
--     \<HEAD\>
--     \<TITLE\>Auto-generated html formated source\</TITLE\>
--     \<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=windows-1252\"\>
--     \</HEAD\>
--     \<BODY LINK=\"#0000ff\" VLINK=\"#800080\" BGCOLOR=\"#ffffff\"\>
--     \<P\> \</P\>
--     \<PRE\>|]
-- @
--
-- Caveat: since the @\"|]\"@ character sequence is used to terminate the
-- quasiquotation, you can't use it inside the raw string literal. Use 'rawQ' if you
-- want to embed that character sequence inside the raw string.
raw :: QuasiQuoter
raw = QuasiQuoter {
    quoteExp  = return . LitE . StringL . normaliseNewlines,

    quotePat  = \_ -> fail "illegal raw string QuasiQuote (allowed as expression only, used as a pattern)",
    quoteType = \_ -> fail "illegal raw string QuasiQuote (allowed as expression only, used as a type)",
    quoteDec  = \_ -> fail "illegal raw string QuasiQuote (allowed as expression only, used as a declaration)"
}

-- | A variant of 'raw' that interprets the @\"|~]\"@ sequence as @\"|]\"@,
-- @\"|~~]\"@ as @\"|~]\"@ and, in general, @\"|~^n]\"@ as @\"|~^(n-1)]\"@
-- for n >= 1.
--
-- Usage:
--
--
-- > [rawQ||~]|~]|]
-- \"|]|]\"
--
-- > [rawQ||~~]|]
-- \"|~]\"
--
-- > [rawQ||~~~~]|]
-- \"|~~~]\"
--
rawQ :: QuasiQuoter
rawQ = QuasiQuoter {
    quoteExp  = return . LitE . StringL . escape_rQ . normaliseNewlines,

    quotePat  = \_ -> fail "illegal raw string QuasiQuote (allowed as expression only, used as a pattern)",
    quoteType = \_ -> fail "illegal raw string QuasiQuote (allowed as expression only, used as a type)",
    quoteDec  = \_ -> fail "illegal raw string QuasiQuote (allowed as expression only, used as a declaration)"
}

escape_rQ :: String -> String
escape_rQ [] = []
escape_rQ ('|':'~':xs) =
  let (tildas, rest) = span (== '~') xs
  in case rest of
    []       -> '|':'~':tildas
    (']':rs) -> '|':tildas ++ ']':escape_rQ rs
    rs       -> '|':'~':tildas ++ escape_rQ rs
escape_rQ (x:xs) = x : escape_rQ xs

-- See https://github.com/23Skidoo/raw-strings-qq/issues/1 and
-- https://ghc.haskell.org/trac/ghc/ticket/11215.
normaliseNewlines :: String -> String
normaliseNewlines []             = []
normaliseNewlines ('\r':'\n':cs) = '\n':normaliseNewlines cs
normaliseNewlines (c:cs)         = c:normaliseNewlines cs
