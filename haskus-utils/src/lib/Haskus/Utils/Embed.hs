-- | Embed data into the executable binary
module Haskus.Utils.Embed
   ( embedBytes
   , module Data.FileEmbed
   -- | Raw text quasiquoter
   , raw
   , rawQ
   )
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.FileEmbed
import Data.Word

-- | Embed bytes in a C array, return an Addr#
embedBytes :: [Word8] -> Q Exp
embedBytes bs = return $ LitE (StringPrimL bs)



{-| Adapted from the raw-strings-qq package (BSD3)

A quasiquoter for raw string literals - that is, string literals that don't
recognise the standard escape sequences (such as @\'\\n\'@). Basically, they
make your code more readable by freeing you from the responsibility to escape
backslashes. They are useful when working with regular expressions, DOS/Windows
paths and markup languages (such as XML).

Don't forget the @LANGUAGE QuasiQuotes@ pragma if you're using this
module in your code.

Usage:

@
    ghci> :set -XQuasiQuotes
    ghci> import Text.RawString.QQ
    ghci> let s = [raw|\\w+\@[a-zA-Z_]+?\\.[a-zA-Z]{2,3}|]
    ghci> s
    \"\\\\w+\@[a-zA-Z_]+?\\\\.[a-zA-Z]{2,3}\"
    ghci> [raw|C:\\Windows\\SYSTEM|] ++ [raw|\\user32.dll|]
    \"C:\\\\Windows\\\\SYSTEM\\\\user32.dll\"
@

Multiline raw string literals are also supported:

@
    multiline :: String
    multiline = [raw|\<HTML\>
    \<HEAD\>
    \<TITLE\>Auto-generated html formated source\</TITLE\>
    \<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=windows-1252\"\>
    \</HEAD\>
    \<BODY LINK=\"#0000ff\" VLINK=\"#800080\" BGCOLOR=\"#ffffff\"\>
    \<P\> \</P\>
    \<PRE\>|]
@

Caveat: since the @\"|]\"@ character sequence is used to terminate the
quasiquotation, you can't use it inside the raw string literal. Use 'rawQ' if you
want to embed that character sequence inside the raw string.
-}
raw :: QuasiQuoter
raw = QuasiQuoter {
    -- Extracted from dead-simple-json.
    quoteExp  = return . LitE . StringL . normaliseNewlines,

    quotePat  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a pattern)",
    quoteType = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a type)",
    quoteDec  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a declaration)"
}

{-| A variant of 'raw' that interprets the @\"|~]\"@ sequence as @\"|]\"@,
@\"|~~]\"@ as @\"|~]\"@ and, in general, @\"|~^n]\"@ as @\"|~^(n-1)]\"@
for n >= 1.

Usage:

@
    ghci> [rawQ||~]|~]|]
    \"|]|]\"
    ghci> [rawQ||~~]|]
    \"|~]\"
    ghci> [rawQ||~~~~]|]
    \"|~~~]\"
@
-}
rawQ :: QuasiQuoter
rawQ = QuasiQuoter {
    quoteExp  = return . LitE . StringL . escape_rQ . normaliseNewlines,

    quotePat  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a pattern)",
    quoteType = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a type)",
    quoteDec  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a declaration)"
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
