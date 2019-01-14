-- | Embed buffers into the program
module Haskus.Memory.Embed
   ( embedBytes
   )
where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Haskus.Memory.Buffer
import Haskus.Format.Binary.Word

-- | Embed bytes at compile time.
-- Return an immutable external buffer (BufferE)
--
-- >>> :set -XTemplateHaskell
-- >>> let b = $$(embedBytes [72,69,76,76,79])
-- >>> bufferSize b
-- 5
embedBytes :: [Word8] -> Q (TExp BufferE)
embedBytes bs = do
   Just bufE <- lookupValueName "BufferE"
   Just w    <- lookupValueName "W#"
   return $ TExp $ ConE bufE
      `AppE` LitE (StringPrimL bs)
      `AppE` (ConE w `AppE` LitE (WordPrimL (fromIntegral (length bs))))
