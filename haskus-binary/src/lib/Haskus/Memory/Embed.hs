{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

-- | Embed buffers into the program
module Haskus.Memory.Embed
   ( embedBytes
   , loadSymbol
   , loadMutableSymbol
   -- * Internals
   , toBufferE
   , toBufferE'
   , toBufferME
   , toBufferME'
   )
where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Haskus.Memory.Buffer
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Ptr
import GHC.Exts

-- | Embed bytes at compile time using GHC's literal strings.
--
-- >>> :set -XTemplateHaskell
-- >>> let b = $$(embedBytes [72,69,76,76,79])
-- >>> bufferSize b
-- 5
embedBytes :: [Word8] -> Q (TExp BufferE)
embedBytes bs = do
   Just bufE <- lookupValueName "toBufferE'"
   return $ TExp $ VarE bufE
      `AppE` LitE (StringPrimL bs)
      `AppE` LitE (WordPrimL (fromIntegral (length bs)))

-- | Load a buffer from a symbol. Return a BufferE
--
-- Note: we can't use Typed TH because of #13587
--
-- >> -- Test.c
-- >> const char mydata[9] = {1,2,30,40,50,6,7,8,9};
--
-- >> let b = $(loadSymbol 9 "mydata")
-- >> print (fmap (bufferReadWord8 b) [0..8])
-- [1,2,30,40,50,6,7,8,9]
--
loadSymbol :: Word -> String -> Q Exp
loadSymbol sz sym = do
   nam <- newName sym
   Just bufE <- lookupValueName "toBufferE"
   ptrTy <- [t| Ptr () |]
   addTopDecls
      [ ForeignD $ ImportF CCall unsafe ("&"++sym) nam ptrTy
      ]
   return $ VarE bufE
      `AppE` VarE nam
      `AppE` LitE (WordPrimL (fromIntegral sz))

-- | Load a buffer from a symbol. Return a BufferME
--
-- Note: we can't use Typed TH because of #13587
--
-- >> -- Test.c
-- >> const char mydata[9] = {1,2,30,40,50,6,7,8,9};
-- >> char mywrtdata[9]    = {1,2,30,40,50,6,7,8,9};
--
-- >> let w = $(loadMutableSymbol 9 "mywrtdata")
-- >> forM_ [0..8] (\i -> bufferWriteWord8IO w i (fromIntegral i))
-- >> print =<< forM [0..8] (bufferReadWord8IO w)
-- [0,1,2,3,4,5,6,7,8]
--
-- Trying to write into constant memory:
-- >> let err = $(loadMutableSymbol 9 "mydata")
-- >> bufferWriteWordIO err 0 10
-- SEGFAULT
--
loadMutableSymbol :: Word -> String -> Q Exp
loadMutableSymbol sz sym = do
   nam <- newName sym
   Just bufE <- lookupValueName "toBufferME"
   ptrTy <- [t| Ptr () |]
   addTopDecls
      [ ForeignD $ ImportF CCall unsafe ("&"++sym) nam ptrTy
      ]
   return $ VarE bufE
      `AppE` VarE nam
      `AppE` LitE (WordPrimL (fromIntegral sz))


toBufferE :: Ptr () -> Word# -> BufferE
{-# INLINE toBufferE #-}
toBufferE (Ptr x) sz = BufferE x (W# sz)

toBufferE' :: Addr# -> Word# -> BufferE
{-# INLINE toBufferE' #-}
toBufferE' x sz = BufferE x (W# sz)

toBufferME :: Ptr () -> Word# -> BufferME
{-# INLINE toBufferME #-}
toBufferME (Ptr x) sz = BufferME x (W# sz)

toBufferME' :: Addr# -> Word# -> BufferME
{-# INLINE toBufferME' #-}
toBufferME' x sz = BufferME x (W# sz)
