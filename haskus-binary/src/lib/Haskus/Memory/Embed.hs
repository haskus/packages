{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Embed buffers into the program
module Haskus.Memory.Embed
   ( embedBytes
   , embedFile
   , embedBuffer
   -- * Internals
   , embedPinnedBuffer
   , embedUnpinnedBuffer
   , loadSymbol
   , loadMutableSymbol
   , toBufferE
   , toBufferE'
   , toBufferME
   , toBufferME'
   , makeEmbeddingFile
   , EmbedEntry (..)
   , SectionType (..)
   )
where

import Haskus.Memory.Buffer
import Haskus.Number.Word
import Haskus.Utils.List (intersperse)
import Haskus.Utils.Maybe
import Haskus.Utils.Monad

import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory (getFileSize)
import GHC.Exts
import System.IO

-- | Embed bytes at compile time using GHC's literal strings.
--
-- >>> :set -XTemplateHaskell
-- >>> let b = $$(embedBytes [72,69,76,76,79])
-- >>> bufferSize b
-- 5
embedBytes :: [Word8] -> Q (TExp BufferE)
embedBytes bs = do
   bufE <- fromMaybe (error "Please import Haskus.Memory.Embed") <$> lookupValueName "toBufferE'"
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
   bufE <- fromMaybe (error "Please import Haskus.Memory.Embed") <$> lookupValueName "toBufferE"
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
   bufE <- fromMaybe (error "Please import Haskus.Memory.Embed") <$> lookupValueName "toBufferME"
   ptrTy <- [t| Ptr () |]
   addTopDecls
      [ ForeignD $ ImportF CCall unsafe ("&"++sym) nam ptrTy
      ]
   return $ VarE bufE
      `AppE` VarE nam
      `AppE` LitE (WordPrimL (fromIntegral sz))


toBufferE :: Ptr () -> Word# -> BufferE
{-# INLINABLE toBufferE #-}
toBufferE (Ptr x) sz = BufferE x (W# sz)

toBufferE' :: Addr# -> Word# -> BufferE
{-# INLINABLE toBufferE' #-}
toBufferE' x sz = BufferE x (W# sz)

toBufferME :: Ptr () -> Word# -> BufferME
{-# INLINABLE toBufferME #-}
toBufferME (Ptr x) sz = BufferME x (W# sz)

toBufferME' :: Addr# -> Word# -> BufferME
{-# INLINABLE toBufferME' #-}
toBufferME' x sz = BufferME x (W# sz)


-- | Section type
data SectionType
   = ReadOnlySection       -- ^ Read-only
   | WriteableSection      -- ^ Writable
   | UninitializedSection  -- ^ Uninitialized
   deriving (Show,Eq,Ord)

-- | An embedding entry. Used to embed binary files into an executable
data EmbedEntry = EmbedEntry
   { embedEntryType       :: SectionType  -- ^ Type of data access
   , embedEntryAlignement :: Word         -- ^ Alignement to respect
   , embedEntrySymbol     :: String       -- ^ Symbol to associate to the data
   , embedEntryFilePath   :: FilePath     -- ^ Input file path
   , embedEntryOffset     :: Maybe Word   -- ^ Offset in the input file
   , embedEntrySize       :: Maybe Word   -- ^ Size limit in the input file
   }
   deriving (Show,Eq,Ord)

-- | Create a GAS entry to include a binary file
makeEmbedEntry :: EmbedEntry -> String
makeEmbedEntry EmbedEntry{..} =
   mconcat $ intersperse "\n" $
      [ ".section " ++ case embedEntryType of
         ReadOnlySection      -> "\".rodata\""
         WriteableSection     -> "\".data\""
         UninitializedSection -> "\".bss\""
      , ".align " ++ show embedEntryAlignement
      , ".global \"" ++ embedEntrySymbol ++ "\""
      , embedEntrySymbol ++ ":"
      , ".incbin \"" ++ embedEntryFilePath ++ "\""
                     ++ (case embedEntryOffset of
                           Just offset -> ","++show offset
                           Nothing     -> ",0")
                     ++ (case embedEntrySize of
                            Just size -> ","++show size
                            Nothing   -> mempty)
      , "\n"
      ]


-- | Create an assembler file for the given embedding entries
makeEmbeddingFile :: FilePath -> [EmbedEntry] -> IO ()
makeEmbeddingFile path entries = do
   let e = concatMap makeEmbedEntry entries
   -- TODO: remove this when we will generate an ASM file directly
   -- (cf GHC #16180)
   let escape v = case v of
         ('"':xs)  -> "\\\"" ++ escape xs
         ('\\':xs) -> "\\\\" ++ escape xs
         ('\n':xs) -> "\\n" ++ escape xs
         x:xs      -> x : escape xs
         []        -> []
   let e' = ("asm(\""++escape e++"\");")
   writeFile path e'

-- | Embed a file in the executable. Return a BufferE
embedFile
   :: FilePath    -- ^ File to embed
   -> Bool        -- ^ Mutable buffer or not
   -> Maybe Word  -- ^ Alignment
   -> Maybe Word  -- ^ Offset in the file in bytes
   -> Maybe Word  -- ^ Size to include in bytes (otherwise up to the end of the file)
   -> Q Exp       -- ^ BufferE or BufferME depending on mutability
embedFile = embedFile' False


-- | Embed a file in the executable. Return a BufferE or a BufferME depending on
-- the mutability parameter.
--
-- `nodep` parameter is used to indicate if we want to add a dependency on the
-- input file (e.g. we don't want to do this for temporary files TH generated).
--
embedFile' :: Bool -> FilePath -> Bool -> Maybe Word -> Maybe Word -> Maybe Word -> Q Exp
embedFile' nodep path mutable malign moffset msize = do
   nam <- newName "buffer"
   let sym = show nam ++ "_data"
   let entry = EmbedEntry
         { embedEntryType       = if mutable
                                    then WriteableSection
                                    else ReadOnlySection
         , embedEntryAlignement = fromMaybe 1 malign
         , embedEntrySymbol     = sym
         , embedEntryFilePath   = path
         , embedEntryOffset     = moffset
         , embedEntrySize       = msize
         }
   sfile <- addTempFile ".c" -- TODO: use .s when LangASM is implemented
   liftIO (makeEmbeddingFile sfile [entry])

   sz <- case msize of
            Just x  -> return x
            Nothing -> fromIntegral <$> liftIO (getFileSize path)

   when (not nodep) $
      addDependentFile path

   -- TODO: use LangASM when implemented (cf GHC #16180)
   addForeignFilePath LangC sfile

   if mutable
      then loadMutableSymbol sz sym
      else loadSymbol        sz sym


-- | Embed a pinned buffer in the executable. Return either a BufferE or a
-- BufferME.
embedPinnedBuffer
   :: Buffer mut 'Pinned fin heap -- ^ Source buffer
   -> Bool        -- ^ Should the embedded buffer be mutable
   -> Maybe Word  -- ^ Alignement
   -> Maybe Word  -- ^ Offset in the buffer
   -> Maybe Word  -- ^ Number of Word8 to write
   -> Q Exp       -- ^ BufferE or BufferME, depending on mutability parameter
embedPinnedBuffer buf mut malign moffset msize = do
   tmp <- qAddTempFile ".dat"
   bsz <- bufferSizeIO buf
   let off = fromMaybe 0 moffset
   let sz  = fromMaybe bsz msize
   when (off+sz > bsz) $
      fail "Invalid buffer offset/size combination"

   liftIO $ unsafeWithBufferPtr buf $ \ptr -> do
      withBinaryFile tmp WriteMode $ \hdl -> do
         hPutBuf hdl (ptr `plusPtr` fromIntegral off) (fromIntegral sz)
   embedFile' True tmp mut malign Nothing Nothing

-- | Embed a unpinned buffer in the executable. Return either a BufferE or a
-- BufferME.
embedUnpinnedBuffer
   :: Buffer mut 'NotPinned fin heap -- ^ Source buffer
   -> Bool        -- ^ Should the embedded buffer be mutable
   -> Maybe Word  -- ^ Alignement
   -> Maybe Word  -- ^ Offset in the buffer
   -> Maybe Word  -- ^ Number of Word8 to write
   -> Q Exp       -- ^ BufferE or BufferME, depending on mutability parameter
embedUnpinnedBuffer buf mut malign moffset msize = do
   bsz <- liftIO (bufferSizeIO buf)
   let sz  = fromMaybe bsz msize
   let off = fromMaybe 0 moffset
   b <- newPinnedBuffer sz
   liftIO (copyBuffer buf off b 0 sz)
   embedPinnedBuffer b mut malign Nothing Nothing

-- | Embed a buffer in the executable. Return either a BufferE or a BufferME.
embedBuffer
   :: Buffer mut pin fin heap -- ^ Source buffer
   -> Bool       -- ^ Should the embedded buffer be mutable or not
   -> Maybe Word -- ^ Optional alignement constraint
   -> Maybe Word -- ^ Optional offset in the source buffer
   -> Maybe Word -- ^ Optional number of bytes to include
   -> Q Exp      -- ^ BufferE or BufferME, depending on mutability parameter
embedBuffer b =
   -- Some buffers with 'NotPinned are in fact pinned by GHC as an optimization.
   -- We detect this with `bufferDynamicallyPinned` and we avoid the copy in
   -- these cases.
   case bufferDynamicallyPinned b of
      Left ub  -> embedUnpinnedBuffer ub
      Right pb -> embedPinnedBuffer pb
