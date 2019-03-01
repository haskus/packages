{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- | Embed files as ByteStrings into an executable
module Haskus.Utils.Embed.ByteString
   ( bufferToByteString
   , embedBS
   , embedBSFile
   , embedBSFilePrefix
   , embedBSOneFileOf
   , embedBSDir
   , module Haskus.Memory.Embed
   )
where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import GHC.Ptr
import System.IO.Unsafe
import System.Directory
import System.FilePath
import Control.Arrow

import Haskus.Memory.Buffer
import Haskus.Memory.Embed
import Haskus.Utils.Monad

----------------------------------------------------------------------
-- File embedding adapted from file-embed package (BSD3).
--
-- We use Haskus's buffer embedding facilities which are much faster then
-- file-embed's ones as of 2019-01-30.
--
-- See: https://hsyl20.fr/home/posts/2019-01-15-fast-file-embedding-with-ghc.html
----------------------------------------------------------------------

-- | Embed a single file in your source code.
--
-- > import qualified Data.ByteString
-- >
-- > myFile :: Data.ByteString.ByteString
-- > myFile = $(embedFile "dirName/fileName")
embedBSFile :: FilePath -> Q Exp
embedBSFile fp = do
   qAddDependentFile fp
   bs <- runIO $ BS.readFile fp
   embedBS bs

embedBSFilePrefix :: FilePath -> FilePath -> Q Exp
embedBSFilePrefix prefix fp' = do
   -- small hack because "stack build" and "stack repl" in the multi-package
   -- project have different CWD
   fp <- liftIO (doesFileExist fp') >>= \case
            True  -> return fp'
            False -> return (prefix </> fp')
   embedBSFile fp


-- | Embed a single existing file in your source code
-- out of list a list of paths supplied.
--
-- > import qualified Data.ByteString
-- >
-- > myFile :: Data.ByteString.ByteString
-- > myFile = $(embedOneFileOf [ "dirName/fileName", "src/dirName/fileName" ])
embedBSOneFileOf :: [FilePath] -> Q Exp
embedBSOneFileOf ps =
  (runIO $ readExistingFile ps) >>= \(path, content) -> do
    qAddDependentFile path
    embedBS content
  where
    readExistingFile :: [FilePath] -> IO (FilePath, BS.ByteString)
    readExistingFile xs = do
      ys <- filterM doesFileExist xs
      case ys of
        (p:_) -> BS.readFile p >>= \c -> return (p, c)
        _     -> error "Cannot find file to embed as resource"



-- | Embed a directory recursively in your source code.
--
-- > import qualified Data.ByteString
-- >
-- > myDir :: [(FilePath, Data.ByteString.ByteString)]
-- > myDir = $(embedDir "dirName")
embedBSDir :: FilePath -> Q Exp
embedBSDir fp = do
   typ <- [t| [(FilePath, BS.ByteString)] |]
   bufToBs <- [| bufferToByteString |]
   let embedPair (relpath,realpath) = do
         exp' <- embedFile realpath False Nothing Nothing Nothing
         return $! TupE [LitE $ StringL relpath, bufToBs `AppE` exp']
   e <- ListE <$> ((runIO $ listDirectoryRec fp) >>= mapM embedPair)
   return $ SigE e typ

-- | Embed a ByteString into an executable
embedBS :: BS.ByteString -> Q Exp
embedBS bs = do
   bufToBs <- [| bufferToByteString |]
   -- make an input BufferE from the ByteString
   buf <- runIO $ BS.unsafeUseAsCStringLen bs $ \(Ptr addr, sz) -> do
            return (BufferE addr (fromIntegral sz))
   -- embed it
   outBuf <- embedBuffer buf False Nothing Nothing Nothing
   -- keep the ByteString alive up to here
   runIO $ touch bs
   -- return an expression converting the embedded buffer into a ByteString
   return $ bufToBs `AppE` outBuf

-- | Convert an external buffer into a ByteString (O(1))
bufferToByteString :: Buffer mut pin 'NotFinalized 'External -> BS.ByteString
bufferToByteString b = unsafePerformIO $ do 
   let pack addr sz = BS.unsafePackAddressLen (fromIntegral sz) addr
   case b of
      BufferE  addr sz -> pack addr sz
      BufferME addr sz -> pack addr sz

-- | List a directory recursively, only returning non-hidden files.
--
-- Return tuples (relative path, real path)
listDirectoryRec :: FilePath -> IO [(FilePath,FilePath)]
listDirectoryRec realTop = go ""
   where
      notHidden :: FilePath -> Bool
      notHidden ('.':_) = False
      notHidden _       = True

      go top = do
         allContents <- filter notHidden <$> getDirectoryContents (realTop </> top)
         let all' = map ((top </>) &&& (\x -> realTop </> top </> x)) allContents
         files <- filterM (doesFileExist . snd) all'
         dirs  <- filterM (doesDirectoryExist . snd) all' >>= mapM (go . fst)
         return $ concat $ files : dirs

