{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Embed files as ByteStrings into an executable
module Haskus.Utils.Embed.ByteString
   ( bufferToByteString
   , embedBS
   , embedBSFile
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
import GHC.Exts
import GHC.IO (IO(..))
import System.IO.Unsafe
import System.Directory
import System.FilePath
import Control.Arrow

import Haskus.Memory.Buffer
import Haskus.Memory.Embed
import Haskus.Utils.Monad
import Haskus.Utils.Embed

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
embedBSFile rfp = do
  fp <- qRelativeToCabalProjectPath rfp
  qAddDependentFile fp
  bs <- runIO $ BS.readFile fp
  embedBS bs

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
#if __GLASGOW_HASKELL__ >= 810
         return $! TupE [Just (LitE $ StringL relpath), Just (bufToBs `AppE` exp')]
#else
         return $! TupE [LitE $ StringL relpath, bufToBs `AppE` exp']
#endif
   e <- ListE <$> ((runIO $ listDirectoryRec fp) >>= mapM embedPair)
   return $ SigE e typ

-- | Embed a ByteString into an executable
embedBS :: BS.ByteString -> Q Exp
embedBS bs = do
   bufToBs <- [| bufferToByteString |]
   -- make an input Buffer from the ByteString
   buf <- runIO $ BS.unsafeUseAsCStringLen bs $ \(Ptr addr, I# sz) -> do
            return (attachExternalBuffer addr (int2Word# sz))
   -- embed it
   outBuf <- embedBuffer buf False Nothing Nothing Nothing
   -- keep the ByteString alive up to here
   runIO $ IO \s -> case touch# bs s of s' -> (# s', () #)
   -- return an expression converting the embedded buffer into a ByteString
   return $ bufToBs `AppE` outBuf

-- | Convert an external buffer into a ByteString (O(1))
bufferToByteString :: Buffer -> BS.ByteString
bufferToByteString b = unsafePerformIO $ do 
  case b of
    InBuffer {}                        -> error "bufferToByteString: not an external buffer"
    OutBuffer _addr _sz (Finalizers _) -> error "bufferToByteString: unsupported finalized buffer"
    OutBuffer addr sz NoFinalizers     -> BS.unsafePackAddressLen (I# (word2Int# sz)) addr

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

