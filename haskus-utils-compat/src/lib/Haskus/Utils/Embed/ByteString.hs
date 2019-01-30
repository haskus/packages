{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- | Embed files as ByteStrings into an executable
module Haskus.Utils.Embed.ByteString
   ( bufferToByteString
   , embedByteString
   , embedFile
   , embedOneFileOf
   , embedDir
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
import qualified Haskus.Memory.Embed as E
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
embedFile :: FilePath -> Q Exp
embedFile fp = do
   qAddDependentFile fp
   bs <- runIO $ BS.readFile fp
   embedByteString bs

-- | Embed a single existing file in your source code
-- out of list a list of paths supplied.
--
-- > import qualified Data.ByteString
-- >
-- > myFile :: Data.ByteString.ByteString
-- > myFile = $(embedOneFileOf [ "dirName/fileName", "src/dirName/fileName" ])
embedOneFileOf :: [FilePath] -> Q Exp
embedOneFileOf ps =
  (runIO $ readExistingFile ps) >>= \(path, content) -> do
    qAddDependentFile path
    embedByteString content
  where
    readExistingFile :: [FilePath] -> IO (FilePath, BS.ByteString)
    readExistingFile xs = do
      ys <- filterM doesFileExist xs
      case ys of
        (p:_) -> BS.readFile p >>= \ c -> return (p, c)
        _     -> error "Cannot find file to embed as resource"



-- | Embed a directory recursively in your source code.
--
-- > import qualified Data.ByteString
-- >
-- > myDir :: [(FilePath, Data.ByteString.ByteString)]
-- > myDir = $(embedDir "dirName")
embedDir :: FilePath -> Q Exp
embedDir fp = do
    typ <- [t| [(FilePath, BS.ByteString)] |]
    e <- ListE <$> ((runIO $ fileList fp) >>= mapM (pairToExp fp))
    return $ SigE e typ

-- | Embed a file. Return an expression (FilePath, ByteString)
pairToExp :: FilePath -> (FilePath, BS.ByteString) -> Q Exp
pairToExp root (path, bs) = do
    qAddDependentFile $ root ++ '/' : path
    exp' <- embedByteString bs
    return $! TupE [LitE $ StringL path, exp']

-- | Embed a ByteString into an executable
embedByteString :: BS.ByteString -> Q Exp
embedByteString bs = do
   bufToBs <- [| bufferToByteString |]
   -- make an input BufferE from the ByteString
   buf <- runIO $ BS.unsafeUseAsCStringLen bs $ \(Ptr addr, sz) -> do
            return (BufferE addr (fromIntegral sz))
   -- embed it
   outBuf <- E.embedBuffer buf False Nothing Nothing Nothing
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
   
fileList :: FilePath -> IO [(FilePath, BS.ByteString)]
fileList top = fileList' top ""

fileList' :: FilePath -> FilePath -> IO [(FilePath, BS.ByteString)]
fileList' realTop top = do
    allContents <- filter notHidden <$> getDirectoryContents (realTop </> top)
    let all' = map ((top </>) &&& (\x -> realTop </> top </> x)) allContents
    files <- filterM (doesFileExist . snd) all' >>=
             mapM (liftPair2 . second BS.readFile)
    dirs <- filterM (doesDirectoryExist . snd) all' >>=
            mapM (fileList' realTop . fst)
    return $ concat $ files : dirs

liftPair2 :: Monad m => (a, m b) -> m (a, b)
liftPair2 (a, b) = b >>= \b' -> return (a, b')

notHidden :: FilePath -> Bool
notHidden ('.':_) = False
notHidden _       = True
