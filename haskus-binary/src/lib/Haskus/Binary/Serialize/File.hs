{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Haskus.Binary.Serialize.File
   ( FileGetState (..)
   , FileGetT (..)
   , runFileGet
   , runFilePathGet
   )
where

import Haskus.Binary.Serialize.Get
import Haskus.Binary.Storable
import Haskus.Memory.Buffer
import Haskus.Utils.Monad
import Haskus.Utils.Maybe

import GHC.Exts (Ptr (..))
import System.IO
import Control.Monad.Trans.State.Strict as S
import Control.Monad.Fix

#if !MIN_VERSION_GLASGOW_HASKELL (8,8,0,0)
import Control.Monad.Fail
#endif

-- | FileGetT state
data FileGetState = FileGetState
   { fileGetHandle :: !Handle
   }

-- | A Get monad over a File
newtype FileGetT m a
   = FileGetT (StateT FileGetState m a)
   deriving newtype (Functor, Applicative, Monad, MonadFail, MonadFix, MonadIO)

-- | Get file handle
getHandle :: Monad m => FileGetT m (Handle)
getHandle = FileGetT (gets fileGetHandle)

-- | Helper to get some things
getSomething :: forall a m.
   ( MonadIO m
   ) => Word -> (Ptr a -> IO a) -> FileGetT m a
getSomething sz act = do
   hdl <- getHandle
   liftIO $ allocaBytes sz \p -> do
      -- FIXME: handle EOF
      _n <- hGetBuf hdl p (fromIntegral sz)
      act p


instance (MonadIO m) => GetMonad (FileGetT m) where
      getSkipBytes n = do
         hdl <- getHandle
         liftIO $ hSeek hdl RelativeSeek (fromIntegral n)

      getWord8       = getSomething 1 peek
      getWord16      = getSomething 2 peek
      getWord32      = getSomething 4 peek
      getWord64      = getSomething 8 peek

      getBufferInto sz dest mdoff = getSomething sz \(Ptr addr) -> do
         let b = BufferE addr sz
         copyBuffer b 0 dest (fromMaybe 0 mdoff) sz


-- | Run a getter on a file
runFileGet :: Handle -> FileGetT IO a -> IO a
runFileGet hdl (FileGetT s) = do
   (a,_s') <- runStateT s (FileGetState hdl)
   return a

-- | Run a getter on a file
runFilePathGet :: FilePath -> FileGetT IO a -> IO a
runFilePathGet path s = withBinaryFile path ReadMode (\hdl -> runFileGet hdl s)
