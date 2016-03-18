-- | Manage graphics devices
module ViperVM.System.Graphics
   ( GraphicCard (..)
   , loadGraphicCards
   , MappedBuffer (..)
   , GenericFrame (..)
   , initGenericFrameBuffer
   , freeGenericFrameBuffer
   , graphicCardConnectors
   , graphicCardControllers
   , graphicCardEncoders
   )
where

import ViperVM.System.System
import qualified ViperVM.Format.Binary.BitSet as BitSet
import ViperVM.Arch.Linux.FileSystem.OpenClose
import ViperVM.Arch.Linux.FileDescriptor
import ViperVM.Arch.Linux.FileSystem
import ViperVM.Arch.Linux.FileSystem.ReadWrite
import ViperVM.Arch.Linux.Error
import ViperVM.Arch.Linux.Memory

import ViperVM.Arch.Linux.Graphics.Capability
import ViperVM.Arch.Linux.Graphics.GenericBuffer
import ViperVM.Arch.Linux.Graphics.Internals
import ViperVM.Arch.Linux.Graphics.Connector
import ViperVM.Arch.Linux.Graphics.Controller
import ViperVM.Arch.Linux.Graphics.Encoder
import ViperVM.Arch.Linux.Graphics.Mode
import ViperVM.Arch.Linux.Graphics.FrameBuffer
import ViperVM.Arch.Linux.Graphics.PixelFormat
import ViperVM.Arch.Linux.Graphics.Event as Graphics

import Control.Monad (void,forM,forM_)
import Foreign.Ptr

import Control.Concurrent.STM
import Control.Concurrent
import Data.Foldable (traverse_)
import Control.Monad.Trans.Class (lift)
import Foreign.Marshal (allocaBytes)
import System.Posix.Types (Fd(..))
import Data.List (isPrefixOf)
import System.FilePath (takeBaseName)

-- | Graphic card
data GraphicCard = GraphicCard
   { graphicCardPath    :: FilePath             -- ^ Path to the graphic card in SysFS
   , graphicCardDev     :: Device               -- ^ Device major/minor to create the device file descriptor
   , graphicCardID      :: Int                  -- ^ Card identifier
   , graphicCardHandle  :: Handle               -- ^ Device handle
   , graphicCardChan    :: TChan Graphics.Event -- ^ Event stream
   }


-- | Return detected graphic cards
--
-- Graphic cards are /class/drm/cardN directories in SysFS where N is the card
-- identifier. The this directory, the dev file contains device major/minor to
-- create appropriate device node.
loadGraphicCards :: System -> Sys [GraphicCard]
loadGraphicCards system = sysLogSequence "Load graphic cards" $ do

   devs <- listDevicesWithClass system "drm"
   let
      isCard (p,_) = "card" `isPrefixOf` takeBaseName p
      devs' = filter isCard devs
   forM devs' $ \(devpath,dev) -> do
      hdl   <- getDeviceHandle system CharDevice dev
      -- We support these capabilities
      setClientCapability hdl ClientCapStereo3D        True
      setClientCapability hdl ClientCapUniversalPlanes True
      setClientCapability hdl ClientCapAtomic          True
      -- Create the DRM event reader thread
      GraphicCard devpath dev (read (drop 4 devpath)) hdl
         <$> newEventWaiterThread hdl


-- | Create a new thread reading input events and putting them in a TChan
newEventWaiterThread :: FileDescriptor -> Sys (TChan Graphics.Event)
newEventWaiterThread fd@(FileDescriptor lowfd) = do
   let
      bufsz = 1000 -- buffer size
      rfd = Fd (fromIntegral lowfd)

   ch <- lift $ newBroadcastTChanIO
   void $ lift $ forkIO $ allocaBytes bufsz $ \ptr -> do
      let go = do
            threadWaitRead rfd
            r <- sysRead fd ptr (fromIntegral bufsz)
            case r of
               -- FIXME: we should somehow signal that an error occured and
               -- that we won't report future events (if any)
               Left _  -> return ()
               Right sz2 -> do
                  evs <- peekEvents ptr (fromIntegral sz2)
                  atomically $ traverse_ (writeTChan ch) evs
                  go
      go
   return ch


data MappedBuffer = MappedBuffer
   { mappedBufferBuffer  :: GenericBuffer
   , mappedBufferMapping :: GenericBufferMap
   , mappedBufferPointer :: Ptr ()
   , mappedBufferInfo    :: Buffer
   }

data GenericFrame = GenericFrame
   { genericFrameBuffer  :: FrameBuffer
   , genericFrameBuffers :: [MappedBuffer]
   }

-- | Allocate and map fullscreen planes for the given format and mode
initGenericFrameBuffer :: Handle -> Mode -> PixelFormat -> Sys GenericFrame
initGenericFrameBuffer hdl mode pixfmt@(PixelFormat fmt _) = do
   let
      width  = fromIntegral $ modeHorizontalDisplay mode
      height = fromIntegral $ modeVerticalDisplay mode
      bpps   = formatBitDepth fmt
      flags  = 0

   mappedPlanes <- forM bpps $ \bpp -> do
      buf <- sysCallAssert "Create a generic buffer" $
         createGenericBuffer hdl width height bpp flags

      bufKerMap <- sysCallAssert "Map generic buffer" $
         mapGenericBuffer hdl buf

      addr <- sysCallAssert "Map generic buffer in user space" $ 
         sysMemMap Nothing
            (cdSize buf)
            (BitSet.fromList [ProtRead,ProtWrite])
            (BitSet.fromList [MapShared])
            Nothing
            (Just (hdl, mdOffset bufKerMap))

      let plane = Buffer (cdHandle buf) (cdPitch buf) 0 0

      return (MappedBuffer buf bufKerMap addr plane)
   
   let planes = fmap mappedBufferInfo mappedPlanes

   fb <- sysCallAssert "Add frame buffer" $ addFrameBuffer hdl width height pixfmt BitSet.empty planes

   return $ GenericFrame fb mappedPlanes


freeGenericFrameBuffer :: Handle -> GenericFrame -> Sys ()
freeGenericFrameBuffer hdl (GenericFrame fb mappedBufs) = do

   forM_ mappedBufs $ \(MappedBuffer buf _ addr _) -> do
      -- unmap generic buffer from user-space
      sysCallAssert "Unmap generic buffer from user space" $ 
         sysMemUnmap addr (cdSize buf)

      -- destroy the generic buffer
      sysCallAssert "Destroy generic buffer" $ destroyGenericBuffer hdl buf


   -- remove the framebuffer
   sysCallAssert "Remove framebuffer" $ removeFrameBuffer hdl fb


-- | Retreive graphic card connectors
graphicCardConnectors :: GraphicCard -> Sys [Connector]
graphicCardConnectors = sysIO . getConnectors . graphicCardHandle

-- | Retrieve graphic card controllers
graphicCardControllers :: GraphicCard -> Sys [Controller]
graphicCardControllers = sysIO . getControllers . graphicCardHandle

-- | Retrieve graphic card encoders
graphicCardEncoders :: GraphicCard -> Sys [Encoder]
graphicCardEncoders = sysIO . getEncoders . graphicCardHandle
