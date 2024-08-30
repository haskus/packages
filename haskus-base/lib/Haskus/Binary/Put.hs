-- | Put monad
--
-- FIXME: PutM uses slow ByteString builder... We need to replace it with a
-- fast one
module Haskus.Binary.Put
   ( Put
   , PutM
   , runPut
   , runPutM
   -- * Put
   , putBuffer
   , putByteString
   , putPadding
   , putPaddingAlign
   , putWord8
   , putWord16le
   , putWord16be
   , putWord32le
   , putWord32be
   , putWord64le
   , putWord64be
   )
where

import qualified Data.ByteString as BS
import qualified Data.Serialize.Put as BP
import Data.Serialize.Put (Put,PutM)
import Data.Bifunctor

import Haskus.Utils.Flow (replicateM_)
import Haskus.Binary.Buffer
import Haskus.Number.Word

-- | Execute Put
runPut :: Put -> Buffer
runPut = Buffer . BP.runPut

-- | Execute PutM
runPutM :: PutM a -> (a,Buffer)
runPutM = second Buffer . BP.runPutM


-- | Put a buffer
putBuffer :: Buffer -> Put
putBuffer (Buffer bs) = BP.putByteString bs

-- | Put a ByteString
putByteString :: BS.ByteString -> Put
putByteString = BP.putByteString

-- | Put null bytes
putPadding :: Word -> Put
putPadding n = replicateM_ (fromIntegral n) (BP.putWord8 0x00)

-- | Put null bytes to align the given value to the second
putPaddingAlign :: Word -> Word -> Put
putPaddingAlign n al = putPadding n'
   where
      n' = case n `mod` al of
               0 -> 0
               x -> al - fromIntegral x

-- | Put a Word8
putWord8 :: Word8 -> Put
putWord8 = BP.putWord8

-- | Put a Word16 little-endian
putWord16le :: Word16 -> Put
putWord16le = BP.putWord16le

-- | Put a Word16 big-endian
putWord16be :: Word16 -> Put
putWord16be = BP.putWord16be

-- | Put a Word32 little-endian
putWord32le :: Word32 -> Put
putWord32le = BP.putWord32le

-- | Put a Word32 big-endian
putWord32be :: Word32 -> Put
putWord32be = BP.putWord32be

-- | Put a Word64 little-endian
putWord64le :: Word64 -> Put
putWord64le = BP.putWord64le

-- | Put a Word64 big-endian
putWord64be :: Word64 -> Put
putWord64be = BP.putWord64be
