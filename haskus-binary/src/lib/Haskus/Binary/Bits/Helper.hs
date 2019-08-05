{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Haskus.Binary.Bits.Helper
   ( bitOffset
   , byteOffset
   )
where

import Haskus.Binary.Bits.Shift
import Haskus.Binary.Bits.Mask

-- | Compute bit offset (equivalent to x `mod` 8 but faster)
bitOffset :: Word -> Word
{-# INLINABLE bitOffset #-}
bitOffset n = mask @3 n

-- | Compute byte offset (equivalent to x `div` 8 but faster)
byteOffset :: Word -> Word
{-# INLINABLE byteOffset #-}
byteOffset n = n `uncheckedShiftR` 3
