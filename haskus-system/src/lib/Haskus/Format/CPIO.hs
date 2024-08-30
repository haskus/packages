{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Module implementing the CPIO archive format
--
-- The CPIO archive format collects any number of files, directories, and other
-- file system objects (symbolic links, device nodes, etc.) into a single stream
-- of bytes.
--
-- This format is used by Linux initramfs.
-- See https://www.kernel.org/doc/html/latest/driver-api/early-userspace/buffer-format.html
--
-- We only consider the "newc" and "crc" CPIO formats because the old ones are
-- deprecated and aren't supported as Linux initramfs.
--
module Haskus.Format.CPIO
   ( FileDesc(..)
   , putFile
   , putFiles
   , getFile
   , getFiles
   )
where

import Data.Char (ord,chr)
import GHC.Exts

import Haskus.Number.Word
import Haskus.Binary.Buffer
import Haskus.Binary.Put
import Haskus.Binary.Get
import Haskus.Binary.Bits
import Haskus.Utils.Flow (forM_,when)
import Haskus.Utils.Text (Text,getTextUtf8)
import qualified Haskus.Utils.Text as Text


-- Note [CPIO format]
-- ~~~~~~~~~~~~~~~~~~
-- Format of a CPIO archive as documented in
-- https://www.kernel.org/doc/html/latest/driver-api/early-userspace/buffer-format.html
--
--    *       is used to indicate "0 or more occurrences of"
--    (|)     indicates alternatives
--    +       indicates concatenation
--    ALGN(n) means padding with null bytes to an n-byte boundary
--
--    cpio_archive := cpio_file* + (<nothing> | cpio_trailer)
--    cpio_file    := ALGN(4) + cpio_header + filename + "\0" + ALGN(4) + data
--    cpio_trailer := ALGN(4) + cpio_header + "TRAILER!!!\0" + ALGN(4)
--
-- The cpio "TRAILER!!!" entry (cpio end-of-archive) is optional, but is not
-- ignored; see "Hard links" below.
--
-- Header
-- ~~~~~~
-- All fields contain hexadecimal ASCII numbers fully padded with '0' on the
-- left to the full width of the field, for example, the integer 4780 is
-- represented by the ASCII string "000012ac".
--
-- | Field name   | Field size | Meaning
-- |--------------|------------|--------------------------------
-- | c_magic      | 6 bytes    | The string "070701" or "070702"
-- | c_ino        | 8 bytes    | File inode number
-- | c_mode       | 8 bytes    | File mode and permissions
-- | c_uid        | 8 bytes    | File uid
-- | c_gid        | 8 bytes    | File gid
-- | c_nlink      | 8 bytes    | Number of hard links
-- | c_mtime      | 8 bytes    | Modification time
-- | c_filesize   | 8 bytes    | Size of data field
-- | c_maj        | 8 bytes    | Major part of file device number
-- | c_min        | 8 bytes    | Minor part of file device number
-- | c_rmaj       | 8 bytes    | Major part of device node reference
-- | c_rmin       | 8 bytes    | Minor part of device node reference
-- | c_namesize   | 8 bytes    | Length of filename, including final 0
-- | c_chksum     | 8 bytes    | Checksum of data field if c_magic is 070702; otherwise zero
--
-- The c_mode field matches the contents of st_mode returned by stat(2) on
-- Linux, and encodes the file type and file permissions.
--
-- The c_filesize should be zero for any file which is not a regular file or
-- symlink.
--
-- The c_chksum field contains a simple 32-bit unsigned sum of all the bytes in
-- the data field. cpio(1) refers to this as "crc", which is clearly incorrect
-- (a cyclic redundancy check is a different and significantly stronger
-- integrity check), however, this is the algorithm used.
--
-- If the filename is "TRAILER!!!" this is actually an end-of-archive marker;
-- the c_filesize for an end-of-archive marker must be zero.
--
-- Hard links
-- ~~~~~~~~~~
-- CPIO supports hardlinks by identifying each entry with a tuple (c_ino, c_maj,
-- c_min). If two entries have the same tuple and a number of hardlink > 1
-- (c_nlink > 1), then they are hardlinked. File data doesn't need to be present
-- for every such entries: the last one will overwrite the previous ones.
--
-- The trailer is useful to concatenate two CPIO archives: there will be no
-- hardlinks between files separated by a trailer, even if they have the same
-- tuple.
--
-- Symlinks
-- ~~~~~~~~
-- Filesize mustn't be zero for symlinks.
--


-- | File description
--
-- * fileDevMajor, fileDevMinor, fileDevInode: The device and inode numbers from
-- the disk. These are used by programs that read cpio archives to determine
-- when two entries refer to the same file. Programs that synthesize cpio
-- archives should be careful to set these to distinct values for each entry. 
--
-- * fileRDevMajor, fileRDevMinor: For block special and character special
-- entries, this field contains the associated device number.  For all other
-- entry types, it should be set to zero by writers and ignored by readers.
--
-- * fileModifTime: Modification time of the file, indicated as the number of
-- seconds since the start of the epoch, 00:00:00 UTC January 1, 1970.
data FileDesc = FileDesc
   { fileInode       :: !Word32   -- ^ File inode
   , fileMode        :: !Word32   -- ^ File mode (type and permissions)
   , fileUID         :: !Word32   -- ^ Owner user ID
   , fileGID         :: !Word32   -- ^ Owner group ID
   , fileNLink       :: !Word32   -- ^ Number of hard links to the file
   , fileModifTime   :: !Word32   -- ^ Modification time
   , fileDevMajor    :: !Word32   -- ^ Major part of file device number
   , fileDevMinor    :: !Word32   -- ^ Minor part of file device number
   , fileRDevMajor   :: !Word32   -- ^ Major part of device node reference
   , fileRDevMinor   :: !Word32   -- ^ Minor part of device node reference
   , fileEnableCheck :: !Bool     -- ^ Enable checksum for this entry
   } deriving (Show)

-- | Put a number as a 8 hexadecimal digits string padding left with zeros
putNumber :: Word32 -> Put
putNumber x = do
  let !ascii = "0123456789abcdef"#
      to_ascii x' =
          let !(I# offset) = fromIntegral x' .&. 0xf
          in W8# (indexWord8OffAddr# ascii offset)
  putWord8 $ to_ascii (x `uncheckedShiftR` 28)
  putWord8 $ to_ascii (x `uncheckedShiftR` 24)
  putWord8 $ to_ascii (x `uncheckedShiftR` 20)
  putWord8 $ to_ascii (x `uncheckedShiftR` 16)
  putWord8 $ to_ascii (x `uncheckedShiftR` 12)
  putWord8 $ to_ascii (x `uncheckedShiftR` 8)
  putWord8 $ to_ascii (x `uncheckedShiftR` 4)
  putWord8 $ to_ascii x

-- | Read a number stored as a 8-characters hexadecimal string
getNumber :: Get Word32
getNumber = do
  let parse_one = \case
        0x30 -> 0x0
        0x31 -> 0x1
        0x32 -> 0x2
        0x33 -> 0x3
        0x34 -> 0x4
        0x35 -> 0x5
        0x36 -> 0x6
        0x37 -> 0x7
        0x38 -> 0x8
        0x39 -> 0x9
        0x61 -> 0xA
        0x62 -> 0xB
        0x63 -> 0xC
        0x64 -> 0xD
        0x65 -> 0xE
        0x66 -> 0xF
        0x41 -> 0xA
        0x42 -> 0xB
        0x43 -> 0xC
        0x44 -> 0xD
        0x45 -> 0xE
        0x46 -> 0xF
        c   -> error $ "getNumber: unexpected non-hexadecimal character: " ++ show (chr (fromIntegral c))

      read_all :: Word -> Word32 -> Get Word32
      read_all 0 v = pure v
      read_all n v = do
        c <- getWord8
        read_all (n-1) ((v `uncheckedShiftL` 4) + parse_one c)

  read_all 8 0

-- | Put null bytes to pad to 4
putPad4 :: Word32 -> Put
putPad4 n = putPaddingAlign (fromIntegral n) 4

-- | Skip padding bytes for padding to 4
skipPad4 :: Word32 -> Get ()
skipPad4 n = uncheckedSkipAlign (fromIntegral n) 4

-- | Put a file in the archive
--
-- * path is the path in the archive
putFile :: FileDesc -> Text -> Buffer -> Put
putFile FileDesc {..} path content = do
  -- Write magic number
  mapM_ (putWord8 . fromIntegral . ord) $ if fileEnableCheck then "070701" else ("070702" :: String)
  -- Put file description
  putNumber fileInode
  putNumber fileMode
  putNumber fileUID
  putNumber fileGID
  putNumber fileNLink
  putNumber fileModifTime
  let content_size = fromIntegral (bufferSize content) -- TODO: check that size <= 32-bit
  putNumber content_size
  putNumber fileDevMajor
  putNumber fileDevMinor
  putNumber fileRDevMajor
  putNumber fileRDevMinor
  -- put the length of the UTF8 encoded string, not the Haskell string
  let bspath = Text.textEncodeUtf8 path
      pathsz = fromIntegral (bufferSize bspath + 1) -- TODO: check that size <= 32-bit
  putNumber pathsz

  -- compute and write checksum if necessary
  if not fileEnableCheck
    then putNumber 0
    else putNumber (computeChecksum content)

  -- Put file name
  putBuffer bspath
  putWord8 0x00 -- ending NUL byte
  putPad4 (110 + pathsz)

  -- Put file contents
  putBuffer content
  putPad4 content_size

computeChecksum :: Buffer -> Word32
computeChecksum b = sum (map fromIntegral (bufferUnpackByteList b))

-- | Put trailer entry
putTrailer :: Put
putTrailer = putFile desc "TRAILER!!!" emptyBuffer
  where
    desc = FileDesc 0 0 0 0 0 0 0 0 0 0 False

-- | Put files in archive (with archive ending marker)
putFiles :: [(FileDesc,Text,Buffer)] -> Put
putFiles files = do
   forM_ files $ \(desc,name,bs) -> putFile desc name bs
   putTrailer

-- | Get a file from the archive
getFile :: Get (FileDesc,Text,Buffer)
getFile = do
  -- Read magic number
  enableChecksum <- getTextUtf8 6 >>= \case
   "070701" -> pure False
   "070702" -> pure True
   magic    -> error ("File format not supported (invalid magic number: " ++ show magic ++ ")")

  -- Read file description
  ino  <- getNumber
  mode <- getNumber
  uid  <- getNumber
  gid  <- getNumber
  nlnk <- getNumber
  mtim <- getNumber
  size <- getNumber
  mad  <- getNumber
  mid  <- getNumber
  mard <- getNumber
  mird <- getNumber
  nameLength <- getNumber -- includes NUL byte
  checksum <- getNumber -- read checksum

  -- Read file name
  fileName <- getTextUtf8 (fromIntegral nameLength - 1)
  skip 1 -- skip \0 byte
  skipPad4 (110 + nameLength)

  -- Read content
  content <- getBuffer (fromIntegral size)
  skipPad4 size

  -- Check checksum
  when enableChecksum do
    let c = computeChecksum content
    when (c /= checksum) do
      error $ "Invalid checksum: expected " ++ show checksum ++ ", got " ++ show c

  let fd = FileDesc ino mode uid gid nlnk mtim mad mid mard mird enableChecksum
  return (fd, fileName, content)

-- | Get all the files from the archive
getFiles :: Get [(FileDesc,Text,Buffer)]
getFiles = rec []
  where
    rec xs = do
      x@(_,name,_) <- getFile
      case name of
        -- FIXME: this is wrong. The trailer is optional and we should support
        -- hardlinks, i.e. trailer between entries (see Note [CPIO format]).
        "TRAILER!!!" -> return (reverse xs)
        _            -> rec (x:xs)
